# Download and stitch NLS (National Library of Scotland) OS town plan tiles for Leeds.
#
# This script is intentionally standalone (no package dependency) and mirrors the
# logic in `README.qmd`.
#
# Usage:
#   Rscript scripts/download_leeds_nls_tiles.R

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(glue)
})

# ---- Configuration (edit and re-run) -----------------------------------------

# Hardcode n_tiles for iterative runs:
n_tiles <- 10000L  # set to 10, 20, 100, then 10000

# NLS tiles are served as XYZ tiles.
base_url <- "https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North"

# Zoom level for the Leeds town plan tiles.
# NOTE: If the case-study area results in >> 10k tiles, lower z.
z <- 19L

# Output (GeoTIFF) and on-disk caches (NOT committed)
cache_dir_png <- file.path(".cache", "nls_tiles_png")
cache_dir_chunks <- file.path(".cache", "nls_tiles_chunks")
out_tif <- "tiles_merged_leeds.tif"

# ---- Helpers (ported from README.qmd) ----------------------------------------

tile_coords_to_xy <- function(lon, lat, z, scheme = c("xyz", "tms")) {
  scheme <- match.arg(scheme)
  stopifnot(is.finite(lon), is.finite(lat), is.finite(z))

  n <- 2^z

  # Web Mercator latitude limit (prevents Inf from tan/log near the poles)
  lat <- max(min(lat, 85.05112878), -85.05112878)
  lat_rad <- lat * pi / 180

  x_tile <- floor((lon + 180) / 360 * n)
  y_tile <- floor((1 - log(tan(lat_rad) + 1 / cos(lat_rad)) / pi) / 2 * n)

  # Clamp to valid range [0, n - 1]
  x_tile <- max(min(x_tile, n - 1), 0)
  y_tile <- max(min(y_tile, n - 1), 0)

  if (scheme == "tms") {
    y_tile <- (n - 1) - y_tile
  }

  c(x = x_tile, y = y_tile)
}

tile_xy_to_lonlat <- function(x, y, z, scheme = c("xyz", "tms"), offset = 0.5) {
  scheme <- match.arg(scheme)
  stopifnot(is.finite(x), is.finite(y), is.finite(z), is.finite(offset))

  n <- 2^z
  if (scheme == "tms") y <- (n - 1) - y

  lon <- ((x + offset) / n) * 360 - 180
  lat_rad <- atan(sinh(pi * (1 - 2 * (y + offset) / n)))
  lat <- lat_rad * 180 / pi

  c(lon = lon, lat = lat)
}

tile_webmerc_extent <- function(z, x, y) {
  max_ext <- 20037508.34
  tile_size <- (2 * max_ext) / (2^z)

  xmin <- -max_ext + (x * tile_size)
  xmax <- -max_ext + ((x + 1) * tile_size)
  ymax <- max_ext - (y * tile_size)
  ymin <- max_ext - ((y + 1) * tile_size)

  terra::ext(xmin, xmax, ymin, ymax)
}

tile_rast_from_png <- function(png_file, z, x, y) {
  r <- suppressWarnings(terra::rast(png_file))

  # NLS tiles are paletted PNGs (single band + color table). Convert to RGB so
  # plotRGB()/writeRaster preserve real colours.
  if (terra::nlyr(r) == 1 && terra::has.colors(r)) {
    ct <- terra::coltab(r)[[1]]
    rR <- terra::subst(r, ct$value, ct$red)
    rG <- terra::subst(r, ct$value, ct$green)
    rB <- terra::subst(r, ct$value, ct$blue)
    r <- c(rR, rG, rB)
    terra::RGB(r) <- 1:3
  }

  # Web tiles are stored with a top-left origin; flip so north is up.
  r <- terra::flip(r, direction = "vertical")

  terra::ext(r) <- tile_webmerc_extent(z = z, x = x, y = y)
  terra::crs(r) <- "EPSG:3857"
  r
}

tile_png_url <- function(base_url, z, x, y) {
  glue("{base_url}/{z}/{x}/{y}.png")
}

download_tile_png <- function(url, destfile) {
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(destfile) && file.info(destfile)$size > 0) {
    return(invisible(TRUE))
  }

  ok <- FALSE
  try({
    # Use httr if available (nice HTTP status handling); fall back to base R.
    if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::GET(url, httr::write_disk(destfile, overwrite = TRUE))
      ok <- !httr::http_error(resp)
    } else {
      utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
      ok <- file.exists(destfile) && file.info(destfile)$size > 0
    }
  }, silent = TRUE)

  if (!ok) {
    if (file.exists(destfile)) unlink(destfile)
  }

  invisible(ok)
}

tiles_for_polygon <- function(poly, z) {
  stopifnot(inherits(poly, "sf") || inherits(poly, "sfc"))

  poly_3857 <- sf::st_transform(poly, 3857)
  bb <- sf::st_bbox(poly_3857)

  max_ext <- 20037508.34
  tile_size <- (2 * max_ext) / (2^z)

  x_min <- floor((bb["xmin"] + max_ext) / tile_size)
  x_max <- ceiling((bb["xmax"] + max_ext) / tile_size)
  y_min <- floor((max_ext - bb["ymax"]) / tile_size)
  y_max <- ceiling((max_ext - bb["ymin"]) / tile_size)

  grid <- expand.grid(
    x = seq.int(x_min, x_max),
    y = seq.int(y_min, y_max)
  )

  # Filter to tiles whose WebMercator footprint intersects the polygon.
  # (This keeps tile count down vs bbox-only.)
  tile_polys <- lapply(seq_len(nrow(grid)), function(i) {
    e <- tile_webmerc_extent(z = z, x = grid$x[i], y = grid$y[i])
    sf::st_polygon(list(
      matrix(
        c(
          e$xmin, e$ymin,
          e$xmin, e$ymax,
          e$xmax, e$ymax,
          e$xmax, e$ymin,
          e$xmin, e$ymin
        ),
        ncol = 2,
        byrow = TRUE
      )
    ))
  })
  sfc <- sf::st_sfc(tile_polys, crs = 3857)
  keep <- sf::st_intersects(sfc, sf::st_geometry(poly_3857), sparse = FALSE)[, 1]
  grid[keep, , drop = FALSE]
}

order_tiles_by_center_distance <- function(tiles) {
  stopifnot(all(c("x", "y") %in% names(tiles)))
  x_center <- mean(range(tiles$x))
  y_center <- mean(range(tiles$y))
  tiles$dist <- sqrt((tiles$x - x_center)^2 + (tiles$y - y_center)^2)
  tiles[order(tiles$dist), c("x", "y"), drop = FALSE]
}

merge_tiles_in_chunks <- function(tile_table, z, cache_dir_png, cache_dir_chunks, out_tif, chunk_size = 200L) {
  dir.create(cache_dir_chunks, recursive = TRUE, showWarnings = FALSE)

  n <- nrow(tile_table)
  if (n == 0) stop("No tiles to merge.")

  n_chunks <- ceiling(n / chunk_size)
  chunk_files <- character(n_chunks)

  for (ci in seq_len(n_chunks)) {
    i0 <- (ci - 1L) * chunk_size + 1L
    i1 <- min(ci * chunk_size, n)
    idx <- i0:i1

    message(glue("Merging chunk {ci}/{n_chunks} (tiles {i0}-{i1} of {n})"))

    r_list <- vector("list", length(idx))
    for (k in seq_along(idx)) {
      x <- tile_table$x[idx[k]]
      y <- tile_table$y[idx[k]]
      png_file <- file.path(cache_dir_png, glue("z{z}"), glue("x{x}"), glue("y{y}.png"))
      r_list[[k]] <- tile_rast_from_png(png_file = png_file, z = z, x = x, y = y)
    }

    chunk_r <- terra::merge(terra::sprc(r_list))

    # Defensive: ensure no colour table metadata is carried through (RGB GeoTIFF
    # cannot store a palette colour table).
    try(terra::coltab(chunk_r) <- NULL, silent = TRUE)

    chunk_file <- file.path(cache_dir_chunks, glue("chunk_{ci}.tif"))
    terra::writeRaster(
      chunk_r,
      filename = chunk_file,
      overwrite = TRUE,
      wopt = list(
        gdal = c("COMPRESS=DEFLATE", "TILED=YES"),
        datatype = "INT1U"
      )
    )
    chunk_files[ci] <- chunk_file
  }

  # Merge chunk GeoTIFFs to final on disk.
  message(glue("Merging {length(chunk_files)} chunks -> {out_tif}"))
  out_tmp1 <- file.path(cache_dir_chunks, "merged_tmp_1.tif")
  out_tmp2 <- file.path(cache_dir_chunks, "merged_tmp_2.tif")
  if (file.exists(out_tmp1)) unlink(out_tmp1)
  if (file.exists(out_tmp2)) unlink(out_tmp2)

  # IMPORTANT: terra::merge() forbids writing to the same filename as an input.
  # When iteratively merging to disk, we alternate between two temp files.
  current_file <- chunk_files[1]
  m <- terra::rast(current_file)
  if (length(chunk_files) > 1) {
    for (ci in 2:length(chunk_files)) {
      next_file <- if (identical(current_file, out_tmp1)) out_tmp2 else out_tmp1
      m <- terra::merge(
        terra::rast(current_file),
        terra::rast(chunk_files[ci]),
        filename = next_file,
        overwrite = TRUE
      )
      current_file <- next_file
    }
  }

  # Ensure final is written to requested path.
  try(terra::coltab(m) <- NULL, silent = TRUE)
  terra::writeRaster(
    m,
    filename = out_tif,
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=DEFLATE", "TILED=YES"), datatype = "INT1U")
  )

  invisible(out_tif)
}

# ---- Case-study area (Leeds) -------------------------------------------------

leeds_boundary <- NULL
if (requireNamespace("zonebuilder", quietly = TRUE)) {
  leeds_boundary <- zonebuilder::zb_zone("Leeds", n_circles = 1)
}
if (is.null(leeds_boundary)) {
  stop("Could not create Leeds boundary (missing package 'zonebuilder').")
}

# ---- Main --------------------------------------------------------------------

# Deterministic sanity check (ported from README.qmd)
expected_x <- 519739
expected_y <- 337593
center <- tile_xy_to_lonlat(expected_x, expected_y, z = 20, scheme = "xyz", offset = 0.5)
stopifnot(all(tile_coords_to_xy(center["lon"], center["lat"], z = 20) == c(x = expected_x, y = expected_y)))

message(glue("Computing tiles for Leeds boundary at z={z} ..."))
tiles_all <- tiles_for_polygon(leeds_boundary, z = z)
tiles_all <- order_tiles_by_center_distance(tiles_all)

message(glue("Total tiles intersecting Leeds boundary at z={z}: {nrow(tiles_all)}"))
tiles_use <- head(tiles_all, min(as.integer(n_tiles), nrow(tiles_all)))
message(glue("Will ensure {nrow(tiles_use)} tiles are downloaded and then stitched."))

dir.create(cache_dir_png, recursive = TRUE, showWarnings = FALSE)

downloaded <- 0L
failed <- 0L

for (i in seq_len(nrow(tiles_use))) {
  x <- tiles_use$x[i]
  y <- tiles_use$y[i]
  url <- tile_png_url(base_url = base_url, z = z, x = x, y = y)
  png_file <- file.path(cache_dir_png, glue("z{z}"), glue("x{x}"), glue("y{y}.png"))
  ok <- download_tile_png(url = url, destfile = png_file)
  if (ok) {
    downloaded <- downloaded + 1L
  } else {
    failed <- failed + 1L
    message(glue("FAILED: {url}"))
  }
  if (i %% 50L == 0L) {
    message(glue("Progress: {i}/{nrow(tiles_use)} (downloaded={downloaded}, failed={failed})"))
  }
}

message(glue("Download complete: downloaded={downloaded}, failed={failed}"))

# Keep only successfully downloaded tiles.
png_exists <- vapply(seq_len(nrow(tiles_use)), function(i) {
  x <- tiles_use$x[i]
  y <- tiles_use$y[i]
  f <- file.path(cache_dir_png, glue("z{z}"), glue("x{x}"), glue("y{y}.png"))
  file.exists(f) && file.info(f)$size > 0
}, logical(1))

tiles_downloaded <- tiles_use[png_exists, , drop = FALSE]
message(glue("Tiles present on disk: {nrow(tiles_downloaded)}"))
if (nrow(tiles_downloaded) == 0) stop("No tiles were downloaded; cannot merge.")

merge_tiles_in_chunks(
  tile_table = tiles_downloaded,
  z = z,
  cache_dir_png = cache_dir_png,
  cache_dir_chunks = cache_dir_chunks,
  out_tif = out_tif,
  chunk_size = 200L
)

if (file.exists(out_tif)) {
  sz <- file.info(out_tif)$size
  message(glue("Wrote {out_tif} ({format(sz, big.mark = ',')} bytes)"))
}
