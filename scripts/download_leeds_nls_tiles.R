# Download and stitch NLS (National Library of Scotland) OS town plan tiles for Leeds.
#
# This script now primarily uses functions from the local `leedstrams` package
# (loaded with devtools::load_all()).
#
# Usage:
#   Rscript scripts/download_leeds_nls_tiles.R

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(glue)
})

# Prefer the *local* package code (no install step). Fall back to library().
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else if (requireNamespace("leedstrams", quietly = TRUE)) {
  library(leedstrams)
} else {
  stop("Need either 'devtools' (preferred) or an installed 'leedstrams' package.")
}

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
out_tif <- "tiles_merged_leeds_all.tif"

# ---- Script-local helpers -----------------------------------------------------

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

  terra::ext(r) <- leedstrams::tile_webmerc_extent(z = z, x = x, y = y)
  terra::crs(r) <- "EPSG:3857"
  r
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
  leeds_boundary <- zonebuilder::zb_zone("Leeds", n_circles = 5)
}
if (is.null(leeds_boundary)) {
  stop("Could not create Leeds boundary (missing package 'zonebuilder').")
}

# Ensure we have a single polygon geometry
leeds_boundary <- sf::st_union(leeds_boundary)

# ---- Main --------------------------------------------------------------------

# Deterministic sanity check
expected_x <- 519739
expected_y <- 337593
center <- leedstrams::tile_xy_to_lonlat(expected_x, expected_y, z = 20, scheme = "xyz", offset = 0.5)
stopifnot(all(leedstrams::tile_coords_to_xy(center["lon"], center["lat"], z = 20) == c(x = expected_x, y = expected_y)))

message(glue("Computing tiles for Leeds boundary at z={z} ..."))
tiles_all <- leedstrams::nls_tiles_for_polygon(leeds_boundary, z = z)
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
  png_file <- file.path(cache_dir_png, glue("z{z}"), glue("x{x}"), glue("y{y}.png"))

  # Robust download: continue through missing (404) tiles.
  ok <- leedstrams::nls_download_tile_png(
    base_url = base_url,
    z = z,
    x = x,
    y = y,
    destfile = png_file
  )
  if (ok) {
    downloaded <- downloaded + 1L
  } else {
    failed <- failed + 1L
    message(glue("FAILED: z={z} x={x} y={y}"))
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
