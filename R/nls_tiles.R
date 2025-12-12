#' Download a single NLS town-plan PNG tile
#'
#' @param base_url Base URL for the NLS tile set.
#' @param z Zoom level.
#' @param x Tile x coordinate.
#' @param y Tile y coordinate.
#' @param destfile Destination file path (a `.png`).
#'
#' @return `TRUE` if the file exists after download; otherwise `FALSE`.
#' @export
nls_download_tile_png <- function(
    base_url = "https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North",
    z,
    x,
    y,
    destfile
  ) {
  stopifnot(is.character(destfile), length(destfile) == 1)
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(destfile) && file.info(destfile)$size > 0) {
    return(TRUE)
  }

  url <- glue::glue("{base_url}/{z}/{x}/{y}.png")
  ok <- FALSE

  try({
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

  ok
}

#' Read an NLS tile into a georeferenced terra raster
#'
#' NLS town-plan tiles are paletted PNGs; this helper converts to RGB and sets
#' the tile extent/CRS (EPSG:3857).
#'
#' @param base_url Base URL for the NLS tile set.
#' @param z Zoom level.
#' @param x Tile x coordinate.
#' @param y Tile y coordinate.
#'
#' @return A [terra::SpatRaster] in EPSG:3857.
#' @export
nls_tile_rast_zxy <- function(
    base_url = "https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North",
    z,
    x,
    y
  ) {
  url <- glue::glue("{base_url}/{z}/{x}/{y}.png")
  r <- suppressWarnings(terra::rast(url))

  # Convert paletted PNG -> RGB
  if (terra::nlyr(r) == 1 && terra::has.colors(r)) {
    ct <- terra::coltab(r)[[1]]
    rR <- terra::subst(r, ct$value, ct$red)
    rG <- terra::subst(r, ct$value, ct$green)
    rB <- terra::subst(r, ct$value, ct$blue)
    r <- c(rR, rG, rB)
    terra::RGB(r) <- 1:3
  }

  # Web tiles have a top-left origin
  r <- terra::flip(r, direction = "vertical")

  terra::ext(r) <- tile_webmerc_extent(z = z, x = x, y = y)
  terra::crs(r) <- "EPSG:3857"
  r
}

#' Read an NLS tile by lon/lat
#'
#' Convenience wrapper around `tile_coords_to_xy()` and `nls_tile_rast_zxy()`.
#'
#' @param base_url Base URL for the NLS tile set.
#' @param z Zoom level.
#' @param lon Longitude.
#' @param lat Latitude.
#'
#' @return A [terra::SpatRaster] in EPSG:3857.
#' @export
nls_tile_rast_lonlat <- function(
    base_url = "https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North",
    z,
    lon,
    lat
  ) {
  xy <- tile_coords_to_xy(lon, lat, z = z, scheme = "xyz")
  nls_tile_rast_zxy(base_url = base_url, z = z, x = xy[["x"]], y = xy[["y"]])
}

#' Tile coordinate set near a lon/lat
#'
#' Chooses tile offsets (e.g. 2x2 block for `n_tiles = 4`) that tend to create
#' compact mosaics.
#'
#' @param lon Longitude.
#' @param lat Latitude.
#' @param z Zoom level.
#' @param n_tiles Number of tiles to return.
#'
#' @return A data frame with integer columns `x` and `y`.
#' @export
nls_tiles_near_lonlat <- function(lon, lat, z, n_tiles = 3) {
  n_tiles <- as.integer(n_tiles)
  stopifnot(n_tiles >= 1)

  xy0 <- tile_coords_to_xy(lon, lat, z = z, scheme = "xyz")
  x0 <- as.integer(xy0[["x"]])
  y0 <- as.integer(xy0[["y"]])

  if (n_tiles == 1) {
    offsets <- data.frame(dx = 0, dy = 0)
  } else if (n_tiles == 2) {
    offsets <- data.frame(dx = c(-1, 0), dy = c(0, 0))
  } else if (n_tiles == 3) {
    offsets <- data.frame(dx = c(-1, 0, 1), dy = c(0, 0, 0))
  } else if (n_tiles == 4) {
    offsets <- expand.grid(dx = c(-1, 0), dy = c(-1, 0))
  } else {
    side <- ceiling(sqrt(n_tiles))
    dx_range <- (-floor(side / 2)):(ceiling(side / 2) - 1)
    dy_range <- (-floor(side / 2)):(ceiling(side / 2) - 1)
    offsets <- expand.grid(dx = dx_range, dy = dy_range)
    offsets <- offsets[order(offsets$dx^2 + offsets$dy^2, abs(offsets$dx), abs(offsets$dy)), ]
    offsets <- utils::head(offsets, n_tiles)
  }

  data.frame(
    x = x0 + offsets$dx,
    y = y0 + offsets$dy
  )
}

#' Tiles intersecting a polygon at a given zoom
#'
#' Computes a bbox grid in EPSG:3857 and filters tiles by intersection with
#' `poly`.
#'
#' @param poly An `sf` object.
#' @param z Zoom level.
#'
#' @return A data frame with integer columns `x` and `y`.
#' @export
nls_tiles_for_polygon <- function(poly, z) {
  stopifnot(inherits(poly, "sf") || inherits(poly, "sfc"))

  poly_3857 <- sf::st_transform(poly, 3857)
  bb <- sf::st_bbox(poly_3857)

  max_ext <- 20037508.34
  tile_size <- (2 * max_ext) / (2^z)

  x_min <- floor((bb[["xmin"]] + max_ext) / tile_size)
  x_max <- ceiling((bb[["xmax"]] + max_ext) / tile_size)
  y_min <- floor((max_ext - bb[["ymax"]]) / tile_size)
  y_max <- ceiling((max_ext - bb[["ymin"]]) / tile_size)

  grid <- expand.grid(
    x = seq.int(x_min, x_max),
    y = seq.int(y_min, y_max)
  )

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
