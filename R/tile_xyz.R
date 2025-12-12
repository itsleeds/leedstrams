#' Convert lon/lat to XYZ or TMS tile coordinates
#'
#' Implements standard Web Mercator tile maths.
#'
#' @param lon Longitude in decimal degrees.
#' @param lat Latitude in decimal degrees.
#' @param z Zoom level (integer).
#' @param scheme Tile scheme, either "xyz" (Google/OSM) or "tms".
#'
#' @return Named numeric vector with elements `x` and `y`.
#' @export
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

#' Convert XYZ/TMS tile coordinates to lon/lat
#'
#' @param x Tile x coordinate.
#' @param y Tile y coordinate.
#' @param z Zoom level.
#' @param scheme Tile scheme, either "xyz" (Google/OSM) or "tms".
#' @param offset Offset within the tile, where 0.5 gives the tile centre.
#'
#' @return Named numeric vector with elements `lon` and `lat`.
#' @export
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

#' Web Mercator extent of an XYZ tile
#'
#' @param z Zoom level.
#' @param x Tile x coordinate.
#' @param y Tile y coordinate.
#'
#' @return A [terra::ext()] object.
#' @export
tile_webmerc_extent <- function(z, x, y) {
  max_ext <- 20037508.34
  tile_size <- (2 * max_ext) / (2^z)

  xmin <- -max_ext + (x * tile_size)
  xmax <- -max_ext + ((x + 1) * tile_size)
  ymax <- max_ext - (y * tile_size)
  ymin <- max_ext - ((y + 1) * tile_size)

  terra::ext(xmin, xmax, ymin, ymax)
}

