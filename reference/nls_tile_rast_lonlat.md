# Read an NLS tile by lon/lat

Convenience wrapper around
[`tile_coords_to_xy()`](https://itsleeds.github.io/leedstrams/reference/tile_coords_to_xy.md)
and
[`nls_tile_rast_zxy()`](https://itsleeds.github.io/leedstrams/reference/nls_tile_rast_zxy.md).

## Usage

``` r
nls_tile_rast_lonlat(
  base_url = "https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North",
  z,
  lon,
  lat
)
```

## Arguments

- base_url:

  Base URL for the NLS tile set.

- z:

  Zoom level.

- lon:

  Longitude.

- lat:

  Latitude.

## Value

A
[terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
in EPSG:3857.
