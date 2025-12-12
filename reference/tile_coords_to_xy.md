# Convert lon/lat to XYZ or TMS tile coordinates

Implements standard Web Mercator tile maths.

## Usage

``` r
tile_coords_to_xy(lon, lat, z, scheme = c("xyz", "tms"))
```

## Arguments

- lon:

  Longitude in decimal degrees.

- lat:

  Latitude in decimal degrees.

- z:

  Zoom level (integer).

- scheme:

  Tile scheme, either "xyz" (Google/OSM) or "tms".

## Value

Named numeric vector with elements `x` and `y`.
