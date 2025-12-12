# Convert XYZ/TMS tile coordinates to lon/lat

Convert XYZ/TMS tile coordinates to lon/lat

## Usage

``` r
tile_xy_to_lonlat(x, y, z, scheme = c("xyz", "tms"), offset = 0.5)
```

## Arguments

- x:

  Tile x coordinate.

- y:

  Tile y coordinate.

- z:

  Zoom level.

- scheme:

  Tile scheme, either "xyz" (Google/OSM) or "tms".

- offset:

  Offset within the tile, where 0.5 gives the tile centre.

## Value

Named numeric vector with elements `lon` and `lat`.
