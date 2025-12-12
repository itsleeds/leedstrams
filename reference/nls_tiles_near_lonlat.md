# Tile coordinate set near a lon/lat

Chooses tile offsets (e.g. 2x2 block for `n_tiles = 4`) that tend to
create compact mosaics.

## Usage

``` r
nls_tiles_near_lonlat(lon, lat, z, n_tiles = 3)
```

## Arguments

- lon:

  Longitude.

- lat:

  Latitude.

- z:

  Zoom level.

- n_tiles:

  Number of tiles to return.

## Value

A data frame with integer columns `x` and `y`.
