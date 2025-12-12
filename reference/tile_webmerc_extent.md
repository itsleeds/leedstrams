# Web Mercator extent of an XYZ tile

Web Mercator extent of an XYZ tile

## Usage

``` r
tile_webmerc_extent(z, x, y)
```

## Arguments

- z:

  Zoom level.

- x:

  Tile x coordinate.

- y:

  Tile y coordinate.

## Value

A [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html)
object.
