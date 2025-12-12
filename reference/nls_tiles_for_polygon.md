# Tiles intersecting a polygon at a given zoom

Computes a bbox grid in EPSG:3857 and filters tiles by intersection with
`poly`.

## Usage

``` r
nls_tiles_for_polygon(poly, z)
```

## Arguments

- poly:

  An `sf` object.

- z:

  Zoom level.

## Value

A data frame with integer columns `x` and `y`.
