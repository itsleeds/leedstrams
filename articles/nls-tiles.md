# NLS town-plan tiles

``` r
library(leedstrams)
```

## Tile maths

The NLS town plan rasters are served as Web Mercator *XYZ tiles* at URLs
like:

`https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North/{z}/{x}/{y}.png`

The helpers in this package implement the standard XYZ tile maths.

Developer-only, one-off checks (including a deterministic XYZ
round-trip) live in `dev/tests.Rmd`.

## Getting tiles near a point

``` r
z <- 20
hyde_park <- c(lon = -1.561482, lat = 53.814711)
tiles <- nls_tiles_near_lonlat(hyde_park["lon"], hyde_park["lat"], z = z, n_tiles = 4)
tiles
#>        x      y
#> 1 519738 337593
#> 2 519739 337593
#> 3 519738 337594
#> 4 519739 337594
```

## Downloading and stitching tiles

Remote tile downloads are intentionally disabled in this vignette so it
remains cheap to build during `R CMD check`.

To try a small mosaic interactively, flip `eval = FALSE` to
`eval = TRUE`.

``` r
base_url <- "https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North"

r_list <- Map(
  function(x, y) nls_tile_rast_zxy(base_url = base_url, z = z, x = x, y = y),
  tiles$x,
  tiles$y
)

mosaic <- terra::merge(terra::sprc(r_list))
terra::plotRGB(mosaic)
```
