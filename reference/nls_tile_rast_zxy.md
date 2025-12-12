# Read an NLS tile into a georeferenced terra raster

NLS town-plan tiles are paletted PNGs; this helper converts to RGB and
sets the tile extent/CRS (EPSG:3857).

## Usage

``` r
nls_tile_rast_zxy(
  base_url = "https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North",
  z,
  x,
  y
)
```

## Arguments

- base_url:

  Base URL for the NLS tile set.

- z:

  Zoom level.

- x:

  Tile x coordinate.

- y:

  Tile y coordinate.

## Value

A
[terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
in EPSG:3857.
