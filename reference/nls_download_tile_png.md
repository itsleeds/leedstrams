# Download a single NLS town-plan PNG tile

Download a single NLS town-plan PNG tile

## Usage

``` r
nls_download_tile_png(
  base_url = "https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North",
  z,
  x,
  y,
  destfile
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

- destfile:

  Destination file path (a `.png`).

## Value

`TRUE` if the file exists after download; otherwise `FALSE`.
