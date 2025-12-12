

## Preprocessing

    Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

    terra 1.8.80


    Attaching package: 'glue'

    The following object is masked from 'package:terra':

        trim

    Loading required namespace: tmaptools

    Please set segment_center = TRUE to divide the centre into multiple segments

### Plan

- Download the town map dataset from Scottish Library:
  https://maps.nls.uk/os/townplans-england/leeds2.html
- Digitise the tramway centerlines from there
- Digitise the tram network from Alex’s image data
- Combine both datasets into a single tramway network
- Bonus: track lines
- Comparing with OSM

### Rail Map online data

![](images/paste-2.png)

See png-to-geojson.py

### Town plan data

Individuals tiles can be accessed from URLs such as
https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North/19/259898/168840.png

This is raster ‘tile pyramids’ - zoom level 19, tile coordinates 259898,
168840.

<!-- We can import this image (single tile): -->

We can put that logic into a small set of helpers:

- Convert lon/lat to XYZ tile coords (and back)
- Download a tile and set its Web Mercator extent
- Convert NLS paletted PNG tiles to RGB so plotting works

# Download a small mosaic near Hyde Park (cheap: \<= 10 tiles)

           x      y
    1 519738 337593
    2 519739 337593
    3 519738 337594
    4 519739 337594

![](README_files/figure-commonmark/hyde-park-mosaic-1.png)

The general pattern is:
https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North/{z}/{x}/{y}.png

You can copy-paste that URL into QGIS ‘XYZ Tiles’ to access the tiles
directly as shown below:

![](images/paste-1.png)

<!-- You can download the tiles using R as follows: -->

You can also use the `ceramic` package to download tiles:

### Extracting tram network from image

The following Python code processes the image `images/paste-2.png` to
extract the Leeds tram network lines (purple), skeletonizes them, and
converts the result to LineStrings.

## Research

- Compare with historic economic and usage data
- …
