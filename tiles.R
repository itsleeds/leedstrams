
library(tidyverse)
library(sf)

# Setup Leeds boundary
# Using zonebuilder as fallback since pct download is failing
# zones = pct::get_pct_zones("west-yorkshire")
# zones_leeds = zones |> 
#   filter(lad_name == "Leeds")
# leeds_boundary_lad = st_union(zones_leeds)
# leeds_boundary = leeds_boundary_lad

# For testing (and robustness):
leeds_boundary = zonebuilder::zb_zone("Leeds", n_circles = 1)

# Define parameters
z = 19
max_ext = 20037508.34
tile_size = (2 * max_ext) / (2^z)

# Calculate tile ranges for Leeds
x_extent = st_transform(leeds_boundary, 3857) |> st_bbox()
x_min_tile = floor((x_extent$xmin + max_ext) / tile_size) 
x_max_tile = ceiling((x_extent$xmax + max_ext) / tile_size)
y_min_tile = floor((max_ext - x_extent$ymax) / tile_size)
y_max_tile = ceiling((max_ext - x_extent$ymin) / tile_size)

# Create directory
dir.create("tiles", showWarnings = FALSE)

# Bulk download loop
# Create a grid of tile coordinates
tile_grid = expand.grid(x = x_min_tile:x_max_tile, y = y_min_tile:y_max_tile)

# Calculate center of the grid (approximate center of Leeds)
x_center = mean(c(x_min_tile, x_max_tile))
y_center = mean(c(y_min_tile, y_max_tile))

# Calculate distance from center for each tile
tile_grid$dist = sqrt((tile_grid$x - x_center)^2 + (tile_grid$y - y_center)^2)

# Sort by distance (ascending) to start from the center and spiral out
tile_grid = tile_grid[order(tile_grid$dist), ]

message(paste("Total tiles to check/download:", nrow(tile_grid)))

# Iterate through sorted tiles
# We'll limit the number of tiles for this test script to verify logic without full download
# Let's try downloading the first 10 valid ones to prove it works
processed_count = 0
max_test_downloads = 20 

for(i in 1:nrow(tile_grid)) {
  if (processed_count >= max_test_downloads) {
    message("Test limit reached. Stopping download loop.")
    break
  }
  
  x = tile_grid$x[i]
  y = tile_grid$y[i]
  
  outfile = glue::glue("tiles/tile_{z}_{x}_{y}.png")
  if (file.exists(outfile)) {
    processed_count = processed_count + 1
    next
  }
  
  u = glue::glue("https://mapseries-tilesets.s3.amazonaws.com/os/town-england/North/{z}/{x}/{y}.png")
  
  tryCatch({
    message(paste("Downloading:", u))
    # Use httr::GET for robust handling of HTTP errors (like 404)
    resp <- httr::GET(u, httr::write_disk(outfile, overwrite = TRUE))
    
    if (httr::http_error(resp)) {
      # If the URL returns an error (e.g. 404), remove the partial/error file
      if (file.exists(outfile)) unlink(outfile)
      message(paste("Failed (HTTP error):", u))
    } else {
      Sys.sleep(0.5) # Be polite
      processed_count = processed_count + 1
      message(paste("Success:", outfile))
    }
  }, error = function(e) {
    message("Error skipping ", u, ": ", e$message)
    if (file.exists(outfile)) unlink(outfile)
  })
}

# Process downloaded tiles
tile_files = list.files("tiles", pattern = glue::glue("tile_{z}_.*.png"), full.names = TRUE)
message(paste("Found", length(tile_files), "tiles to process."))

if(length(tile_files) > 0) {
  r_list = lapply(tile_files, function(f) {
    # Parse filename to get x and y
    parts = strsplit(basename(f), "_")[[1]]
    x = as.numeric(parts[3])
    y = as.numeric(gsub(".png", "", parts[4]))
    
    r = terra::rast(f)
    
    # Set extent
    xmin <- -max_ext + (x * tile_size)
    xmax <- -max_ext + ((x + 1) * tile_size)
    ymax <- max_ext - (y * tile_size)
    ymin <- max_ext - ((y + 1) * tile_size)
    
    terra::ext(r) <- c(xmin, xmax, ymin, ymax)
    terra::crs(r) <- "EPSG:3857"
    return(r)
  })
  
  # Merge
  message("Merging tiles...")
  tile_collection = terra::sprc(r_list)
  leeds_tiles = terra::merge(tile_collection)
  
  # Reproject and save
  message("Reprojecting and saving...")
  leeds_tiles_bn = terra::project(leeds_tiles, "EPSG:27700")
  terra::writeRaster(leeds_tiles_bn, "leeds_townplan_test.tif", overwrite=TRUE)
  
  message("Done. Saved leeds_townplan_test.tif")
} else {
  message("No tiles found to process.")
}
