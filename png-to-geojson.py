import rasterio
import cv2
import numpy as np
from skimage.morphology import skeletonize
from shapely.geometry import LineString
import geopandas as gpd
import matplotlib.pyplot as plt

# Path to the georeferenced TIFF
tif_path = 'images/tramways-georeferenced.tif'
output_file = "tramways.geojson"

print(f"Processing {tif_path}...")

try:
    with rasterio.open(tif_path) as src:
        # Read the image data
        # Read as (bands, height, width)
        img_data = src.read()
        
        # Get the transform and CRS
        transform = src.transform
        crs = src.crs
        print(f"CRS detected: {crs}")
        
        # Prepare image for OpenCV
        # Transpose to (height, width, bands)
        img_transposed = np.transpose(img_data, (1, 2, 0))
        
        # If 4 bands, drop alpha or use it as mask? 
        # Usually we just want RGB for color detection.
        if img_transposed.shape[2] >= 3:
            img_rgb = img_transposed[:, :, :3]
        else:
            raise ValueError("Image needs at least 3 bands (RGB)")
            
        # Convert RGB to HSV (Note: rasterio reads as RGB usually, unlike cv2 which reads BGR)
        # So we use COLOR_RGB2HSV
        hsv = cv2.cvtColor(img_rgb, cv2.COLOR_RGB2HSV)

        # Define color range for the Leeds tram network (purple/blue lines)
        # Same values as before, assuming the colors are preserved in the TIFF
        lower_purple = np.array([120, 50, 50])
        upper_purple = np.array([170, 255, 255])

        # Create a mask
        mask = cv2.inRange(hsv, lower_purple, upper_purple)

        # Skeletonize the mask
        skeleton = skeletonize(mask // 255)

        # Find contours
        contours, _ = cv2.findContours(skeleton.astype(np.uint8), cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

        lines = []
        for cnt in contours:
            if len(cnt) >= 2:
                points = cnt.squeeze()
                
                # Handle cases where squeeze might result in 1D array if points are collinear or few
                if points.ndim == 1:
                    # If we have [x, y], it's a single point, but we checked len >= 2
                    # If original was [[x,y], [x,y]], squeeze -> [[x,y], [x,y]] (2,2)
                    # If original was [[x,y]], squeeze -> [x,y] (2,) -> invalid for line
                    continue
                
                map_points = []
                for pt in points:
                    # cv2 contours are (x, y) -> (col, row)
                    col, row = pt
                    # Use rasterio transform to get map coordinates
                    # src.xy(row, col, offset='center') returns center of pixel
                    x_map, y_map = src.xy(row, col)
                    map_points.append((x_map, y_map))
                
                if len(map_points) >= 2:
                    lines.append(LineString(map_points))

        # Create GeoDataFrame
        gdf = gpd.GeoDataFrame(geometry=lines)
        
        # Set CRS
        if crs:
            gdf.set_crs(crs, inplace=True)
            # Reproject to WGS84
            gdf = gdf.to_crs(epsg=4326)
            print("Reprojected to EPSG:4326 (WGS84)")
        else:
            print("Warning: No CRS found in TIFF")

        # Save
        gdf.to_file(output_file, driver="GeoJSON")
        print(f"Saved georeferenced network to {output_file}")
        
        # Plotting for verification
        fig, ax = plt.subplots(1, 2, figsize=(12, 6))
        ax[0].imshow(img_rgb)
        ax[0].set_title('Original Image (RGB)')
        ax[1].imshow(skeleton, cmap='gray')
        ax[1].set_title('Skeletonized Network')
        # plt.show()
        print("Plot created (not shown)")

except Exception as e:
    print(f"An error occurred: {e}")