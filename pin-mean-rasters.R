# Move 30-year Mean rasters to a pin on Posit Connect

library(pins)
library(raster)
library(terra)

# Establish connection
board <- board_connect(
  auth = "manual",
  server = Sys.getenv("CONNECT_SERVER"),
  key = Sys.getenv("CONNECT_API_KEY")
)

# Load static mean/SD rasters 
mean_sd <- raster::brick("shiny-app/mean-sd-brick.tif")

# Create mean RasterBrick 
means <- mean_sd[[1:50]]

# Convert to SpatRaster
means <- terra::rast(means)
names(means) <- seq(50, 2500, by = 50)

# Write to pin
means <- terra::wrap(means)
pin_write(board = board,
          x = means,
          type = "qs",
          name = "ezylstra/means")
