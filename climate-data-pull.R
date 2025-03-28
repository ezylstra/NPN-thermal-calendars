# Download daily temperature data for 2025 so far
# 28 March 2025

library(rnpn)
library(dplyr)
library(lubridate)
library(pins)
library(raster)
library(terra)

# Vector of dates
yesterday <- Sys.Date() - 1
# dates <- seq.Date(as.Date("2025-01-01"), yesterday, by = 1)
dates <- c(yesterday - 1, yesterday)

# Load shapefile with NECASC boundary
roi <- terra::vect("Northeast_CASC/Northeast_CASC.shp") 

# Function to calculate GDDs (from E. Scott)
#' Baskerville-Emin method for GDD calculation
#' 
#' @param tmin Numeric vector; min daily temp in ºF.
#' @param tmax Numeric vector; max daily temp in ºF.
#' @param base Base temp in ºF.
calc_gdd_be <- function(tmin = NULL, tmax = NULL, base = 32) {
  withr::local_options(list(warn = 2)) # Turn warnings into errors in the scope of this function
  .mapply(function(tmin, tmax) { # For each day...
    # NAs beget NAs
    if (is.na(tmin) | is.na(tmax)) {
      return(NA)
    }
    # Check that tmax >= tmin
    if (tmin > tmax) {
      stop("tmin > tmax!")
    }
    # Step 2
    if (tmax < base) {
      return(0)
    }
    # Step 3
    tmean <- (tmin + tmax) / 2
    
    # Step 4
    if (tmin >= base) { # Simple case
      return (tmean - base)
    }
    
    # Step 5
    W <- (tmax - tmin) / 2
    x <- (base - tmean) / W
    # Special case for floating-point errors when `x` is (almost) equal to 1 or -1
    if (isTRUE(all.equal(x, 1))) {
      x <- 1
    }
    if (isTRUE(all.equal(x, -1))) {
      x <- -1
    }
    A <- asin(x)
    gdd <- ((W * cos(A)) - ((base - tmean) * ((pi/2) - A))) / pi
    return(gdd)
  }, dots = list(tmin = tmin, tmax = tmax), MoreArgs = NULL) |> as.numeric()
}

# Loop through dates to create a SpatRaster with daily GDD values
for (i in 1:length(dates)) {
  
  message(paste0("Calculating GDD for ", dates[i]))
  
  # Download min and max temperatures (deg C)
  tmin_download <- npn_download_geospatial(coverage_id = "climate:tmin",
                                           date = dates[i],
                                           format = "geotiff")
  tmax_download <- npn_download_geospatial(coverage_id = "climate:tmax",
                                           date = dates[i],
                                           format = "geotiff") 
  
  # Crop temperature rasters to NE
  tmin_roi <- terra::crop(tmin_download, roi, mask = TRUE)
  tmax_roi <- terra::crop(tmax_download, roi, mask = TRUE)
  
  # Convert temperatures to F
  tmin_roi <- tmin_roi * 9/5 + 32
  tmax_roi <- tmax_roi * 9/5 + 32
  
  # Calculate GDDs
  temps_sds <- terra::sds(tmin_roi, tmax_roi)
  gdd <- terra::lapp(temps_sds, function(x, y) {
    calc_gdd_be(
      tmin = x,
      tmax = y,
      base = 50
    )
  })

  # If Jan 1, then layer becomes the first layer in GDDs SpatRaster
  # If not, append layer to GDDs
  if (yday(dates[i]) == 1) {
    GDDs <- gdd
  } else {
    GDDs <- c(GDDs, gdd)
  }
  
}

# Convert to Raster Brick (Saving SpatRasters as .rds files can be problematic)
GDDs_r <- raster::brick(GDDs)

# Pin GDD raster
board <- board_connect(
  auth = "manual",
  server = Sys.getenv("CONNECT_SERVER"),
  key = Sys.getenv("CONNECT_API_KEY")
)

pin_write(board = board,
          x = GDDs_r,
          name = "ezylstra/GDDs")
