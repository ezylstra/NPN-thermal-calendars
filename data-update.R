# Download daily temperature data and pin updated rasters
# To be run daily
# 28 March 2025

library(rnpn)
library(dplyr)
library(lubridate)
library(pins)
library(raster)
library(terra)
library(withr)

# Set pins board
board <- board_connect(
  auth = "manual",
  server = Sys.getenv("CONNECT_SERVER"),
  key = Sys.getenv("CONNECT_API_KEY")
)

# Download min and max temperatures (deg C) for yesterday
yesterday <- Sys.Date() - 1
tmin_download <- npn_download_geospatial(coverage_id = "climate:tmin",
                                         date = yesterday,
                                         format = "geotiff")
tmax_download <- npn_download_geospatial(coverage_id = "climate:tmax",
                                         date = yesterday,
                                         format = "geotiff") 

# Load shapefile with NECASC boundary
roi <- terra::vect("Northeast_CASC/Northeast_CASC.shp")
# roi <- terra::project(roi, tmin_download) # Both in lat/long NAD83 (epsg:4269)
tmin_roi <- terra::crop(tmin_download, roi, mask = TRUE)
tmax_roi <- terra::crop(tmax_download, roi, mask = TRUE)

# Convert temperatures to F
tmin_roi <- tmin_roi * 9/5 + 32
tmax_roi <- tmax_roi * 9/5 + 32

#' Baskerville-Emin method for GDD calculation (function from E. Scott)
#' 
#' @param tmin Numeric vector; min daily temp in ºF.
#' @param tmax Numeric vector; max daily temp in ºF.
#' @param base Base temp in ºF.
calc_gdd_be <- function(tmin = NULL, tmax = NULL, base = 32) {
  withr::local_options(list(warn = 2)) #turn warnings into errors in the scope of this function
  .mapply(function(tmin, tmax) { #for each day...
    #NAs beget NAs
    if (is.na(tmin) | is.na(tmax)) {
      return(NA)
    }
    #check that tmax >= tmin
    if (tmin > tmax) {
      stop("tmin > tmax!")
    }
    #step 2
    if (tmax < base) {
      return(0)
    }
    #step 3
    tmean <- (tmin + tmax) / 2
    
    #step4
    if (tmin >= base) { #simple case
      return (tmean - base)
    }
    
    #step5
    W <- (tmax - tmin) / 2
    x <- (base - tmean) / W
    #special case for floating-point errors when `x` is (almost) equal to 1 or -1
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

temps_sds <- terra::sds(tmin_roi, tmax_roi)
gdd <- terra::lapp(temps_sds, function(x, y) {
  calc_gdd_be(
    tmin = x,
    tmax = y,
    base = 50
  )
})

# Convert SpatRaster to Raster Layer
# gdd <- raster::raster(gdd)

# If Jan 1, then layer becomes the first layer of new GDDs. 
# If not, append layer to GDDs.
if (yday(yesterday) == 1) {
  GDDs <- gdd
} else {
  GDDs_brick <- pin_read(board = board,
                         name = "ezylstra/GDDs")
  GDDs <- terra::rast(GDDs_brick)
  GDDs <- c(GDDs, gdd)
}
# Write to pin, as raster brick
# pin_write(board = board,
#           x = GDDs,
#           name = "ezylstra/GDDs")

# Calculate AGDD
agdd <- cumsum(GDDs)

# For each threshold, determine what day of the year the threshold was reached
thresholds <- seq(50, 2500, by = 50)
threshold_doys <- list()

for (i in 1:length(thresholds)) {
  threshold_doys[[i]] <- terra::which.lyr(agdd > thresholds[i])
}

# Convert list of SpatRasters to a raster brick
doys_brick <- rast(threshold_doys)
doys_brick <- brick(doys_brick)

# Project for Leaflet (see leaflet::projectRasterForLeaflet)
doys_brick <- raster::projectRaster(doys_brick, 
                                    raster::projectExtent(doys_brick, 
                                                          crs = "epsg:3857"))

pins::pin_write(doys_brick)
