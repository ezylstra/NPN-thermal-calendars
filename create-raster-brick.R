# Merge individual mean/SD rasters into a single RasterBrick (for quicker
# loading of the shiny app)

# ER Zylstra
# 20 Feb 2025

library(raster)
library(leaflet)
library(leafem)

# Note: using raster instead of terra for this project since terra doesn't seem 
# to play nice with leafem::addImageQuery()

# Load file that lists summary statistics and thresholds
params <- read.csv("shiny-app/parameters.csv")
thresholds <- sort(unique(params$threshold))

# Load rasters into a list 
means_list <- list()
sds_list <- list()

for (i in 1:length(thresholds)) {
  means_list[[i]] <- raster(paste0("mean-sd-rasters/normals_mean_", 
                                   thresholds[i], ".tiff"))
  sds_list[[i]] <- raster(paste0("mean-sd-rasters/normals_sd_", 
                                 thresholds[i], ".tiff"))
}

# Create RasterStacks, rename layers, and replace Inf values with NA
means_rast <- stack(means_list)
names(means_rast) <- paste0("mean_", thresholds)
means_rast[means_rast == Inf] <- NA
sds_rast <- stack(sds_list)
names(sds_rast) <- paste0("sd_", thresholds)
sds_rast[sds_rast == Inf] <- NA

# Merge all rasters and convert to a RasterBrick
all_rast <- stack(means_rast, sds_rast)
all_rast <- brick(all_rast)

# Project for Leaflet (see leaflet::projectRasterForLeaflet)
all_rast <- projectRaster(all_rast, raster::projectExtent(all_rast, 
                                                          crs = "epsg:3857"))
# Notes on projection of raster layers:
# I think that you get a tiny bit of misalignment between the raster layer and 
# the base map/pointer that's unavoidable, but it's only noticeable when you're 
# zoomed in really far. You definitely get better results if you project the 
# raster/brick first and then set project = FALSE in addRasterImage().

# Write RasterBrick to file (in shiny-app folder)
writeRaster(all_rast, 
            filename = "shiny-app/mean-sd-brick", 
            format = "GTiff", 
            overwrite = TRUE, 
            options = c("INTERLEAVE = BAND", "COMPRESS = LZW"))

# Test run with leaflet -------------------------------------------------------#

one_rast <- all_rast[["mean_2000"]]

pal <- colorNumeric(palette = "viridis", 
                    domain = values(one_rast),
                    na.color = "transparent",
                    reverse = TRUE)

leaflet() %>%
  fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
  addTiles() %>%
  addRasterImage(one_rast, 
                 colors = pal, 
                 group = "Value",
                 layerId = "Value",
                 opacity = 0.8, 
                 project = FALSE) %>%
  addLegend("bottomright", pal = pal, 
            values = values(one_rast),
            title = "DOY", opacity = 0.8) %>%
  addImageQuery(one_rast,
                digits = 2,
                type = "click",
                position = "bottomleft",
                prefix = "",
                layerId = "Value",
                project = TRUE)
