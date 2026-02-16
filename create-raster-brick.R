# Merge individual mean/SD rasters into a single RasterBrick (for quicker
# loading of the shiny app)

# ER Zylstra
# 16 Feb 2026

# Note: updated this in Feb 2026. Originally, we kept the geographic extent 
# to 14 states in the NE. Then we wanted the app to include the NE and upper
# midwest (like the manuscript). Moved the original rasters to folders that have
# "14state" in the name and put the new rasters with the larger geographic 
# extent into the rasters-means, rasters-sds, and rasters-counts folders.

library(dplyr)
library(raster)
library(leaflet)
library(leafem)

# List of thresholds
thresholds <- seq(50, 2500, by = 50)

# Load rasters into a list 
means_list <- list()
sds_list <- list()
counts_list <- list()

for (i in 1:length(thresholds)) {
  means_list[[i]] <- raster(paste0("rasters-means/normals_mean_", 
                                   thresholds[i], ".tiff"))
  sds_list[[i]] <- raster(paste0("rasters-sds/normals_sd_", 
                                 thresholds[i], ".tiff"))
  counts_list[[i]] <- raster(paste0("rasters-counts/normals_count_", 
                                    thresholds[i], ".tiff"))
}

# Explored mean, SD, and year count rasters in count-years-exploration.R
# Upshot is that there can be 5 types of cells in rasters:
# mean = NA, SD = NA, count = 0: outside area of interest 
# mean = Inf, SD = NA, count = 0: cell reached threshold in 0 years
# mean = value, SD = NA, count = 1: cell reached threshold in 1 year
# mean = value, SD = 0, count > 1: cell reached same threshold every year it was reached
# mean = value, SD = value, count > 1: cells reached different thresholds multiple years

# Create RasterBrick for each threshold, convert mean = Inf to mean = NA and 
# convert cells in mean raster to NA when the threshold wasn't reached in at 
# least X (min_years) years
min_years <- 2

# Commented code in loop below can be used to double-check that the raster
# cell conversions were done correctly

for (i in 1:length(thresholds)) {
  
  comb <- brick(means_list[[i]], sds_list[[i]], counts_list[[i]])
  
  # orig_counts <- as.data.frame(comb) %>%
  #   mutate(mean2 = case_when(
  #     mean == Inf ~ "Inf",
  #     is.na(mean) ~ "NA",
  #     .default = "value"
  #   )) %>%
  #   mutate(sd2 = case_when(
  #     is.na(sd) ~ "NA",
  #     sd == 0 ~ "0",
  #     .default = "value"
  #   )) %>%
  #   mutate(count2 = case_when(
  #     count == 0 ~ "0",
  #     count %in% 1 ~ "1",
  #     count %in% 2:30 ~ "2:30",
  #   )) %>%
  #   count(mean2, sd2, count2)
  
  comb[[1]][comb[[1]] == Inf] <- NA
  comb[[1]][comb[[3]] < min_years] <- NA
  
  # new_counts <- as.data.frame(comb) %>%
  #   mutate(mean2 = case_when(
  #     mean == Inf ~ "Inf",
  #     is.na(mean) ~ "NA",
  #     .default = "value"
  #   )) %>%
  #   mutate(sd2 = case_when(
  #     is.na(sd) ~ "NA",
  #     sd == 0 ~ "0",
  #     .default = "value"
  #   )) %>%
  #   mutate(count2 = case_when(
  #     count == 0 ~ "0",
  #     count %in% 1 ~ "1",
  #     count %in% 2:30 ~ "2:30",
  #   )) %>%
  #   count(mean2, sd2, count2)
  
  # Put mean raster back in list
  means_list[[i]] <- comb[[1]]
}

# Create raster stack for each variable and rename layers
means_rast <- stack(means_list)
names(means_rast) <- paste0("mean_", thresholds)
sds_rast <- stack(sds_list)
names(sds_rast) <- paste0("sd_", thresholds)

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
            filename = "shiny-app/mean-sd-brick-22state", 
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