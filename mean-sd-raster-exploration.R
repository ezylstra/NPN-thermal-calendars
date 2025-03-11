# Exploring mean and SD rasters

# ER Zylstra
# 21 Feb 2025

library(terra)
library(ggplot2)
library(tidyterra)

# Load all rasters and then pick two for a single threshold -------------------#
# (Original rasters in mean-sd-rasters folder are not under version control)

# params <- read.csv("shiny-app/parameters.csv")
# thresholds <- sort(unique(params$threshold))
# 
# means_list <- list()
# sds_list <- list()
# 
# for (i in 1:length(thresholds)) {
#   means_list[[i]] <- rast(paste0("mean-sd-rasters/normals_mean_", 
#                                    thresholds[i], ".tiff"))
#   sds_list[[i]] <- rast(paste0("mean-sd-rasters/normals_sd_", 
#                                  thresholds[i], ".tiff"))
# }
# means_rast <- rast(means_list)
# names(means_rast) <- paste0("mean_", thresholds)
# sds_rast <- rast(sds_list)
# names(sds_rast) <- paste0("sd_", thresholds)
# 
# thresh <- 44
# mn <- means_rast[[thresh]]
# sd <- sds_rast[[thresh]]

# Alternatively, just load rasters for a single threshold ---------------------#
# (Mean/SD rasters for 2200 threshold are under version control)

mn <- rast("normals_mean_2200.tiff")
sd <- rast("normals_sd_2200.tiff")

# Explore relationship between mean and SD rasters ----------------------------#

# Reclassify values in mean raster
  # 1 = value [1, 365]
  # 2 = Inf
  # 0 = NA
mn1 <- terra::classify(mn, rcl = matrix(c(0, 365, 1), nrow = 1))
mn1 <- terra::subst(mn1, Inf, 2)
mn1 <- terra::subst(mn1, NA, 0)

# Reclassify values in SD raster
  # 3 = value greater than 0 (0, 365]
  # 6 = 0
  # 0 = NA
sd1 <- terra::classify(sd, rcl = matrix(c(0, 365, 3), nrow = 1))
sd1 <- terra::subst(sd1, 0, 6)
sd1 <- terra::subst(sd1, NA, 0)

# Create new layer that's a sum of reclassified values and then re-label
combo <- mn1 + sd1
  # 0 = both NA
  # 1 = mean value; SD NA
  # 2 = mean Inf; SD NA
  # 4 = mean value; SD > 0
  # 7 = mean value; SD = 0
names(combo) <- "Info"
combo_classes <- data.frame(id = c(0, 1, 2, 4, 7), 
                            Info = c("Both NA",
                                     "mean = value; SD = NA",
                                     "mean = Inf; SD = NA",
                                     "mean = value; SD > 0",
                                     "mean = value; SD = 0"))
levels(combo) <- combo_classes

# Plot different mean-SD combinations
ggplot() +
  geom_spatraster(data = combo) +
  scale_fill_brewer(palette = "Accent") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Counts of cells in each category
freq(combo)

# Based on emails with Eric Scott, here's what all these things mean:
# Mean and SD = NA: outside area of interest
# Mean = Inf (and SD = NA): cell never reached threshold
# Mean = value and SD = NA: cell reached threshold once
# Mean = value and SD = 0: cell reached threshold more than once, but on the same day
# mean = value and SD = value: cell reached threshold more than once (different days)



# A few cells with SD = 0 and mean = some value between 1 and 365. I assume 
  # this happens if there's only one annual value or if all annual values are 
  # identical
# If SD = NA, there are mean cells = value, Inf, and NA.

# What conditions produce mean = value and SD = NA?
# What conditions produce mean = Inf and SD = NA?

# For interactive tool, should we include cells that reached the threshold only
# once in mean maps? If so, we might need to identify those cells in the SD
# layer to more clearly indicate that the threshold was only reached once. 


