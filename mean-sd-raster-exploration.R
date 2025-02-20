# Exploring mean and SD rasters
# E. Zylstra
# 20 Feb 2025

library(terra)

tcdf <- read.csv("shiny-app/parameters.csv")
thresholds <- sort(unique(tcdf$threshold)) # 50 of them

means_list <- list()
sds_list <- list()

for (i in 1:length(thresholds)) {
  means_list[[i]] <- rast(paste0("shiny-app/rasters/normals_mean_", 
                                   thresholds[i], ".tiff"))
  sds_list[[i]] <- rast(paste0("shiny-app/rasters/normals_sd_", 
                                 thresholds[i], ".tiff"))
}
means_rast <- rast(means_list)
names(means_rast) <- paste0("mean_", thresholds)
sds_rast <- rast(sds_list)
names(sds_rast) <- paste0("sd_", thresholds)

# Exploring relationship between mean and SD rasters for a given threshold
thresh <- 45
mn <- means_rast[[thresh]]
sd <- sds_rast[[thresh]]
both <- c(mn, sd)
plot(both)

# In mean raster, we have cells = NA, cells = Inf, and cells in [1, 365]
# In SD raster, we have cells = NA, cells = 0, and cells in (0, some big number]

# What conditions produce mean = Inf vs mean = NA?

# Looking at _targets.R, I think that we've calculated the population SD, which
# is: sqrt(sum((x-mean(x))^2) / length(x))

# Reclassify raster values to make it easier to compare:
  # 0 = 0 (SD raster only)
  # 1 = (0, 365]
  # 2 = Inf (mean raster only)
  # 3 = NA
mn1 <- terra::classify(mn, rcl = matrix(c(0, 365, 1), nrow = 1))
mn1 <- terra::subst(mn1, Inf, 2)
mn1 <- terra::subst(mn1, NA, 3)
values(sd)[values(sd) < 1 & !is.na(values(sd))]
sd1 <- terra::classify(sd, rcl = matrix(c(-1, 0, 0,
                                          0, 100, 1), 
                                        byrow = TRUE, 
                                        nrow = 2))
sd1 <- terra::subst(sd1, Inf, 2)
sd1 <- terra::subst(sd1, NA, 3)

# See how values in these raster cells match up:
table(values(mn1), values(sd1))
  # A few cells with SD = 0 (mean = some value between 1 and 365). I assume this
    # could happen if there's only one annual value or all annual values are identical
  # If SD = value, mean always = value.
  # If SD = NA, there are mean cells = value, Inf, and NA.

# What conditions produce mean = value and SD = NA?
# What conditions produce mean = Inf (and SD = NA)?

# For interactive tool, should we include cells that reached the threshold only
# once in mean layers? If so, probably need to identify those cells in the SD
# layer to more clearly indicate that the threshold was only reached once.  

