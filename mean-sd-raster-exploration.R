# Exploring mean and SD rasters
# E. Zylstra
# 19 Feb 2025

library(terra)

tcdf <- read.csv("shiny-app/parameters.csv")
thresholds <- sort(unique(tcdf$threshold))

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
mn <- means_rast[[35]]
sd <- sds_rast[[35]]
both <- c(mn, sd)

mn1 <- terra::classify(mn, rcl = matrix(c(0, 365, 1), nrow = 1))
mn1 <- terra::subst(mn1, Inf, 2)
mn1 <- terra::subst(mn1, NA, 3)
values(sd)[values(sd) < 1 & !is.na(values(sd))]
sd1 <- terra::classify(sd, rcl = matrix(c(-1, 0, 0,
                                          0, 100, 1), byrow = TRUE, nrow = 2))
sd1 <- terra::subst(sd1, Inf, 2)
sd1 <- terra::subst(sd1, NA, 3)
# See how values in these raster cells match up:
  # 0 = 0 (mean layer doesn't have any 0s)
  # 1 = (0, 365]
  # 2 = Inf (SD layer doesn't have any Inf)
  # 3 = NA
table(values(mn1), values(sd1))
  # few cells with SD = 0 and value for mean
  # If SD = NA, there are mean cells = value, = Inf, and = NA

# Assuming if we get SD = 0, there is > 1 annual value, but they're identical
# If mean = value and SD = NA, does that mean that there's only one year of data?
# If mean = Inf and SD = NA, what does that mean?
# If both are NA, I assume that means that there are no annual values

# In current app, treating Inf like NAs for display purposes.




