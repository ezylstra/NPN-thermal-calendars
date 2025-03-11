
library(terra)

# Load file that lists summary statistics and thresholds
params <- read.csv("shiny-app/parameters.csv")
thresholds <- sort(unique(params$threshold))

# Load rasters into a list 
means_list <- list()
sds_list <- list()
counts_list <- list()

for (i in 1:length(thresholds)) {
  means_list[[i]] <- rast(paste0("rasters-means/normals_mean_", 
                                   thresholds[i], ".tiff"))
  sds_list[[i]] <- rast(paste0("rasters-sds/normals_sd_", 
                                 thresholds[i], ".tiff"))
  counts_list[[i]] <- rast(paste0("rasters-counts/normals_count_", 
                                    thresholds[i], ".tiff"))
}

thresh <- 1
comb <- c(means_list[[thresh]], sds_list[[thresh]], counts_list[[thresh]])
plot(comb)

thresh <- 50
comb <- c(means_list[[thresh]], sds_list[[thresh]], counts_list[[thresh]])
plot(comb)
plot(comb[[3]] == 29)


