library(dplyr)
library(terra)
library(ggplot2)
library(tidyterra)
library(cowplot)

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

# First, look at rasters and see whether there are any obvious issues
# (Note: this takes a few minutes)
for (thresh in 1:50) {
  comb <- c(means_list[[thresh]], sds_list[[thresh]], counts_list[[thresh]])
  comb[[3]] <- as.factor(comb[[3]])
  title_gg <- ggplot() +
    labs(title = paste0("Threshold: ", thresholds[thresh], " degF"))
  m <- ggplot() +
    geom_spatraster(data = comb[[1]], aes(fill = mean)) +
    scale_fill_viridis_c(name = "Mean")
  sd <- ggplot() + 
    geom_spatraster(data = comb[[2]], aes(fill = sd)) + 
    scale_fill_viridis_c(name = "SD")
  count <- ggplot() +
    geom_spatraster(data = comb[[3]], aes(fill = count)) +
    scale_fill_viridis_d(name = "No. years")
  plots <- plot_grid(m, sd, count, nrow = 2)
  plot(plot_grid(title_gg, plots, ncol = 1, rel_heights = c(0.1, 1)))
}

# Look at values in one set of rasters
thresh <- 50
thresholds[thresh]
comb <- c(means_list[[thresh]], sds_list[[thresh]], counts_list[[thresh]])
df <- data.frame(comb)

head(df)
df <- df %>%
  mutate(mean2 = case_when(
    mean == Inf ~ "Inf",
    is.na(mean) ~ "NA",
    .default = "value"
  )) %>%
  mutate(sd2 = case_when(
    is.na(sd) ~ "NA",
    sd == 0 ~ "0",
    .default = "value"
  )) %>%
  mutate(count2 = case_when(
    count == 0 ~ "0",
    count %in% 1 ~ "1",
    count %in% 2:30 ~ "2:30",
  ))

count(df, mean2, sd2, count2)
# mean = NA, SD = NA, count = 0: outside area of interest 
# mean = Inf, SD = NA, count = 0: cell reached threshold in 0 years
# mean = value, SD = NA, count = 1: cell reached threshold in 1 year
# mean = value, SD = 0, count > 1: cell reached same threshold every year it was reached
# mean = value, SD = value, count > 1: cells reached different thresholds multiple years

# Create a new raster where all means with year count == 1 converted to NA
comb2 <- comb
comb2[[1]][comb2[[3]] == 1] <- NA
df2 <- data.frame(comb2) %>%
  mutate(mean2 = case_when(
    mean == Inf ~ "Inf",
    is.na(mean) ~ "NA",
    .default = "value"
  )) %>%
  mutate(sd2 = case_when(
    is.na(sd) ~ "NA",
    sd == 0 ~ "0",
    .default = "value"
  )) %>%
  mutate(count2 = case_when(
    count == 0 ~ "0",
    count %in% 1 ~ "1",
    count %in% 2:30 ~ "2:30",
  ))
count(df2, mean2, sd2, count2)
# Everything looks good.

# Compare BE rasters to those calculated using simple averaging----------------#
sa_thresholds <- c(50, 1250, 2500)

# Load SA rasters into a list 
means_sa_list <- list()
sds_sa_list <- list()
counts_sa_list <- list()

for (i in 1:length(sa_thresholds)) {
  means_sa_list[[i]] <- rast(paste0("rasters-simple-averaging/normals_mean_simple_", 
                                    sa_thresholds[i], ".tiff"))
  sds_sa_list[[i]] <- rast(paste0("rasters-simple-averaging/normals_sd_simple_", 
                                  sa_thresholds[i], ".tiff"))
  counts_sa_list[[i]] <- rast(paste0("rasters-simple-averaging/normals_count_simple_", 
                                     sa_thresholds[i], ".tiff"))
}

# To compare mean values between BE, SA
be_thresh_index <- which(thresholds %in% sa_thresholds)
diff <- list()
for (thresh in 1:3) {
  bet <- be_thresh_index[thresh]
  comb <- c(means_list[[bet]], means_sa_list[[thresh]])
  diff[[thresh]] <- comb[[1]] - comb[[2]]
  # title_gg <- ggplot() +
  #   labs(title = paste0("Threshold: ", sa_thresholds[thresh], " degF"))
  # be <- ggplot() +
  #   geom_spatraster(data = comb[[1]], aes(fill = mean)) +
  #   scale_fill_viridis_c(name = "BE")
  # sa <- ggplot() + 
  #   geom_spatraster(data = comb[[2]], aes(fill = mean)) + 
  #   scale_fill_viridis_c(name = "SA")
  # plots <- plot_grid(be, sa, nrow = 2)
  # plot(plot_grid(title_gg, plots, ncol = 1, rel_heights = c(0.1, 1)))
}
# Look similar

# Look at differences
diff[[1]][diff[[1]] == -Inf] <- NA
diff[[2]][diff[[2]] == -Inf] <- NA
diff[[3]][diff[[3]] == -Inf] <- NA

ggplot() + 
  geom_spatraster(data = diff[[1]], aes(fill = mean)) + 
  scale_fill_viridis_c(name = "BE - SA", na.value = "transparent") + 
  labs(title = "50 deg")
global(diff[[1]], fun = "mean", na.rm = TRUE) # Mean = -16
global(diff[[1]], fun = "sd", na.rm = TRUE) # SD = 5

ggplot() + 
  geom_spatraster(data = diff[[2]], aes(fill = mean)) + 
  scale_fill_viridis_c(name = "BE - SA", na.value = "transparent") + 
  labs(title = "1250 deg")
global(diff[[2]], fun = "mean", na.rm = TRUE) # Mean = -5
global(diff[[2]], fun = "sd", na.rm = TRUE) # SD = 1.6

ggplot() + 
  geom_spatraster(data = diff[[3]], aes(fill = mean)) + 
  scale_fill_viridis_c(name = "BE - SA", na.value = "transparent") + 
  labs(title = "2500 deg")
global(diff[[3]], fun = "mean", na.rm = TRUE) # Mean = -4
global(diff[[3]], fun = "sd", na.rm = TRUE) # SD = 4.6
