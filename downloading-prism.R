# Downloading PRISM data
# 8 May 2025

library(httr2)
library(dplyr)
library(stringr)
library(terra)

# Exploring different ways of downloading PRISM data in the new geotiff format

# Select dates for download
dates <- as.Date(c("2025-01-14", "2025-01-15"))
dates_nodash <- str_remove_all(dates, "-")

# Select variables for download
elements <- c("tmin", "tmax")

# Identify folder to hold all PRISM files
prism_folder <- "C:/Users/erin/Desktop/prism/"

# Download PRISM data directly via Web Service --------------------------------#

# https://www.prism.oregonstate.edu/formats/
# https://www.prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf

url_base <- "https://services.nacse.org/prism/data/get/us/4km"

# Download file at a time:
date1 <- dates_nodash[2]
element1 <- elements[2]
pd_folder <- paste0(prism_folder, "prism_", element1, "_us_25m_", date1)
pd_zippath <- paste0(pd_folder, ".zip")

req <- request(url_base)
req <- req %>% req_url_path_append(element1, date1)
req
req_perform(req, path = pd_zippath)

# Unzip folder
suppressWarnings(
  utils::unzip(pd_zippath, exdir = pd_folder)
)
# Remove zip
invisible(file.remove(pd_zippath))

# check:
# tif <- terra::rast(paste0(pd_folder, "/prism_", element1, "_us_25m_", date1, ".tif"))

# Download PRISM via ACIS -----------------------------------------------------#

# https://www.rcc-acis.org/docs_webservices
# https://builder.rcc-acis.org/

acis_base <- "https://grid2.rcc-acis.org/GridData"

elements_acis <- c("mint", "maxt")

# Need to specify bounding box in decimal degrees (W, S, E, N)
bbox <- c(-125, 24, -66, 50)
# These are slightly bigger than we need for CONUS

# Download file at a time:
date1 <- dates[2]
element1 <- elements_acis[2]

# Define parameters as a list
params <- list(
  grid = "prism",
  bbox = paste(bbox, collapse = ","),
  date = date1,
  elems = element1,
  output = "geotiff"
)

req_acis <- request(acis_base) %>%
  req_method("POST") %>%
  req_headers("Content-Type" = "application/json") %>%
  req_body_json(params)
req_acis

resp <- req_perform(req_acis)
raw_dat <- resp_body_raw(resp)

# File name
acis_filename <- paste0(prism_folder, "prism_", element1, "_us_4km_", 
                        dates_nodash[2], ".tif")

# Save to file
writeBin(raw_dat, acis_filename)

# check:
# tif_acis <- terra::rast(acis_filename)

