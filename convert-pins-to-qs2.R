# Need to convert qs pins to qs2

library(dplyr)
library(pins)
library(qs)
library(qs2)
library(terra)

board <- board_connect(
  auth = "manual",
  server = Sys.getenv("CONNECT_SERVER"),
  key = Sys.getenv("CONNECT_API_KEY")
)

# name <- "ezylstra/tc-GDDs-2026"
# name <- "ezylstra/tc-anomalies-2026"
# name <- "ezylstra/tc-current-doys-2026"
name <- "ezylstra/tc-means"

# Download qs type
x <- board %>% pin_download(name) %>% qs::qread()

# Overwrite as qs2 type
pin_write(board = board, x = x, type = "qs2", name = name)

# Check
pin_meta(board, name)
checkraster <- pin_read(board, name)
checkraster <- terra::unwrap(checkraster)
checkraster
plot(checkraster[[1]])
