# App to display rasters with mean and current day-of-year that AGDD thresholds 
# are reached in the northeastern US

# ER Zylstra
# 1 Apr 2025

library(lubridate)
library(raster)
library(terra)
library(shiny)
library(leaflet)
library(leafem)
library(htmlwidgets)
library(bslib)
library(pins)
library(qs)

# Note: using raster instead of terra since terra doesn't seem to play nice with 
# leafem::addImageQuery()

board <- board_connect(
  auth = "manual",
  server = Sys.getenv("CONNECT_SERVER"),
  key = Sys.getenv("CONNECT_API_KEY")
)

# Load current-year rasters
doys <- pin_read(board, "ezylstra/current-doys")
doys <- terra::unwrap(doys)
# Convert to RasterBrick
doys <- raster::brick(doys)
names(doys) <- stringr::str_replace_all(names(doys), "X", "t")

# Load file that lists summary statistics and thresholds
params <- read.csv("parameters.csv")

# Load static mean/SD rasters 
mean_sd <- brick("mean-sd-brick.tif")

# Create mean RasterBrick 
means <- mean_sd[[1:50]]
names(means) <- names(doys)
# Create SD RasterBrick
sds <- mean_sd[[51:100]]
names(sds) <- names(doys)

# Mean, SD, and current-year rasters projected for Leaflet already (epsg:3857):
# +proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 
# +units=m +nadgrids=@null +wktext +no_defs

# Modify function used to format legend labels so we can add dates
myLabelFormat <- function(..., dates = FALSE){ 
  if (dates) { 
    function(type = "numeric", cuts) { 
      dd <- parse_date_time(paste("2019", cuts), orders = "%Y %j")
      dd <- format(dd, "%d %B")
      paste0(cuts, " (", dd, ")")
    } 
  } else {
    labelFormat(...)
  }
}

# ui --------------------------------------------------------------------------#

ui <- page_fillable(
  
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  
  titlePanel("What day of the year do locations in the northeastern U.S. reach 
             accumulated heat thresholds?"),
  
  layout_columns(
    navset_tab(
      nav_panel("Select threshold",
                layout_columns(
                  selectInput(inputId = "threshold", 
                              label = "AGDD threshold (deg F):", 
                              choices = unique(params$threshold)),
                  sliderInput(inputId = "opacity",
                              label = "Opacity:",
                              min = 0,
                              max = 1, 
                              value = 0.8),
                  p("Zoom in and click on the map to identify the mean day of the 
                  year (and SD) for a specific location. Value will appear in 
                  the lower left corner of the map."),
                  col_widths = c(8, 4, 12),
                )
      ),
      nav_panel("Methods",
                p("For each year from 1991-2020, we obtained daily minimum and maximum
                  temperatures for the northeastern US at 4-km resolution from the
                  PRISM Climate Group (Oregon State University, ",
                  a(href = "https://prism.oregonstate.edu",
                    "https://prism.oregonstate.edu", .noWS = "after"),
                  "). We used these data to calculate accumulated growing degree days
                  (AGDD) using the Baskerville-Emin method with a base temperature of 50
                  deg F, and identified the day of the year that each threshold was
                  reached. The maps display the mean day of the year each threshold was
                  reached (or the standard deviation, SD) over the 30-year period. We
                  excluded any locations where the threshold was reached in only one of
                  the 30 years.")
      )
    ),
    card(card_header(textOutput(outputId = "doy_header")), leafletOutput("doy")),
    card(card_header("Mean day of year"), leafletOutput("mean")),
    card(card_header("Standard deviation"), leafletOutput("sd")),
    col_widths = c(12, 4, 4, 4),
    row_heights = c(1, 3, 3, 3)

  ) # end layout_columns
) # end page_fillable

# server ----------------------------------------------------------------------#

server <- shinyServer(function(input, output) {
  
  reacMean <- reactive({means[[paste0("t", input$threshold)]]})
  reacSD <- reactive({sds[[paste0("t", input$threshold)]]})
  reacDOY <- reactive({doys[[paste0("t", input$threshold)]]})
  
  # Logical, where TRUE indicates that all cells in current-year raster are NA 
  # (i.e., no locations have reached this threshold yet)
  reacDOY_NA <- reactive({ifelse(ncell(reacDOY()) == freq(reacDOY(), value = NA),
                                 TRUE, FALSE)})
  
  output$doy_header <- renderText({
    ifelse(reacDOY_NA() == TRUE,
           "Current day of year - NO LOCATIONS REACHED THRESHOLD YET",
           "Current day of year")
  })
  
  output$mean <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
      addTiles() %>%
      htmlwidgets::onRender("
          function(el, x) {
            const observer = new MutationObserver(function(mutations) {
              mutations.forEach(function(mutation) {
                if (mutation.addedNodes.length > 0) {
                  var labels = el.querySelectorAll('.info.legend text'); // 1 Find all legend text elements as before
                  if (labels.length > 0) {
                    //console.log('Applying text alignment to', labels.length, 'legend labels');
                    labels.forEach(function(label) {
                      label.setAttribute('text-anchor', 'start'); // 2
                      label.setAttribute('dx', '5');
                    });
                  }
                }
              });
            });
            // 3 observing the entire map container el for changes
            observer.observe(el, { childList: true, subtree: true });
          }
        ")
  })
  
  output$sd <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
      addTiles() %>%
      htmlwidgets::onRender("
          function(el, x) {
            const observer = new MutationObserver(function(mutations) {
              mutations.forEach(function(mutation) {
                if (mutation.addedNodes.length > 0) {
                  var labels = el.querySelectorAll('.info.legend text'); // 1 Find all legend text elements as before
                  if (labels.length > 0) {
                    //console.log('Applying text alignment to', labels.length, 'legend labels');
                    labels.forEach(function(label) {
                      label.setAttribute('text-anchor', 'start'); // 2
                      label.setAttribute('dx', '5');
                    });
                  }
                }
              });
            });
            // 3 observing the entire map container el for changes
            observer.observe(el, { childList: true, subtree: true });
          }
        ")
  })

  output$doy <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
      addTiles() %>%
      htmlwidgets::onRender("
          function(el, x) {
            const observer = new MutationObserver(function(mutations) {
              mutations.forEach(function(mutation) {
                if (mutation.addedNodes.length > 0) {
                  var labels = el.querySelectorAll('.info.legend text'); // 1 Find all legend text elements as before
                  if (labels.length > 0) {
                    //console.log('Applying text alignment to', labels.length, 'legend labels');
                    labels.forEach(function(label) {
                      label.setAttribute('text-anchor', 'start'); // 2
                      label.setAttribute('dx', '5');
                    });
                  }
                }
              });
            });
            // 3 observing the entire map container el for changes
            observer.observe(el, { childList: true, subtree: true });
          }
        ")
  })
  
  observe({
    
    # Mean map
    # (creating legend that encompasses mean and current-year values)
    pal_mean <- colorNumeric(palette = "viridis", 
                             domain = c(values(reacMean()), values(reacDOY())),
                             na.color = "transparent",
                             reverse = TRUE)
    leafletProxy("mean") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(reacMean(), 
                     colors = pal_mean, 
                     group = "Mean DOY",
                     layerId = "Mean DOY",
                     opacity = input$opacity, 
                     project = FALSE) %>%
      addLegend("bottomright", 
                pal = pal_mean, 
                values = c(values(reacMean()), values(reacDOY())),
                labFormat = myLabelFormat(dates = TRUE),
                title = "Day of year", 
                opacity = 0.8) %>%
      addImageQuery(reacMean(),
                    digits = 2,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "Mean DOY",
                    project = TRUE)   
    
    # SD map
    pal_sd <- colorNumeric(palette = "plasma",
                           domain = values(reacSD()),
                           na.color = "transparent",
                           reverse = TRUE)
    leafletProxy("sd") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(reacSD(), 
                     colors = pal_sd, 
                     group = "SD",
                     layerId = "SD",
                     opacity = input$opacity, 
                     project = FALSE) %>%
      addLegend("bottomright", 
                pal = pal_sd, 
                values = values(reacSD()),
                labFormat = myLabelFormat(dates = FALSE),
                title = "Days", 
                opacity = 0.8) %>%
      addImageQuery(reacSD(),
                    digits = 2,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "SD",
                    project = TRUE)       
    
    # Current DOY map (if at least some locations have reached threshold)
    if (!reacDOY_NA()) {
      leafletProxy("doy") %>%
        clearImages() %>%
        clearControls() %>%
        addRasterImage(reacDOY(), 
                       colors = pal_mean, 
                       group = "Current DOY",
                       layerId = "Current DOY",
                       opacity = input$opacity, 
                       project = FALSE) %>%
        addLegend("bottomright", 
                  pal = pal_mean, 
                  values = c(values(reacMean()), values(reacDOY())),
                  labFormat = myLabelFormat(dates = TRUE),
                  title = "Day of year", 
                  opacity = 0.8) %>%
        addImageQuery(reacDOY(),
                      digits = 2,
                      type = "click",
                      position = "bottomleft",
                      prefix = "",
                      layerId = "Current DOY",
                      project = TRUE)
    } else {
      leafletProxy("doy") %>%
        clearImages() %>%
        clearControls()
    }

  }) # end observe
}) # end server

# run app ---------------------------------------------------------------------#

shinyApp(ui = ui, server = server)
