# Shiny app to explore what day of the year AGDD thresholds are met in the
# northeastern US: 30-year mean and SD, as well as day in current year

library(lubridate)
library(dplyr)
library(raster)
library(terra)
library(shiny)
library(shinyjs)
library(leaflet)
library(leaflet.extras2)
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

# List of thresholds
threshold_list <- seq(50, 2500, by = 50)

# Load file with pest-specific thresholds
pests <- read.csv("pest-thresholds-subset.csv")
pests <- pests %>%
  mutate(layer = paste0("t", threshold))

# Load static mean/SD rasters 
mean_sd <- brick("mean-sd-brick.tif")

# Create mean RasterBrick 
means <- mean_sd[[1:50]]
names(means) <- paste0("t", threshold_list)
# Create SD RasterBrick
sds <- mean_sd[[51:100]]
names(sds) <- paste0("t", threshold_list)

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

  useShinyjs(),

  titlePanel("What day of the year do locations in the northeastern U.S. reach 
             accumulated heat thresholds?"),
  
  layout_columns(
    navset_card_tab(
      nav_panel("Settings",
        layout_columns(
          p("Select a threshold directly or select a pest species, event and 
            threshold type. Click on the Plot button to display maps."),
          radioButtons("method", "Selection method",
                       choices = c("Threshold" = "Threshold", "Pest" = "Pest")),
          selectInput(inputId = "threshold", 
                      label = "AGDD threshold:", 
                      choices = unique(threshold_list)), 
          selectInput(inputId = "pest",
                      label = "Species",
                      choices = unique(pests$spp)),
          selectInput("event", "Biological event", choices = NULL),
          selectInput("type", "Threshold type", choices = NULL),
          actionButton(inputId = "plot",
                       label = "Plot", class = "btn btn-primary"),
          value_box(title = "Selected threshold",
                    value = uiOutput("selected"),
                    theme = "text-blue"),
          sliderInput(inputId = "opacity",
                      label = "Opacity:",
                      min = 0,
                      max = 1, 
                      value = 0.8),          
          col_widths = c(12, 2, 2, 3, 3, 2, 1, 2, -7, 2),
          row_heights = c(1, rep(6, 5), rep(6, 4))
        )
      ),
      nav_panel("Methods",
                p(strong("Mean day of year and standard deviation (SD) maps: "), 
                  "For each year from 1991-2020, we obtained daily minimum and maximum
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
                  the 30 years."),
                p(strong("Current day of year map: "),
                  "Map displays the day of the current year the selected AGDD threshold 
                  was reached. Daily minimum and maximum temperatures for each day of the
                  current year were obtained from USA-NPN (link) based on hourly 
                  data from NCEP (info). These data were then used to calculate daily 
                  AGDD values (through yesterday) using the Baskerville-Emin method 
                  with a base temperature of 50 deg F.")
      ),
    ),
    card(card_header(textOutput(outputId = "doy_header")), leafletOutput("doy")),
    card(card_header("Mean day of year"), leafletOutput("mean")),
    card(card_header("Standard deviation"), leafletOutput("sd")),
    col_widths = c(12, 4, 4, 4),
    row_heights = c(2, 3, 3, 3)

  ) # end layout_columns
) # end page_fillable

# server ----------------------------------------------------------------------#

server <- shinyServer(function(input, output, session) {
  
  # Create base maps
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
  
  # Create choices for Event input
  pest_tab <- reactive({
    filter(pests, spp == input$pest)
  })
  observeEvent(pest_tab(), {
    choices <- unique(pest_tab()$event)
    updateSelectInput(inputId = "event", choices = choices)
  })
  
  # Create choices for Type input
  event_tab <- reactive({
    filter(pest_tab(), event == input$event)
  })
  observeEvent(event_tab(), {
    choices <- unique(event_tab()$type)
    updateSelectInput(inputId = "type", choices = choices)
  })
  
  # Enable/disable inputs depending on selection method (Pest/Threshold)
  observeEvent(input$method, {
    if(input$method == "Pest") {
      disable("threshold")
      enable("pest")
      enable("event")
      enable("type")
    } else {
      enable("threshold")
      disable("pest")
      disable("event")
      disable("type")
    }
  })
  
  # Execute after clicking Plot button
  observeEvent(input$plot, {
    req(input$method)
    if (input$method == "Pest") {
      req(input$pest, input$event, input$type)
      reacMean <- means[[pests$layer[pests$spp %in% input$pest &
                                       pests$event %in% input$event &
                                       pests$type %in% input$type]]]
      reacDOY <- doys[[pests$layer[pests$spp %in% input$pest &
                                     pests$event %in% input$event &
                                     pests$type %in% input$type]]]
      reacSD <- sds[[pests$layer[pests$spp %in% input$pest &
                                   pests$event %in% input$event &
                                   pests$type %in% input$type]]]
      output$selected <- renderText({pests$threshold[pests$spp %in% input$pest &
                                                       pests$event %in% input$event &
                                                       pests$type %in% input$type]})
    } else {
      req(input$threshold)
      reacMean <- means[[paste0("t", input$threshold)]]
      reacDOY <- doys[[paste0("t", input$threshold)]]
      reacSD <- sds[[paste0("t", input$threshold)]]
      output$selected <- renderText({input$threshold})
    }

    # Determine whether any locations have met threshold
    reacDOY_NA <- ifelse(ncell(reacDOY) == freq(reacDOY, value = NA), TRUE, FALSE)
    
    # Add note to Current DOY panel if no locations have reached threshold yet
    output$doy_header <- renderText({
      ifelse(reacDOY_NA == TRUE,
             "Current day of year - NO LOCATIONS REACHED THRESHOLD YET",
             "Current day of year")
    })
    
    # Add rasters to each base map (using same legend for both DOY maps)
    pal_mean <- colorNumeric(palette = "viridis",
                             domain = c(values(reacMean), values(reacDOY)),
                             na.color = "transparent",
                             reverse = TRUE)
    
    leafletProxy("mean") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(reacMean,
                     colors = pal_mean,
                     group = "Mean DOY",
                     layerId = "Mean DOY",
                     opacity = input$opacity,
                     project = FALSE) %>%
      addLegend("bottomright",
                pal = pal_mean,
                values = c(values(reacMean), values(reacDOY)),
                labFormat = myLabelFormat(dates = TRUE),
                title = "Day of year",
                opacity = 0.8) %>%
      addImageQuery(reacMean,
                    digits = 2,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "Mean DOY",
                    project = TRUE) %>%
      addLeafletsync(c("mean", "doy", "sd"))
    
    if (!reacDOY_NA) {
      leafletProxy("doy") %>%
        clearImages() %>%
        clearControls() %>%
        addRasterImage(reacDOY, 
                       colors = pal_mean, 
                       group = "Current DOY",
                       layerId = "Current DOY",
                       opacity = input$opacity, 
                       project = FALSE) %>%
        addLegend("bottomright", 
                  pal = pal_mean, 
                  values = c(values(reacMean), values(reacDOY)),
                  labFormat = myLabelFormat(dates = TRUE),
                  title = "Day of year", 
                  opacity = 0.8) %>%
        addImageQuery(reacDOY,
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
    
    pal_sd <- colorNumeric(palette = "plasma",
                           domain = values(reacSD),
                           na.color = "transparent",
                           reverse = TRUE)
    leafletProxy("sd") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(reacSD, 
                     colors = pal_sd, 
                     group = "SD",
                     layerId = "SD",
                     opacity = input$opacity, 
                     project = FALSE) %>%
      addLegend("bottomright", 
                pal = pal_sd, 
                values = values(reacSD),
                labFormat = myLabelFormat(dates = FALSE),
                title = "Days", 
                opacity = 0.8) %>%
      addImageQuery(reacSD,
                    digits = 2,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "SD",
                    project = TRUE)  
    
  }) # end observeEvent
}) # end server

# run app ---------------------------------------------------------------------#

shinyApp(ui = ui, server = server)
