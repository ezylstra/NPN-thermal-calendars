# App to display AGDD threshold rasters (current DOY & anomaly; 30-year mean
# DOY & SD)
# New version to sink queries across maps

# ER Zylstra
# 12 Feb 2026

library(dplyr)
library(lubridate)
library(stringr)
library(raster)
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(htmlwidgets)
library(bslib)
library(shinyjs)
library(pins)
library(qs)

board <- board_connect(
  auth = "manual",
  server = Sys.getenv("CONNECT_SERVER"),
  key = Sys.getenv("CONNECT_API_KEY")
)

# Get year (for yesterday - last date we have data for)
yesterday <- Sys.Date() - 1
yesterday_yr <- year(yesterday)

# Load current-year rasters
doys <- pin_read(board, paste0("ezylstra/current-doys-", yesterday_yr, "-prism"))
doys <- terra::unwrap(doys)
# Convert to RasterBrick
doys <- raster::brick(doys)
names(doys) <- stringr::str_replace_all(names(doys), "X", "t")

# Load anomaly rasters
anoms <- pin_read(board, paste0("ezylstra/anomalies-", yesterday_yr, "-prism"))
anoms <- terra::unwrap(anoms)
# Convert to RasterBrick
anoms <- raster::brick(anoms)
names(anoms) <- stringr::str_replace_all(names(anoms), "X", "t")

# List of thresholds
threshold_list <- seq(50, 2500, by = 50)

# Load file with pest-specific thresholds
pests <- read.csv("pest-thresholds-2spp.csv")
pests_empty <- data.frame(spp = "", event = "", threshold = NA)
pests <- rbind(pests_empty, pests) %>%
  mutate(layer = paste0("t", threshold))

# Load static mean/SD rasters 
mean_sd <- brick("mean-sd-brick.tif")

# Create mean RasterBrick 
means <- mean_sd[[1:50]]
names(means) <- paste0("t", threshold_list)
# Create SD RasterBrick
sds <- mean_sd[[51:100]]
names(sds) <- paste0("t", threshold_list)

# Modify function used to format legend labels, accomodating dates & anomalies
myLabelFormat <- function(..., dates = FALSE, anomalies = FALSE){ 
  if (dates) { 
    function(type = "numeric", cuts) { 
      dd <- parse_date_time(paste("2019", cuts), orders = "%Y %j")
      dd <- format(dd, "%d %B")
      paste0(cuts, " (", dd, ")")
    } 
  } else if (anomalies) {
    function(type = "numeric", cuts) {
      labels <- paste0(abs(cuts), " days late")
      ind_neg <- which(cuts < 0)
      ind_0 <- which(cuts == 0)
      labels[ind_0] <- "Average"
      labels[ind_neg] <- str_replace(labels[ind_neg], "late", "early")
      labels
    }
  } else {
    labelFormat(...)
  }
}

plot_card <- function(header, ...) {
  card(
    full_screen = TRUE,
    card_header(header, class = "graylabel"),
    card_body(..., min_height = 150, class = "p-0")
  )
}

# ui --------------------------------------------------------------------------#

ui <- page_sidebar(
  
  useShinyjs(),

  padding = 0, 
  
  theme = bs_theme() %>%
    bs_add_rules("
      .graylabel {
         background-color: #898989 !important;
         color: white;
      }
  "),

  tags$head(
    tags$style(
      ".leaflet .legend {
                 line-height: 95%;
                 font-size: 95%;
                 }",
      ".bslib-card .card-header {
                    line-height: 0.7rem;
                    }",
      ".sidebar-content {
                    font-size: 90%;
                    margin: 0 !important;
                    padding-top: 5px !important;
                    }",
      ".bslib-sidebar-layout .sidebar {
                    padding-top: 0 !important;
                    }",
      ".nav-tabs {
                    margin: 0 !important;
                    padding: 0 !important;
                    }"
    )
  ),
  
  title = h4("What day of the year do locations in the northeastern U.S. reach
             accumulated growing degree day thresholds?"),
  
  sidebar = sidebar(
    padding = list(0, "1rem", "1rem", "1rem"),  # top, right, bottom, left
    div(class = "sidebar-content",
        navset_tab(
          nav_panel("Settings",
                    br(),
                    p("Begin by selecting an accumulated growing degree day
                      (AGDD) threshold of interest from the dropdown menu and 
                      clicking the ", strong("Plot"), " button to view maps.
                      Clicking on one of the maps will provide the values at 
                      that location in the the lower left corner of each map.",
                      br(),
                      br(),
                      "Pest examples: Two common pests in this region are 
                      provided to illustrate how this tool may be used when AGDD 
                      thresholds for a species are known. After selecting the ", 
                      strong("Pest"),
                      "radio button and the subsequent options of interest, 
                      click ", strong("Update threshold"), " and then the ",
                      strong("Plot"), " button to view maps. A list of known 
                      AGDD thresholds for other pests maybe found on the ",
                      tags$a(
                        href = "https://www.usanpn.org/data/maps/forecasts",
                        "USA-NPN website."
                      )
                    ),
                    radioButtons("method", "Selection method",
                                 choices = c("Threshold" = "Threshold", 
                                             "Pest" = "Pest"),
                                 inline = TRUE),
                    uiOutput("selections"),
                    br(),
                    br(),
                    sliderInput(inputId = "opacity",
                                label = "Opacity:",
                                min = 0,
                                max = 1, 
                                value = 0.8)
          ),
          nav_panel("Background",
                    br(),
                    p("This website displays maps indicating when locations in 
                      the northeastern U.S. reach accumulated growing degree day 
                      (AGDD) thresholds, both in the current calendar year and 
                      averaged over a recent 30-year period (1991-2020). Maps 
                      are provided for 50 thresholds in 50 degree-day 
                      increments, from 50 AGDD to 2500 AGDD. Thresholds can be 
                      selected in the Settings tab by selecting an AGDD 
                      threshold directly or by selecting a pest species and 
                      biological event.",
                      br(),
                      br(),
                      "For each threshold, four maps are displayed:"),
                      tags$ul(
                        tags$li(strong("Current day of year: "), 
                                "The day of year the threshold was reached this 
                                calendar year."),
                        tags$li(strong("Deviation from average: "), 
                                "The number of days earlier or later the
                                threshold has been reached this year compared to
                                the average day the threshold was reached in 
                                1991-2020."),
                        tags$li(strong("Average day of year: "),
                                "The average day of year the threshold was 
                                reached between 1991 and 2020."),
                        tags$li(strong("Variability associated with average conditions: "),
                                "The variability (standard deviation) associated 
                                with the average day of year the threshold was 
                                reached between 1991 and 2020.")
                      ),
                    p("This project was supported by the Northeast Climate 
                      Adapation Science Center, the USA National Phenology 
                      Network, the University of Arizona, and the U.S. Forest 
                      Service."),
                    div(
                      # style = "text-align: center;",
                      style = "display: inline-flex; justify-content: space-evenly; align-items: center;",
                      img(src = "NECASClogo.png", height = "70%", width = "70%"),
                      img(src = "USFSlogo.png", height = "20%", width = "20%")
                    ),
                    div(
                      style = "display: inline-flex; justify-content: space-evenly; align-items: center;",
                      img(src = "NPNlogo.png", height = "50%", width = "50%"),
                      img(src = "UAlogo.png", height = "25%", width = "25%")
                    )
          ),
          nav_panel("Methods",
                    br(),
                    p(strong("Current day of year"), " and ",
                      strong("Deviation from average"), " maps: ",
                      br(),
                      "We obtained daily minimum and maximum temperatures 
                      for the northeastern U.S. at 4-km resolution for the 
                      current calendar year through yesterday from the PRISM 
                      Climate Group (Oregon State University, ",
                      tags$a(href = "https://prism.oregonstate.edu", 
                             "https://prism.oregonstate.edu", 
                             .noWS = "after"),
                      "). We used these data to calculate accumulated growing 
                      degree days (AGDD) using the Baskerville-Emin method with 
                      a base temperature of 50 deg F and a start date of 
                      January 1, and identified the day of the year that each of
                      50 thresholds was reached. To calculate deviations from
                      average conditions, we subtracted the current day of the 
                      year a threshold was reached from the long-term average; 
                      negative values indicate a threshold was reached earlier 
                      than normal and positive values indicate a threshold was 
                      reached later than normal."
                    ), 
                    p(strong("Average day of year"), " and ",
                      strong("Variability associated with average conditions"),
                      "maps:",
                      br(),
                      "We used the same methods described above to
                      calculate daily AGDD values for each year from 1991 to 
                      2020. The maps display the average day of the year each 
                      threshold was reached (or its variability) over the 
                      30-year period. We excluded any locations where the 
                      threshold was reached in only one of the 30 years."
                    ),
                    br(),
                    p(strong("Disclaimer:"),
                      "Actual AGDD values and threshold dates may differ from 
                      those presented here due to local environmental 
                      conditions. Activities or decisions based on these data 
                      should be buffered accordingly.")
          )
        )
    ),
    width = "25%"
  ),
  
  layout_column_wrap(
    width = 1/2,
    plot_card(textOutput(outputId = "doy_header"), leafletOutput("doy")),
    plot_card(textOutput(outputId = "anom_header"), leafletOutput("anom")),
    plot_card("Average day of year", leafletOutput("mean")),
    plot_card("Variability associated with average conditions", leafletOutput("sd")),
    gap = 0
  )
)

# server ----------------------------------------------------------------------#

server <- function(input, output, session) {
  
  threshold <- reactiveVal(0)
  
  observeEvent(input$method, {
    reset("pest")
    reset("event")
    reset("type")
    reset("plot")
  })
  
  v <- reactiveValues(
    anom_header = "Deviation from average",
    doy_header = "Current day of year"
  )
  output$anom_header <- renderText({v$anom_header})
  output$doy_header <- renderText({v$doy_header})
  
  # Create correct inputs based on method
  output$selections <- renderUI({
    if (input$method == "Threshold") {
      load_threshold_selection()
    } else {
      load_pest_selection()
    }
  })
  
  # List inputs/boxes that will appear when "Threshold" method is selected
  load_threshold_selection <- function() {
    output <- tagList()
    output[[1]] <- selectInput(inputId = "threshold", 
                               label = "AGDD threshold:", 
                               choices = c(unique(threshold_list), ""),
                               selected = "")
    output[[2]] <- actionButton(inputId = "plot",
                                label = "Plot",
                                class = "btn btn-primary",
                                disabled = TRUE)
    output
  }
  
  # List inputs/boxes that will appear when "Pest" method is selected
  load_pest_selection <- function() {
    output <- tagList()
    output[[1]] <- selectInput(inputId = "pest",
                               label = "Species",
                               choices = unique(pests$spp),
                               selected = "")
    output[[2]] <- selectInput("event", "Biological event", choices = NULL)
    output[[3]] <- layout_columns(
      col_widths = c(5, 7),
      actionButton(inputId = "show_threshold",
                   label = "Update threshold",
                   class = "btn btn-primary btn-sm",
                   style = "margin-top: 5px;"),
      div(class = "custom-threshold-box",
          style = "display: flex; align-items: center; justify-content: space-between; 
                   border: 1px solid #dee2e6; border-radius: 0.25rem; padding: 0.5rem 1rem;
                   background-color: #f8f9fa; min-height: 60px;",
          div(style = "font-size: 0.90rem; color: #6c757d; font-weight: 500;", 
              "Selected threshold"),
          div(style = "font-size: 1.25rem; font-weight: 500;",
              uiOutput("selected"))
      )
    )
    output[[4]] <- actionButton(inputId = "plot",
                                label = "Plot", 
                                class = "btn btn-primary",
                                disabled = TRUE)
    output  
  }
  
  # Select threshold directly
  observeEvent(input$threshold, {
    threshold(input$threshold)
    if (input$threshold != "") {
      updateActionButton(input = "plot", disabled = FALSE)
    }
  })
  
  # When selecting a new pest, create choices for Event input and clear other
  # type dropdown and selected threshold box
  observeEvent(input$pest, {
    event_choices <- pests %>% 
      filter(spp %in% c(input$pest, "")) %>%
      pull(event)
    updateSelectInput(inputId = "event", choices = event_choices, selected = "")
    output$selected <- renderText({""})
    updateSelectInput(inputId = "type", choices = "")
    updateActionButton(input = "plot", disabled = TRUE)
  })

  # Clear selected threshold if changed threshold type 
  observeEvent(input$event, {
    output$selected <- renderText({""})
    updateActionButton(input = "plot", disabled = TRUE)
  })
  
  # Identify selected threshold if going with pest options
  observeEvent(input$show_threshold, {
    req(input$pest, input$event)
    selected <- pests %>% 
      filter(spp == input$pest, event == input$event) %>%
      pull(threshold)
    output$selected <- renderText({selected})
    updateActionButton(inputId = "plot", disabled = FALSE)
    threshold(selected)
  })
  
  # Initialize base maps 
  output$anom <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
      addTiles() %>%
      htmlwidgets::onRender("
        function(el, x) {
          const observer = new MutationObserver(function(mutations) {
            mutations.forEach(function(mutation) {
              if (mutation.addedNodes.length > 0) {
                var labels = el.querySelectorAll('.info.legend text');
                if (labels.length > 0) {
                  labels.forEach(function(label) {
                    label.setAttribute('text-anchor', 'start');
                    label.setAttribute('dx', '5');
                  });
                }
              }
            });
          });
          observer.observe(el, { childList: true, subtree: true });
          
          // Add click handler for synchronized queries
          el.addEventListener('click', function(e) {
            var map = this;
            if (map._leaflet_map) {
              var latlng = map._leaflet_map.mouseEventToLatLng(e);
              Shiny.setInputValue('anom_click', {
                lat: latlng.lat,
                lng: latlng.lng,
                nonce: Math.random()
              });
            }
          });
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
                var labels = el.querySelectorAll('.info.legend text');
                if (labels.length > 0) {
                  labels.forEach(function(label) {
                    label.setAttribute('text-anchor', 'start');
                    label.setAttribute('dx', '5');
                  });
                }
              }
            });
          });
          observer.observe(el, { childList: true, subtree: true });
          
          // Add click handler for synchronized queries
          el.addEventListener('click', function(e) {
            var map = this;
            if (map._leaflet_map) {
              var latlng = map._leaflet_map.mouseEventToLatLng(e);
              Shiny.setInputValue('doy_click', {
                lat: latlng.lat,
                lng: latlng.lng,
                nonce: Math.random()
              });
            }
          });
        }
      ")
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
                var labels = el.querySelectorAll('.info.legend text');
                if (labels.length > 0) {
                  labels.forEach(function(label) {
                    label.setAttribute('text-anchor', 'start');
                    label.setAttribute('dx', '5');
                  });
                }
              }
            });
          });
          observer.observe(el, { childList: true, subtree: true });
          
          // Add click handler for synchronized queries
          el.addEventListener('click', function(e) {
            var map = this;
            if (map._leaflet_map) {
              var latlng = map._leaflet_map.mouseEventToLatLng(e);
              Shiny.setInputValue('mean_click', {
                lat: latlng.lat,
                lng: latlng.lng,
                nonce: Math.random()
              });
            }
          });
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
                var labels = el.querySelectorAll('.info.legend text');
                if (labels.length > 0) {
                  labels.forEach(function(label) {
                    label.setAttribute('text-anchor', 'start');
                    label.setAttribute('dx', '5');
                  });
                }
              }
            });
          });
          observer.observe(el, { childList: true, subtree: true });
          
          // Add click handler for synchronized queries
          el.addEventListener('click', function(e) {
            var map = this;
            if (map._leaflet_map) {
              var latlng = map._leaflet_map.mouseEventToLatLng(e);
              Shiny.setInputValue('sd_click', {
                lat: latlng.lat,
                lng: latlng.lng,
                nonce: Math.random()
              });
            }
          });
        }
      ")
  })
  
  # Store current rasters for querying
  currentRasters <- reactiveValues(
    anom = NULL,
    doy = NULL,
    mean = NULL,
    sd = NULL
  )
  
  # Helper function to get raster value at coordinates
  getRasterValue <- function(raster, lat, lng) {
    if (is.null(raster)) return(NULL)
    tryCatch({
      pt <- sp::SpatialPoints(cbind(lng, lat), proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
      pt_transformed <- sp::spTransform(pt, raster::crs(raster))
      value <- raster::extract(raster, pt_transformed)
      if (!is.na(value) && !is.null(value)) {
        round(value, 2)
      } else {
        NULL
      }
    }, error = function(e) NULL)
  }
  
  # Create reactive values to store click coordinates
  clickCoords <- reactiveVal(NULL)
  
  # Observe clicks on any map and update all
  observeEvent(input$anom_click, {
    clickCoords(list(lat = input$anom_click$lat, lng = input$anom_click$lng))
  })
  
  observeEvent(input$doy_click, {
    clickCoords(list(lat = input$doy_click$lat, lng = input$doy_click$lng))
  })
  
  observeEvent(input$mean_click, {
    clickCoords(list(lat = input$mean_click$lat, lng = input$mean_click$lng))
  })
  
  observeEvent(input$sd_click, {
    clickCoords(list(lat = input$sd_click$lat, lng = input$sd_click$lng))
  })
  
  # Update all map displays when coordinates change
  observeEvent(clickCoords(), {
    req(clickCoords())
    coords <- clickCoords()
    
    # Get values from all rasters
    anom_val <- getRasterValue(currentRasters$anom, coords$lat, coords$lng)
    doy_val <- getRasterValue(currentRasters$doy, coords$lat, coords$lng)
    mean_val <- getRasterValue(currentRasters$mean, coords$lat, coords$lng)
    sd_val <- getRasterValue(currentRasters$sd, coords$lat, coords$lng)
    
    # Update each map with custom HTML control showing the value
    # Use removeControl with layerId instead of clearControls to preserve legends
    # If value is NULL, remove the control entirely
    if (!is.null(anom_val)) {
      leafletProxy("anom") %>%
        removeControl(layerId = "query_result") %>%
        addControl(
          html = paste0("<div style='background: white; padding: 5px; font-size: 14px;'>", 
                        "Anomaly, in days (negative = early; positive = late): ",
                        anom_val, "</div>"),
          position = "bottomleft",
          layerId = "query_result"
        )
    } else {
      leafletProxy("anom") %>%
        removeControl(layerId = "query_result")
    }
    
    if (!is.null(doy_val)) {
      leafletProxy("doy") %>%
        removeControl(layerId = "query_result") %>%
        addControl(
          html = paste0("<div style='background: white; padding: 5px; font-size: 14px;'>", 
                        "Current day of year: ", doy_val, "</div>"),
          position = "bottomleft",
          layerId = "query_result"
        )
    } else {
      leafletProxy("doy") %>%
        removeControl(layerId = "query_result")
    }
    
    if (!is.null(mean_val)) {
      leafletProxy("mean") %>%
        removeControl(layerId = "query_result") %>%
        addControl(
          html = paste0("<div style='background: white; padding: 5px; font-size: 14px;'>", 
                        "Average day of year: ", mean_val, "</div>"),
          position = "bottomleft",
          layerId = "query_result"
        )
    } else {
      leafletProxy("mean") %>%
        removeControl(layerId = "query_result")
    }
    
    if (!is.null(sd_val)) {
      leafletProxy("sd") %>%
        removeControl(layerId = "query_result") %>%
        addControl(
          html = paste0("<div style='background: white; padding: 5px; font-size: 14px;'>", 
                        "Standard deviation, in days: ", sd_val, "</div>"),
          position = "bottomleft",
          layerId = "query_result"
        )
    } else {
      leafletProxy("sd") %>%
        removeControl(layerId = "query_result")
    }
  })
  
  # Execute after clicking Plot button
  observeEvent(input$plot, {
    
    reacAnom <- anoms[[paste0("t", threshold())]]
    reacDOY <- doys[[paste0("t", threshold())]]    
    reacMean <- means[[paste0("t", threshold())]]
    reacSD <- sds[[paste0("t", threshold())]]
    
    # Store rasters for querying
    currentRasters$anom <- reacAnom
    currentRasters$doy <- reacDOY
    currentRasters$mean <- reacMean
    currentRasters$sd <- reacSD
    
    # Determine whether any locations have met threshold this year
    reacDOY_NA <- ifelse(ncell(reacDOY) == freq(reacDOY, value = NA), 
                         TRUE, FALSE)
    
    if (reacDOY_NA == TRUE) {
      v$anom_header <- "Deviation from average - NO LOCATIONS REACHED THRESHOLD YET"
      v$doy_header <- "Current day of year - NO LOCATIONS REACHED THRESHOLD YET"
      currentRasters$anom <- NULL
      currentRasters$doy <- NULL
    } else {
      v$anom_header <- "Deviation from average"
      v$doy_header <- "Current day of year"
    } 

    # Create color scale for each DOY and SD maps
    pal_mean <- colorNumeric(palette = "viridis",
                             domain = c(values(reacMean), values(reacDOY)),
                             na.color = "transparent",
                             reverse = TRUE)
    pal_sd <- colorNumeric(palette = "plasma",
                           domain = values(reacSD),
                           na.color = "transparent",
                           reverse = TRUE)

    # Add rasters to each base map
    leafletProxy("mean") %>%
      clearImages() %>%
      clearControls() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
      addRasterImage(reacMean,
                     colors = pal_mean,
                     group = "Average day of year",
                     layerId = "Average day of year",
                     opacity = input$opacity,
                     project = FALSE) %>%
      addLegend("bottomright",
                pal = pal_mean,
                values = values(reacMean),
                labFormat = myLabelFormat(dates = TRUE),
                title = NULL,
                opacity = 0.8) %>%
      addLeafletsync(c("anom", "doy", "mean", "sd"))
    
    leafletProxy("sd") %>%
      clearImages() %>%
      clearControls() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
      addRasterImage(reacSD, 
                     colors = pal_sd,
                     group = "Standard deviation, in days",
                     layerId = "Standard deviation, in days",
                     opacity = input$opacity, 
                     project = FALSE) %>%
      addLegend("bottomright", 
                pal = pal_sd, 
                values = values(reacSD),
                labFormat = myLabelFormat(dates = FALSE),
                title = "Standard deviation<br>(days)", 
                opacity = 0.8) %>%
      addLeafletsync(c("anom", "doy", "mean", "sd"))

    if (!reacDOY_NA) {
      leafletProxy("doy") %>%
        clearImages() %>%
        clearControls() %>%
        fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
        addRasterImage(reacDOY, 
                       colors = pal_mean, 
                       group = "Current day of year",
                       layerId = "Current day of year",
                       opacity = input$opacity, 
                       project = FALSE) %>%
        addLegend("bottomright", 
                  pal = pal_mean, 
                  values = c(values(reacMean), values(reacDOY)),
                  labFormat = myLabelFormat(dates = TRUE),
                  title = NULL, 
                  opacity = 0.8) %>%
        addLeafletsync(c("anom", "doy", "mean", "sd"))
    } else {
      leafletProxy("doy") %>%
        clearImages() %>%
        clearControls() %>%
        fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
        addLeafletsync(c("anom", "doy", "mean", "sd"))
    }
    
    if (!reacDOY_NA) {
      color_limit <- max(abs(range(values(reacAnom), na.rm = TRUE)))
      pal_anom <- colorNumeric(palette = "RdYlBu",
                               domain = c(-color_limit, color_limit),
                               na.color = "transparent")
      leafletProxy("anom") %>%
        clearImages() %>%
        clearControls() %>%
        fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
        addRasterImage(reacAnom, 
                       colors = pal_anom, 
                       group = "Anomaly, in days (negative = early; positive = late)",
                       layerId = "Anomaly, in days (negative = early; positive = late)",
                       opacity = input$opacity, 
                       project = FALSE) %>%
        addLegend("bottomright", 
                  pal = pal_anom, 
                  values = values(reacAnom),
                  labFormat = myLabelFormat(anomalies = TRUE),
                  title = NULL, 
                  opacity = 0.8) %>%
        addLeafletsync(c("anom", "doy", "mean", "sd"))
    } else {
      leafletProxy("anom") %>%
        clearImages() %>%
        clearControls() %>%
        fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
        addLeafletsync(c("anom", "doy", "mean", "sd"))
    }

  }) # end observe
} # end server

# run app ---------------------------------------------------------------------#

shinyApp(ui = ui, server = server)
