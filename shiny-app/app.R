# App to display AGDD threshold rasters (current DOY & anomaly; 30-year mean
# DOY & SD)

# ER Zylstra
# 5 May 2025

library(dplyr)
library(lubridate)
library(stringr)
library(raster)
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(leafem)
library(htmlwidgets)
library(bslib)
library(shinyjs)
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
doys <- pin_read(board, "ezylstra/current-doys-prism")
doys <- terra::unwrap(doys)
# Convert to RasterBrick
doys <- raster::brick(doys)
names(doys) <- stringr::str_replace_all(names(doys), "X", "t")

# Load anomaly rasters
anoms <- pin_read(board, "ezylstra/anomalies-prism")
anoms <- terra::unwrap(anoms)
# Convert to RasterBrick
anoms <- raster::brick(anoms)
names(anoms) <- stringr::str_replace_all(names(anoms), "X", "t")

# List of thresholds
threshold_list <- seq(50, 2500, by = 50)

# Load file with pest-specific thresholds
pests <- read.csv("pest-thresholds-subset.csv")
pests_empty <- data.frame(spp = "", event = "", type = "", threshold = NA)
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
                    }"
    )
  ),

  title = h4("What day of the year do locations in the northeastern U.S. reach
             accumulated heat thresholds?"),
  
  sidebar = sidebar(
    div(style = "font-size:90%",
    navset_tab(
      nav_panel("Settings",
                br(),
                p("Begin by selecting whether to choose an AGDD threshold 
                  directly or indirectly by identifying a pest species. After 
                  selecting the options of interest (and clicking “Update 
                  threshold” if selecting a pest species), click the Plot button 
                  to view maps. Clicking on a map will provide the value at that 
                  location in the lower left corner of the map."),
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
                p("This website displays maps indicating when locations in the 
                  Northeastern U.S. reach accumulated growing degree day (AGDD) 
                  thresholds, both in the current calendar year and averaged over 
                  a recent 30-year period (1991-2020). Maps are provided for 50 
                  thresholds in 50 degree-day increments, from 50 AGDD to 2500 
                  AGDD. Thresholds can be selected in the Settings tab by 
                  selecting a AGDD threshold directly or by selecting a pest 
                  species, biological event, and threshold type.",
                  br(),
                  br(),
                  "For each threshold, four maps are displayed:"),
                tags$ul(
                  tags$li(strong("Anomaly: "), 
                          "Number of days earlier or later the threshold has
                          been reached this year compared to the average day
                          the threshold was reached in 1991-2020"),
                  tags$li(strong("Current day of year: "), 
                          "The day of year the threshold was reached this 
                          calendar year"),
                  tags$li(strong("Mean day of year: "),
                          "The average day of year the threshold was reached 
                          between 1991 and 2020."),
                  tags$li(strong("Standard deviation: "),
                          "The standard deviation associated with the average 
                          day of year the threshold was reached between 1991 and 
                          2020.")
                ),
                br(),
                p("This project was supported by the Northeast Climate Adapation
                  Science Center, the USA National Phenology Network, the
                  University of Arizona, and the U.S. Forest Service."),
                div(
                  style = "text-align: center;",
                  img(src = "NECASClogo.png", height = "75%", width = "75%")
                ),
                div(
                  style = "display: inline-flex; justify-content: space-evenly; align-items: center;",
                  img(src = "NPNlogo.png", height = "50%", width = "50%"),
                  img(src = "UAlogo.png", height = "25%", width = "25%")
                ),
                br(),
                br(),
                p("The code to run this website is available at ... For more 
                  information contact...")
      ),
      nav_panel("Methods",
                br(),
                p(strong("Mean day of year and standard deviation (SD) maps: "),
                  "For each year from 1991-2020, we obtained daily minimum and 
                  maximum temperatures for the Northeastern U.S. at 4-km resolution 
                  from the PRISM Climate Group (Oregon State University, ",
                  a(href = "https://prism.oregonstate.edu", 
                    "https://prism.oregonstate.edu", .noWS = "after"),
                  "). We used these data to calculate AGDD using the 
                  Baskerville-Emin method with a base temperature of 50 deg F, 
                  and identified the day of the year that each threshold was 
                  reached. The maps display the mean day of the year each 
                  threshold was reached (or the standard deviation, SD) over the 
                  30-year period. We excluded any locations where the threshold 
                  was reached in only of the 30 years."),
                p(strong("Current day of year and anomaly maps: "),
                  "We obtained daily minimum and maximum temperatures for the 
                  Northeastern U.S. for the current calendar year through 
                  yesterday from PRISM. We used the same methods described above 
                  to calculate daily AGDD values and identify the day of the year 
                  that each threshold was reached. To calculate anomalies, we 
                  subtracted the current day of year a threshold was reached 
                  from the long-term average; negative values indicate a threshold 
                  was reached earlier than normal and positive values indicate a 
                  threshold was reached later than normal.")
      ),
    )
    ),
    width = "25%"
  ),
    
  layout_column_wrap(
    width = 1/2,
    plot_card(textOutput(outputId = "anom_header"), leafletOutput("anom")),
    plot_card(textOutput(outputId = "doy_header"), leafletOutput("doy")),
    plot_card("Mean day of year", leafletOutput("mean")),
    plot_card("Standard deviation", leafletOutput("sd")),
    gap = 0
  )
)

# server ----------------------------------------------------------------------#

server <- shinyServer(function(input, output, session) {
  
  threshold <- reactiveVal(0)
  
  observeEvent(input$method, {
    reset("pest")
    reset("event")
    reset("type")
    reset("plot")
  })
  
  v <- reactiveValues(
    anom_header = "Anomaly",
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
    output[[3]] <- selectInput("type", "Threshold type", choices = NULL)
    output[[4]] <- actionButton(inputId = "show_threshold",
                                label = "Update threshold",
                                class = "btn btn-primary btn-sm")
    output[[5]] <- p("")
    output[[6]] <- value_box(title = "Selected threshold",
                             value = uiOutput("selected"),
                             theme = "text-blue",
                             max_height = "100px")
    output[[7]] <- actionButton(inputId = "plot",
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
  
  # Create choices for Type input
  observeEvent(input$event, {
    type_choices <- pests %>% 
      filter(spp %in% c(input$pest, ""), 
             event %in% c(input$event, "")) %>%
      pull(type)
    updateSelectInput(inputId = "type", choices = type_choices, selected = "")
    output$selected <- renderText({""})
    updateActionButton(input = "plot", disabled = TRUE)
  })
  
  # Clear selected threshold if changed threshold type 
  observeEvent(input$type, {
    output$selected <- renderText({""})
    updateActionButton(input = "plot", disabled = TRUE)
  })
  
  # Identify selected threshold if going with pest options
  observeEvent(input$show_threshold, {
    req(input$pest, input$event, input$type)
    selected <- pests %>% 
      filter(spp == input$pest, event == input$event, type == input$type) %>%
      pull(threshold)
    output$selected <- renderText({selected})
    updateActionButton(inputId = "plot", disabled = FALSE)
    threshold(selected)
  })
  
  # Create base maps
  output$anom <- renderLeaflet({
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
  
  # Execute after clicking Plot button
  observeEvent(input$plot, {
    
    reacAnom <- anoms[[paste0("t", threshold())]]
    reacDOY <- doys[[paste0("t", threshold())]]    
    reacMean <- means[[paste0("t", threshold())]]
    reacSD <- sds[[paste0("t", threshold())]]
    
    # Determine whether any locations have met threshold this year
    reacDOY_NA <- ifelse(ncell(reacDOY) == freq(reacDOY, value = NA), 
                         TRUE, FALSE)
    
    if (reacDOY_NA == TRUE) {
      v$anom_header <- "Anomaly - NO LOCATIONS REACHED THRESHOLD YET"
      v$doy_header <- "Current day of year - NO LOCATIONS REACHED THRESHOLD YET"
    } else {
      v$anom_header <- "Anomaly"
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
                     group = "Mean DOY",
                     layerId = "Mean DOY",
                     opacity = input$opacity,
                     project = FALSE) %>%
      addLegend("bottomright",
                pal = pal_mean,
                values = values(reacMean),
                labFormat = myLabelFormat(dates = TRUE),
                title = NULL,
                opacity = 0.8) %>%
      addImageQuery(reacMean,
                    digits = 2,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "Mean DOY",
                    project = TRUE) %>%
      addLeafletsync(c("anom", "doy", "mean", "sd"))
    
    leafletProxy("sd") %>%
      clearImages() %>%
      clearControls() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
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
                title = NULL, 
                opacity = 0.8) %>%
      addImageQuery(reacSD,
                    digits = 2,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "SD",
                    project = TRUE) %>%
      addLeafletsync(c("anom", "doy", "mean", "sd"))

    if (!reacDOY_NA) {
      leafletProxy("doy") %>%
        clearImages() %>%
        clearControls() %>%
        fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
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
                  title = NULL, 
                  opacity = 0.8) %>%
        addImageQuery(reacDOY,
                      digits = 2,
                      type = "click",
                      position = "bottomleft",
                      prefix = "",
                      layerId = "Current DOY",
                      project = TRUE) %>%
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
                       group = "Anomaly (negative = early; positive = late)",
                       layerId = "Anomaly (negative = early; positive = late)",
                       opacity = input$opacity, 
                       project = FALSE) %>%
        addLegend("bottomright", 
                  pal = pal_anom, 
                  values = values(reacAnom),
                  labFormat = myLabelFormat(anomalies = TRUE),
                  title = NULL, 
                  opacity = 0.8) %>%
        addImageQuery(reacAnom,
                      digits = 2,
                      type = "click",
                      position = "bottomleft",
                      prefix = "",
                      layerId = "Anomaly (negative = early; positive = late)",
                      project = TRUE) %>%
        addLeafletsync(c("anom", "doy", "mean", "sd"))
    } else {
      leafletProxy("anom") %>%
        clearImages() %>%
        clearControls() %>%
        fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
        addLeafletsync(c("anom", "doy", "mean", "sd"))
    }

  }) # end observe
}) # end server

# run app ---------------------------------------------------------------------#

shinyApp(ui = ui, server = server)

