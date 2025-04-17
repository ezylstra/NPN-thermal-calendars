# App to display rasters with mean day-of-year (or SD) that AGDD thresholds are
# reached in the northeastern US - accordion maps w/ sidebar

# ER Zylstra
# 15 April 2025

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
  # library(shinycssloaders) 
  # Wanted to use withSpinner() to have busy indicators when raster images
  # load, but it doesn't seem to work with leafletProxy()

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

# Load anomaly rasters
anoms <- pin_read(board, "ezylstra/anomalies")
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

# Modify function used to format legend labels so we can add dates
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

# ui --------------------------------------------------------------------------#

ui <- page_sidebar(
  
  useShinyjs(),
  
  title = "What day of the year do locations in the northeastern U.S. reach 
             accumulated heat thresholds?",
  
  sidebar = sidebar(
    navset_tab(
      nav_panel("Settings",
                HTML("<br>"),
                radioButtons("method", "Selection method",
                             choices = c("Threshold" = "Threshold", "Pest" = "Pest"),
                             inline = TRUE),
                uiOutput("selections"),
                HTML("<br><br>"),
                sliderInput(inputId = "opacity",
                            label = "Opacity:",
                            min = 0,
                            max = 1, 
                            value = 0.8)
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
      )
    ),
    width = "27%"
  ),
  
  accordion(
    id = "accordion",
    open = TRUE,
    # open = c(HTML("<b>Current year anomaly</b>")),
    accordion_panel(
      title = HTML("<b>Current year anomaly</b>"),
      value = "panel_anom",
      leafletOutput("anom")
    ),
    accordion_panel(
      title = HTML("<b>Current day of year</b>"),
      value = "panel_doy",
      leafletOutput("doy")
    ),
    accordion_panel(
      title = HTML("<b>Mean day of year</b>"),
      value = "panel_mean",
      leafletOutput("mean")
    ),
    accordion_panel(
      title = HTML("<b>Standard deviation</b>"),
      value = "panel_sd",
      leafletOutput("sd")
    )
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
                               choices = unique(threshold_list))
    output[[2]] <- actionButton(inputId = "plot",
                                label = "Plot", 
                                class = "btn btn-primary")
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
    
    if (input$method == "Threshold") {
      threshold(input$threshold)
    }
    reacAnom <- anoms[[paste0("t", threshold())]]
    reacDOY <- doys[[paste0("t", threshold())]]    
    reacMean <- means[[paste0("t", threshold())]]
    reacSD <- sds[[paste0("t", threshold())]]
    
    # Determine whether any locations have met threshold this year
    reacDOY_NA <- ifelse(ncell(reacDOY) == freq(reacDOY, value = NA), 
                         TRUE, FALSE)
    
    if (reacDOY_NA == TRUE) {
      accordion_panel_update(id = "accordion", 
                             target = "panel_anom",
                             title = HTML("<b>Current year anomaly - NO LOCATIONS REACHED THRESHOLD THIS YEAR</b>"))
      accordion_panel_update(id = "accordion", 
                             target = "panel_doy",
                             title = HTML("<b>Current day of year - NO LOCATIONS REACHED THRESHOLD THIS YEAR</b>"))
    } else {
      accordion_panel_update(id = "accordion", 
                             target = "panel_anom",
                             title = HTML("<b>Current year anomaly</b>"))
      accordion_panel_update(id = "accordion", 
                             target = "panel_doy",
                             title = HTML("<b>Current day of year</b>"))
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
                title = "Day of year",
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
                    project = TRUE) %>%
      addLeafletsync(c("anom", "doy", "mean", "sd")) 

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
                      project = TRUE) %>%
        addLeafletsync(c("anom", "doy", "mean", "sd"))
    } else {
      leafletProxy("doy") %>%
        clearImages() %>%
        clearControls() %>%
        addLeafletsync(c("anom", "doy", "mean", "sd"))
    }
    
    if (!reacDOY_NA) {
      color_limit <- max(abs(range(values(reacAnom), na.rm = TRUE)))
      pal_anom <- colorNumeric(palette = "Spectral",
                               domain = c(-color_limit, color_limit),
                               na.color = "transparent")
      leafletProxy("anom") %>%
        clearImages() %>%
        clearControls() %>%
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
                  title = "Anomaly", 
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
        addLeafletsync(c("anom", "doy", "mean", "sd"))
    }
    accordion_panel_close(id = "accordion", 
                          values = c("panel_doy", "panel_mean", "panel_sd"))
    
  }) # end observe
  
  # Wanted to use this so that we could start with just the first panel open,
  # but using these commands messes up the zoom sync. 
    # outputOptions(output, "doy", suspendWhenHidden = FALSE)
    # outputOptions(output, "mean", suspendWhenHidden = FALSE)
    # outputOptions(output, "sd", suspendWhenHidden = FALSE)
  
}) # end server

# run app ---------------------------------------------------------------------#

shinyApp(ui = ui, server = server)

