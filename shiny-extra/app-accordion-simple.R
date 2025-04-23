# App to display AGDD threshold rasters (current DOY & anomaly; 30-year mean
# DOY & SD)

# Added in the different selection methods, and I get resizing issues in browser

# ER Zylstra
# 22 April 2025

library(lubridate)
library(raster)
library(shiny)
library(bslib)
library(leaflet)
library(leafem)
library(leaflet.extras2)
library(dplyr)

# Create raster brick (example)
A1 <- raster(xmn=-90, xmx=-75, ymn=40, ymx=47, resolution = 1)
A1 <- projectRaster(A1, crs = "epsg:3857")
A1 <- setValues(A1, sample(30:40, ncell(A1), replace = TRUE))
A2 <- setValues(A1, sample(35:45, ncell(A1), replace = TRUE))
A3 <- setValues(A1, sample(40:50, ncell(A1), replace = TRUE))
B1 <- setValues(A1, sample(1:5, ncell(A1), replace = TRUE))
B2 <- setValues(A1, sample(3:7, ncell(A1), replace = TRUE))
B3 <- setValues(A1, sample(6:10, ncell(A1), replace = TRUE))
C1 <- setValues(A1, sample(10:12, ncell(A1), replace = TRUE))
C2 <- setValues(A1, sample(12:14, ncell(A1), replace = TRUE))
C3 <- setValues(A1, sample(14:16, ncell(A1), replace = TRUE))
A_rasters <- brick(A1, A2, A3)
names(A_rasters) <- paste0("t", 1:3)
B_rasters <- brick(B1, B2, B3)
names(B_rasters) <- paste0("t", 1:3)
C_rasters <- brick(C1, C2, C3)
names(C_rasters) <- paste0("t", 1:3)

# Colors lookup
colors <- data.frame(color = c(rep(c("Blue", "Red", "Yellow"), each = 2), ""),
                     shape = c("Square", "Circle", "Triangle", "Square", 
                               "Rectangle", "Oval", ""),
                     threshold = c(1:3, 1:3, NA))

# ui --------------------------------------------------------------------------#

ui <- page_sidebar(

  title = "Title",
  
  tags$head(
    tags$style(HTML("
      .leaflet-container {
        width: 100%;
        height: 100%;
      }
      .accordion-body {
        min-height: 500px;
      }
    "))
  ),
  tags$head(
    tags$script(HTML("
      $(document).ready(function() {
        // Initialize ResizeObserver to monitor container size changes
        const resizeObserver = new ResizeObserver(entries => {
          for (let entry of entries) {
            // Find any Leaflet maps in the resized element
            const maps = entry.target.querySelectorAll('.leaflet-container');
            maps.forEach(map => {
              if (map._leaflet_id) {
                // Get the Leaflet map instance and invalidate its size
                const leafletMap = window.L.map[map._leaflet_id];
                if (leafletMap) {
                  leafletMap.invalidateSize();
                }
              }
            });
          }
        });
        
        // Observe accordion panels
        document.querySelectorAll('.accordion-item').forEach(panel => {
          resizeObserver.observe(panel);
        });
      });
    "))
  ),
  
  sidebar = sidebar(
    navset_tab(
      nav_panel("Settings",
                radioButtons("method", "Method",
                             choices = c("Threshold", "Color"), inline = TRUE),
                uiOutput("selections")
      )
    )
  ),

  accordion(
    id = "accordion",
    open = TRUE,
    accordion_panel(
      title = "TypeA",
      value = "panel_a",
      div(style = "height: 500px;", leafletOutput("a", height = "100%"))
    ),
    accordion_panel(
      title = "TypeB",
      value = "panel_b",
      div(style = "height: 500px;", leafletOutput("b", height = "100%"))
    ), 
    accordion_panel(
      title = "TypeC",
      value = "panel_c",
      div(style = "height: 500px;", leafletOutput("c", height = "100%"))
    )
  )
)

# server ----------------------------------------------------------------------#

server <- shinyServer(function(input, output, session) {
  
  threshold <- reactiveVal(0)

  # Create correct inputs based on method
  output$selections <- renderUI({
    if (input$method == "Threshold") {
      load_threshold_selection()
    } else {
      load_color_selection()
    }
  })
  
  # List inputs/boxes that will appear when "Threshold" method is selected
  load_threshold_selection <- function() {
    output <- tagList()
    output[[1]] <- selectInput(inputId = "threshold", 
                               label = "Threshold", 
                               choices = c("", 1:3),
                               selected = "")
    output[[2]] <- actionButton(inputId = "plot",
                                label = "Plot",
                                class = "btn btn-primary",
                                disabled = TRUE)
    output
  }
  
  # List inputs/boxes that will appear when "Color" method is selected
  load_color_selection <- function() {
    output <- tagList()
    output[[1]] <- selectInput(inputId = "color",
                               label = "Color",
                               choices = unique(colors$color),
                               selected = "")
    output[[2]] <- selectInput("shape", "Shape", choices = NULL)
    output[[3]] <- actionButton(inputId = "plot",
                                label = "Plot", 
                                class = "btn btn-primary",
                                disabled = TRUE)
    output  
  }
  
  # Select threshold
  observeEvent(input$threshold, {
    threshold(input$threshold)
    if (input$threshold != "") {
      updateActionButton(input = "plot", disabled = FALSE)
    }
  })

  # When selecting a new color, create choices for Shape input
  observeEvent(input$color, {
    shape_choices <- colors %>% 
      filter(color %in% c(input$color, "")) %>%
      pull(shape)
    updateSelectInput(inputId = "shape", choices = shape_choices, selected = "")
  })
  
  observeEvent(input$shape, {
    selected <- colors %>%
      filter(color == input$color, shape == input$shape) %>%
      pull(threshold)
    threshold(selected)
    updateActionButton(input = "plot", disabled = FALSE)
  })
  
  # Create base maps
  output$a <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 49) %>%
      addTiles()
  })
  output$b <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 49) %>%
      addTiles()
  })
  output$c <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 49) %>%
      addTiles()
  })

  observeEvent(input$accordion, {
    session$sendCustomMessage(type = "leaflet-invalidate-size", message = list(
      id = paste0(input$accordion, "")
    ))
  })
  
  # Execute after clicking Plot button
  observeEvent(input$plot, {
 
    reacA <- A_rasters[[paste0("t", threshold())]]
    reacB <- B_rasters[[paste0("t", threshold())]]
    reacC <- C_rasters[[paste0("t", threshold())]]
    
    pal_a <- colorNumeric(palette = "viridis", 
                          domain = values(reacA))
    pal_b <- colorNumeric(palette = "plasma",
                          domain = values(reacB))
    pal_c <- colorNumeric(palette = "inferno",
                          domain = values(reacC))

    # Add rasters to each base map
    leafletProxy("a") %>%
      clearImages() %>%
      clearControls() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 48) %>%
      addRasterImage(reacA,
                     colors = pal_a,
                     group = "a",
                     layerId = "a",
                     project = FALSE) %>%
      addLegend("bottomright",
                pal = pal_a,
                values = values(reacA),
                title = "Type A") %>%
      addImageQuery(reacA,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "a",
                    project = TRUE) %>%
      addLeafletsync(c("a", "b", "c"))
    
    leafletProxy("b") %>%
      clearImages() %>%
      clearControls() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 48) %>%
      addRasterImage(reacB,
                     colors = pal_b,
                     group = "b",
                     layerId = "b",
                     project = FALSE) %>%
      addLegend("bottomright",
                pal = pal_b,
                values = values(reacB),
                title = "Type B") %>%
      addImageQuery(reacB,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "b",
                    project = TRUE) %>%
      addLeafletsync(c("a", "b", "c"))
    
    leafletProxy("c") %>%
      clearImages() %>%
      clearControls() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 48) %>%
      addRasterImage(reacC,
                     colors = pal_c,
                     group = "c",
                     layerId = "c",
                     project = FALSE) %>%
      addLegend("bottomright",
                pal = pal_c,
                values = values(reacC),
                title = "Type C") %>%
      addImageQuery(reacC,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "c",
                    project = TRUE) %>%
      addLeafletsync(c("a", "b", "c"))

    # Close all panels except a
    accordion_panel_close(id = "accordion", values = c("panel_b", "panel_c"))

  }) # end observe
}) # end server

# run app ---------------------------------------------------------------------#

shinyApp(ui = ui, server = server)

