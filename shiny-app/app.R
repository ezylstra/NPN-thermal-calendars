# App to display rasters with mean day-of-year (or SD) that AGDD thresholds are
# reached in the northeastern US

# ER Zylstra
# 16 Mar 2025

library(lubridate)
library(raster)
library(shiny)
library(leaflet)
library(leafem)
library(htmlwidgets)

# Note: using raster instead of terra since terra doesn't seem to play nice with 
# leafem::addImageQuery()

# Load file that lists summary statistics and thresholds
params <- read.csv("parameters.csv")

# Load RasterBrick and name layers
all_rast <- brick("mean-sd-brick.tif")
lyr_names <- paste0(tolower(params$summary), "_", params$threshold)
names(all_rast) <- lyr_names
  # Imported raster brick was projected for Leaflet already:
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

ui <- fluidPage(
  
  titlePanel("What day of the year do locations in the northeastern U.S. reach 
             accumulated heat thresholds?"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "threshold", 
                  label = "AGDD threshold (deg F):", 
                  choices = unique(params$threshold)),
      
      selectInput(inputId = "summary",
                  label = "Summary statistic:",
                  choices = unique(params$summary)),
      
      sliderInput(inputId = "opacity",
                  label = "Opacity:",
                  min = 0,
                  max = 1, 
                  value = 0.8),
      
      p("Zoom in and click on the map to identify the mean day of the year (or 
        SD) for a specific location. Value will appear in the lower left corner
        of the map."),
      
      br(),
      
      p(tags$strong("METHODS:")),
      p("For each year from 1991-2020, we obtained daily minimum and maximum 
        temperatures for the northeastern US at 4-km resolution from the 
        PRISM Climate Group (Oregon State University, ",
        a(href = "https://prism.oregonstate.edu", 
          "https://prism.oregonstate.edu", .noWS = "after"),
        "). We used these data to calculate accumulated growing degree days 
        (AGDD) using the Baskerville-Emin method with a base temperature of 50 
        deg F, and identified the day of the year that each threshold was
        reached. The maps display the mean day of the year each threshold was
        reached (or the standard deviation, SD) over the 30-year period.")

    ), # end sidebarPanel
    
    mainPanel(
      
      leafletOutput("map", height = "80vh"),
      
      br(),
      
      p("The data and code behind this webpage are available at... (USA-NPN github)?")

    ) # end mainPanel
  ) # end sidebarLayout
) # end fluidPage

# server ----------------------------------------------------------------------#

server <- shinyServer(function(input, output) {
  
  reacRaster <- reactive({all_rast[[paste0(tolower(input$summary), 
                                           "_", input$threshold)]]})
  
  legend_title <- reactive({ifelse(input$summary == "SD",
                                   "SD (days)", "Day of year")})
  
  legend_labels <- reactive({ifelse(input$summary == "SD",
                                    myLabelFormat(dates = FALSE),
                                    myLabelFormat(dates = TRUE))})
  
  output$map <- renderLeaflet({
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
    pal <- colorNumeric(palette = "viridis", 
                        domain = values(reacRaster()),
                        na.color = "transparent",
                        reverse = TRUE)
    
    leafletProxy("map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(reacRaster(), 
                     colors = pal, 
                     group = "Value",
                     layerId = "Value",
                     opacity = input$opacity, 
                     project = FALSE) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = values(reacRaster()),
                labFormat = legend_labels(),
                # labFormat = myLabelFormat(dates = TRUE),
                title = legend_title(), 
                opacity = 0.8) %>%
      addImageQuery(reacRaster(),
                    digits = 2,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "Value",
                    project = TRUE)   

  }) # end observe
}) # end server

# run app ---------------------------------------------------------------------#

shinyApp(ui = ui, server = server)
