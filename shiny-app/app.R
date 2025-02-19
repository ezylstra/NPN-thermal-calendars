# App to display rasters with mean day-of-year (or SD) that AGDD thresholds are
# reached in the northeastern US
# E. Zylstra
# 18 Feb 2025

library(raster)
library(shiny)
library(leaflet)
library(leafem)

# Note: using raster instead of terra since terra doesn't seem to play nice with 
# leafem::addImageQuery()

tcdf <- read.csv("parameters.csv")
thresholds <- sort(unique(tcdf$threshold))

means_list <- list()
sds_list <- list()

for (i in 1:length(thresholds)) {
  means_list[[i]] <- raster(paste0("rasters/normals_mean_", 
                                   thresholds[i], ".tiff"))
  sds_list[[i]] <- raster(paste0("rasters/normals_sd_", 
                                 thresholds[i], ".tiff"))
}
means_rast <- raster::stack(means_list)
names(means_rast) <- paste0("mean_", thresholds)
means_rast[means_rast == Inf] <- NA

sds_rast <- raster::stack(sds_list)
names(sds_rast) <- paste0("sd_", thresholds)
sds_rast[sds_rast == Inf] <- NA

all_rast <- raster::stack(means_rast, sds_rast)
all_rast <- raster::brick(all_rast)
# all_rast <- projectRasterForLeaflet(all_rast, method = "bilinear")


ui <- fluidPage(
  
  titlePanel("Testing maps"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "threshold", 
                  label = "AGDD threshold (deg F):", 
                  choices = unique(tcdf$threshold)),
      
      selectInput(inputId = "summary",
                  label = "Summary statistic:",
                  choices = unique(tcdf$summary)),
      
      sliderInput(inputId = "opacity",
                  label = "Opacity:",
                  min = 0,
                  max = 1, 
                  value = 0.8)
      
    ), # end sidebarPanel
    
    mainPanel(
      
      leafletOutput("map", height = 700)
      
    ) # end mainPanel
  ) # end sidebarLayout
) # end fluidPage

server <- shinyServer(function(input, output) {
  
  reacRaster <- reactive({all_rast[[paste0(tolower(input$summary), 
                                           "_", input$threshold)]]})
  
  legend_title <- reactive({ifelse(input$summary == "SD",
                                   "SD (days)", "Day of year")})
  
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -88, lat1 = 35, lng2 = -65, lat2 = 47) %>%
      addTiles() 
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
                     project = TRUE) %>%
      addLegend("bottomright", pal = pal, 
                values = values(reacRaster()),
                title = legend_title(), opacity = 0.8) %>%
      addImageQuery(reacRaster(),
                    digits = 2,
                    type = "click",
                    position = "bottomleft",
                    prefix = "",
                    layerId = "Value",
                    project = TRUE)
  })
  
})

shinyApp(ui = ui, server = server)
