library(shiny.semantic)

ui <- semanticPage(
  
  h2("What day of the year do locations in the northeastern U.S. reach 
             accumulated heat thresholds?"),
  
  grid(
    grid_template = grid_template(
      default = list(
        areas = rbind(
          c("settings", "map1"),
          c("settings", "map2"),
          c("plot", "map2")
        ),
        cols_width = c("30%", "70%"),
        rows_height = c("50%", "35%", "15%")
      )
    ),  
    settings = card(
      navset_tab(
        nav_panel("Info"),
        nav_panel("Threshold"),
        nav_panel("Pest")
      )
    ),
    plot = card(
      actionButton(inputId = "plot",
                   label = "Plot")
    ),
    map1 = navset_card_tab(
      nav_panel("Card 1a"),
      nav_panel("Card 1b")
    ),
    map2 = card("Card 2")
  )
)

server <- shinyServer(function(input, output, session) {})

# run app ---------------------------------------------------------------------#

shinyApp(ui = ui, server = server)

