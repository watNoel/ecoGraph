library(shiny)
library(leaflet)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel(title = div(h2("Shiny App", align = "left"))),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select Continent:",
                  choices = c("Asia", "Europe", "North America", "South America", "Africa", "Oceania")),
      uiOutput("country_ui")
    ),
    mainPanel(
      leafletOutput("map", height = 400),
      br(),  # Add space between the map and tabs
      tabsetPanel(
        tabPanel("Iris Dataset", plotOutput("irisPlot")),
        tabPanel("Mtcars Dataset", plotOutput("mtcarsPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to update the list of countries based on the selected continent
  output$country_ui <- renderUI({
    req(input$continent)
    countries <- switch(input$continent,
                        "Asia" = c("China", "India", "Japan"),
                        "Europe" = c("Germany", "France", "Italy"),
                        "North America" = c("United States", "Canada", "Mexico"),
                        "South America" = c("Brazil", "Argentina", "Chile"),
                        "Africa" = c("Nigeria", "South Africa", "Egypt"),
                        "Oceania" = c("Australia", "New Zealand", "Fiji"))
    selectInput("country", "Select Country:", choices = countries)
  })
  
  # Coordinates for the countries
  country_coords <- data.frame(
    country = c("China", "India", "Japan", "Germany", "France", "Italy",
                "United States", "Canada", "Mexico", "Brazil", "Argentina", 
                "Chile", "Nigeria", "South Africa", "Egypt", "Australia", 
                "New Zealand", "Fiji"),
    lat = c(35.8617, 20.5937, 36.2048, 51.1657, 46.6034, 41.8719, 
            37.0902, 56.1304, 23.6345, -14.2350, -38.4161, -35.6751, 
            9.0820, -30.5595, 26.8206, -25.2744, -40.9006, -17.7134),
    lon = c(104.1954, 78.9629, 138.2529, 10.4515, 1.8883, 12.5674,
            -95.7129, -106.3468, -102.5528, -51.9253, -63.6167, -71.5429, 
            8.6753, 22.9375, 30.8025, 133.7751, 174.8850, 179.0123)
  )
  
  # Reactive expression for selected country coordinates
  selected_coords <- reactive({
    req(input$country)
    subset(country_coords, country == input$country)
  })
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # Observe changes in the selected country and update the map
  observe({
    req(selected_coords())
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = selected_coords()$lon, lat = selected_coords()$lat,
                 popup = selected_coords()$country)
  })
  
  # Plot for iris dataset
  output$irisPlot <- renderPlot({
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Iris Dataset Plot", x = "Sepal Length", y = "Sepal Width")
  })
  
  # Plot for mtcars dataset
  output$mtcarsPlot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg, color = factor(gear))) +
      geom_point() +
      theme_minimal() +
      labs(title = "Mtcars Dataset Plot", x = "Weight", y = "Miles per Gallon", color = "Gears")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
