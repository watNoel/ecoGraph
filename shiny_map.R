{
library(shiny)
library(tidyverse)
library(leaflet) # for interactive map

library(jsonlite) # FOr reading URLS
library(purrr) # for MAP
} 
#require(plotly)

#### Loading data- precomputing


base_url <- "https://www.web-of-life.es/"

json_url <- paste0(base_url,"get_networks.php") 

all_nws <- jsonlite::fromJSON(json_url)
# This holds also the country/long/lat information
all_nw_info <- read.csv(paste0(base_url,"get_network_info.php")) # |> select relevant columns

# Precompute the marker information for the map in the app.
markers<- all_nw_info |> 
  filter(! ( is.na(latitude) | is.na(longitude))) |> 
  rename(lat=latitude,lng=longitude) |> 
  mutate(labs=paste(sep = "<br/>",
                    paste0("<b>Network Name:</b> ",network_name),
                    paste0("<b>Network Type:</b>",network_type),
                    paste0("<b>Location:</b>", location),
                    paste0("<b> Long\\Lat: </b>",round(lng,3),"\\",round(lat,3))
  )
  ) |> 
  select(!c(location_address,cell_values_description,abundance_description))






ui <- fluidPage(
  titlePanel(title = div(h2("Shiny App", align = "left"))),
  sidebarLayout(
    sidebarPanel(
      selectInput("net_type", "Select Network Type:",
                   choices = unique(markers$network_type)),
      selectInput("select_country", "Select Country :",
                  choices = unique(markers$country)),
      uiOutput("ui_out")
    ),
    mainPanel(
      leafletOutput("mymap", height = 400),
      br(),  # Add space between the map and tabs
      tabsetPanel(
        tabPanel("Graph Panel", plotOutput("graph")),
        tabPanel("Network Barplot", textOutput("tOut"))
      )
    )
  ),
  
  
)




server <- function(input, output, session) {
  
  
  map_input <- reactive({
    
    map_df<-markers # Here we can add filters and stuff later...
    
    colorpalette <- colorFactor("viridis", levels = unique(map_df$network_type))
    
    
    
    return(list(
      markers=map_df,
      colorpalette=colorpalette))
    
  })
  
  
  
  revals<-reactiveValues(netID=NA)
  
  observeEvent(input$mymap_marker_click, {
    revals$netID <-input$mymap_marker_click$id
    print("Something")
    
    
  })
  
  
  
  #output$tOut <- renderText({
   # print(input$mymap_marker_click)
   # print(input$mymap_shape_click$id)
    
    #input$mymap_shape_click$id
    
 # })
  
  
  
  output$graph<-renderPlot({
    
    netName<-revals$netID
    print("Hey")

    net<-all_nws|> 
      filter(network_name==!!netName)|> 
      select(species1, species2, connection_strength)
    
    graph<-graph_from_data_frame(net, directed = T)
    
    
    species_info <- read.csv(paste0(base_url,
                                    paste("get_species_info.php?network_name=",netName, sep="")))
    
    isResource <- species_info$is.resource %>% as.logical() # 0/1 converted to FALSE/TRUE
    
    # Add the "type" attribute to the vertices of the graph 
    V(graph)$type <- !(isResource) 
    E(graph)$arrow.mode = "-"
    
    color1<-"lightblue"
    color2<-"yellow"
    
    V(graph)$color <- ifelse(V(graph)$type == TRUE, color1, color2)
    
    netLayout<-layout_as_bipartite(graph)
    
    plot(graph, 
         layout=netLayout, 
         arrow.mode=0,
         vertex.label=NA,
         vertex.size=4,
         asp=0.2)
  })
  
  
  
  
  output$mymap <- renderLeaflet({
    map_data <- map_input()
    leaflet(map_data$markers) |> 
      addTiles() |> 
      setView(lat=57.469696,lng=18.487759,zoom=1) |> 
      addCircleMarkers(color=~map_data$colorpalette(network_type),
                       popup  = ~labs,
                       label = ~network_name,
                       layerId = ~network_name # layerID allows to fetch
      ) |> 
      addLegend(
        position = "topright",
        pal = map_data$colorpalette, values = ~network_type,
        title = "Network Type",
        opacity = 1)
  })
  
}

shinyApp(ui, server)


