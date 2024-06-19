{
library(shiny)
library(tidyverse)
library(leaflet) # for interactive map
library(jsonlite) # FOr reading URLS
library(purrr) # for MAP
library(igraph) # for network analysis
} 
#require(plotly)

#### Loading data- precomputing


base_url <- "https://www.web-of-life.es/"

json_url <- paste0(base_url,"get_networks.php") 

all_nws <- jsonlite::fromJSON(json_url)
# This holds also the country/long/lat information
all_nw_info <- read.csv(paste0(base_url,"get_network_info.php")) # |> select relevant columns

all_nw_names <- distinct(all_nws, network_name)

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
      checkboxGroupInput("select_net_type", "Select Network Type:",
                   choices = c("All",unique(markers$network_type)),
                   selected = "All"),
      uiOutput("select_country")
      #selectInput("select_country", "Select Country :",
       #           choices = c("All",unique(markers$country))),

    ),
    mainPanel(
      leafletOutput("mymap", height = 400),
      br(),  # Add space between the map and tabs
      tabsetPanel(
        tabPanel("Network Metrics", dataTableOutput('table')),
        tabPanel("Network Plotting", plotOutput("graph"))
      )
    )
  ),
)




server <- function(input, output, session) {
  
  
  output$select_country <- renderUI({
    if ("All" %in% input$select_net_type | is.null(input$select_net_type)){
      
      avail_countries<-unique(markers$country)
      
    } else {
      
      avail_countries<- markers |> filter(network_type %in% input$select_net_type) |> select(country) |> unique()
    }
    
      selectInput("select_country", "Select Country :",
                  choices = c("All",avail_countries))
    
  })
  
  map_input <- reactive({
    
    map_df<-markers # Here we can add filters and stuff later...
    
    colorpalette <- colorFactor("viridis", levels = unique(map_df$network_type))
    
    
    if ("All" %in% input$select_net_type | is.null(input$select_net_type)){
      print( "All network types selected")
    } else {
      map_df <- map_df |> filter(network_type %in% input$select_net_type)
    }
    
    if ("All" == input$select_country | is.null(input$select_country)){
      starting_point<-data.frame("lat"=57.469696,
                                 "lng"=18.487759,
                                 "zoom"=1)
        
    } else {
      map_df <- map_df |> filter(country == input$select_country)
      starting_point<-map_df |> select(lng,lat) |> summarise_all(mean) |> mutate(zoom=2)
      
    }
    
    
    
    return(list(
      markers=map_df,
      colorpalette=colorpalette,
      starting_point=starting_point))
    
  })
  
  
  # This is for 
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
  
  output$table<-renderDataTable({
    
    netName<-revals$netID
  
net_info <- read.csv(paste0(base_url,paste("get_species_info.php?network_name=",netName, sep="")))
    
    # General metrics
    
    plants<-net_info|> filter(role=="Plant")|> nrow()
    animals <- net_info|> filter(role=="Pollinator")|> nrow()
    size <- plants*animals
    richness <- plants+animals
    links <- net|> nrow()
    conn <- round(links/size, 3)
    
netMetrics <- data.frame(netName, size, conn, plants, animals, richness)
    
names(netMetrics) <- c("Network", "Size", "Connectance", "Amount of plants species", "Amount of pollinator species", "Total amount of species")
    
netMetrics
    })
  
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
      setView(lat=map_data$starting_point$lat,
              lng=map_data$starting_point$lng,
              zoom=map_data$starting_point$zoom) |> 
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


