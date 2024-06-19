### Script to pull data from the web page and work with

# Load required packages
require(tidyverse)
require(purrr) # part of tidyverse but for clarity
require(jsonlite) # for working with the database API
require(plotly)

install.packages("renv")
require(renv)

renv::init()
# 
## From https://www.web-of-life.es/ 
# Common url for all 
base_url <- "https://www.web-of-life.es/"


json_url <- paste0(base_url,"get_networks.php") 
all_nws <- jsonlite::fromJSON(json_url)





## Species info for a particular network..
#https://www.web-of-life.es/get_species_info.php
speci<-read.csv(paste0(base_url,"get_species_info.php?network_name=M_PL_073"))


## MEtadata for all networks
# This holds also the country/long/lat information
all_nw_info <- read.csv(paste0(base_url,"get_network_info.php")) # |> select relevant columns




ggplot(all_nw_info)

1
1
  
