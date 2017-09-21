if(FALSE){
  install.packages("devtools")
  devtools::install_github("hrbrmstr/nominatim")
  install.packages("leafletR")
  install.packages("ggmap")
  install.packages("leaflet")
  install.packages("rgdal")
  install.packages("htmltools")
  devtools::install_github("rstudio/crosstalk")
  #system dependencies libssl-dev  libcurl4-openssl-dev
}


#----------------------------------
# leaflet
#----------------------------------
library(leaflet)
library(htmltools)


portale <- read.csv("data/portale_geocoded2.csv")
portale$Link <- paste0("<a href=\"",htmlEscape(portale$URL),"\" target=\"_blank\">",htmlEscape(portale$Titel),"</a>")
portale$popup <- paste0("<a href=\"",htmlEscape(portale$URL),"\" target=\"_blank\">",htmlEscape(portale$Titel),"</a><br>", htmlEscape(portale$Beschreibung))
source("R/create_map_function.R")
m <- createMap(portale)
setwd("leaflet")
saveWidget(m, file="portale_clustered.html",  selfcontained = FALSE)
setwd("..")

