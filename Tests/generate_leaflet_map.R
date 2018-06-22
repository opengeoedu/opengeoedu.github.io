if(FALSE){
  install.packages("devtools")
  devtools::install_github("hrbrmstr/nominatim")
  install.packages("leafletR")
  install.packages("ggmap")
  install.packages("leaflet")
  install.packages("rgdal")
  install.packages("htmltools")
  devtools::install_github("rstudio/crosstalk")
  devtools::install_github("rstudio/DT")
  #system dependencies libssl-dev  libcurl4-openssl-dev
}


#----------------------------------
# leaflet
#----------------------------------
library(leaflet)
library(htmltools)
library(htmlwidgets)


portale <- read.csv("out_geodata/portale_shifted.csv")
portale$Link <- paste0("<a href=\"",htmlEscape(portale$URL),"\" target=\"_blank\">",htmlEscape(portale$Titel),"</a>")
portale$popup <- paste0("<a href=\"",htmlEscape(portale$URL),"\" target=\"_blank\">",htmlEscape(portale$Titel),"</a><br>", htmlEscape(portale$Beschreibung),"<p><b>Ort:&nbsp;</b>", htmlEscape(portale$Ort),"</p>")
portale$label = htmlEscape(paste(portale$Titel, "|", portale$Ort))
portale$GDI <- portale$Typ == "gdi"

portale$Typ <- as.factor(portale$GeodatDI)
levels(portale$Typ) <- c("Open Data Portal","GDI")

leaflet(portale) %>% addMarkers()

source("R/create_map_function.R")
m <- createMap(portale)
setwd("leaflet")
saveWidget(m, file="portale_clustered.html",  selfcontained = FALSE)
setwd("..")

