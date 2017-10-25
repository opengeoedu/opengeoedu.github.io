if(FALSE){
  #install necessary packages
  install.packages("devtools") #install packages from github
  devtools::install_github("hrbrmstr/nominatim") #(optional) geocoding with openstreetmap
  install.packages("leafletR") #(optional) leaflet package
  install.packages("ggmap") #use google maps and google geocoding
  #install.packages("leaflet")
  install.packages("rgdal")
  install.packages("htmltools")
  install.packages("htmlwidgets")

  devtools::install_github("rstudio/leaflet") #another leaflet package
  devtools::install_github("rstudio/crosstalk")
  devtools::install_github("rstudio/DT")
  devtools::install_github("rstudio/flexdashboard")
  devtools::install_github('bhaskarvk/leaflet.extras')
  install.packages("rmarkdown")
  #system dependencies: libssl-dev  libcurl4-openssl-dev  pandoc
}

library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(htmlwidgets)

source("R/data_processing.R")

source("R/createTable.R")

#if pandoc fails with error in RStudio, just run again...
rmarkdown::render("R/portals_dashboard.Rmd", output_dir = "out")

source("R/create_map_function.R")

source("R/generate_leaflet_map.R")

m
