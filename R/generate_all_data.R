if(FALSE){
  #install necessary packages
  install.packages("devtools") #install packages from github
#  install.packages("leafletR") #(optional) leaflet package
  #install.packages("leaflet")
  install.packages("rgdal")
  install.packages("rgeos")
  install.packages("geosphere")
  install.packages("htmltools")
  install.packages("htmlwidgets")
  install.packages("digest")

  devtools::install_github("rstudio/leaflet") #another leaflet package
  devtools::install_github("rstudio/shiny")
  #devtools::install_github("schloerke/leaflet",ref = "barret/v1x")
  devtools::install_github("rstudio/rmarkdown")
  devtools::install_github("rstudio/crosstalk")
  devtools::install_github("rstudio/DT")
  devtools::install_github("rstudio/flexdashboard")
  devtools::install_github('bhaskarvk/leaflet.extras')
  install.packages("rmarkdown")
  install.packages("xml2")
  install.packages("shinyjs")
  
  install.packages("jsonlite")
  install.packages(c("rtable", "ReportRs"))
  #system dependencies: libssl-dev  libcurl4-openssl-dev  pandoc
}

library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(htmlwidgets)

source("R/data_processing.R")

source("R/generate_stats.R")

source("R/portal_prerendering.R")
# cache the R objects that are consumed by the shiny application:
save("m", "portale","sd", "sd_table","table_meta","statistics_html", "country_json", "pchIcons","addAllPortalMarkers","addPortalMarker",file = "out/prerendered_content.RData")

# in interactive sessions test-render the map
if(interactive()) {
  rmarkdown::run("portals_dasboard_shiny.Rmd")
}
#source("R/createTable.R")

#if pandoc fails with error in RStudio, just run again...
#rmarkdown::render("R/portals_dashboard.Rmd", output_dir = "out")

#source("R/create_map_function.R")

#source("R/generate_leaflet_map.R")

m
