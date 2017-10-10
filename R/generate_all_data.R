library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(htmlwidgets)

source("R/data_processing.R")

source("R/createTable.R")

rmarkdown::render("R/portals_dashboard.Rmd", output_dir = "out")

source("R/create_map_function.R")

source("R/generate_leaflet_map.R")

m
