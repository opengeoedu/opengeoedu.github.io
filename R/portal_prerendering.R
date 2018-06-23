

library(leaflet)
library(flexdashboard)
library(DT)
library(htmltools)
library(crosstalk)
requireNamespace("jsonlite")
library(sp)
library(rgeos)
library(shiny)


#portale <- read.csv("../data/portale_geocoded3.csv")
portale <- read.csv("out_geodata/portale_shifted.csv")

## metadata of column factors, names and colors for spatial range ("Reichweite")
table_meta <- jsonlite::read_json("data/table_meta.json", simplifyVector = TRUE)

portale$Link <- paste0("<a href=\"",htmlEscape(portale$URL),"\" target=\"_blank\">",htmlEscape(portale$Titel),"</a>")

portale$popup <- paste0("<a href=\"",htmlEscape(portale$URL),"\" target=\"_blank\">",htmlEscape(portale$Titel),"</a><br>", htmlEscape(portale$Beschreibung))
portale$Reichweite <- factor(portale$Reichweite, levels=table_meta$reichw, ordered = TRUE)
portale$label = htmlEscape(paste(portale$Titel, "|", portale$Ort))
portale$Typ <- factor(portale$Typ, levels=table_meta$typ, ordered = TRUE)
portale$Typ_names <- portale$Typ
levels(portale$Typ_names) <- table_meta$typ_names

sd <- SharedData$new(portale, group = "portale")
sd_table <- SharedData$new(portale[c("Link","Beschreibung","Ort","Reichweite")], group = "portale")

load("data/auxiliary.RData")
load("data/cities-geonames-deatch.RData")


source("R/create_map_function.R")


# render leaflet map
m <- createMap(portale,
               table_meta = table_meta,
               clustering = FALSE,
               layerControls = FALSE,
               polygon_fill_color = "#AFBF56",
               polygon_fill_opacity = 0.1)


## json map for country-wise selection (select box)

country_options <- list("Deutschland", "Liechtenstein", "Österreich", "Schweiz")
country_chr <- as.character(portale$Land)
country_map <- sapply(country_options, function(country){
  out <- list(which(stringr::str_detect(country_chr, country) | portale$Land == "Europa" | portale$Reichweite == "international"))
  names(out) <- country
  out
})
country_json <- list(
  items= list(value = country_options, label=country_options),
  map = country_map,
  group = "portale"
)

country_json <- as.character(jsonlite::toJSON(country_json, pretty = TRUE))
