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

#Column {data-width=650}
#Column {data-width=350}

#Statistik über die eingetragenen Datenportale:
library(rtable)
library(ReporteRs)
#library(flextable)
#blue table (matching the design color)
colorP <- colorRampPalette(colors = c("white","#009de0"))

#'old' blue table
#colorP <- colorRampPalette(colors = c("white","#044e96"))
#gray table
#colorP <- colorRampPalette(colors = c("white","gray"))
tab_colors <- colorP(10)

#selector for country-values that involve multiple values (not one of either austria, swiss or germany)
sel <- which(!(portale$Land %in% c("Deutschland", "Österreich", "Schweiz")))
# create simplified categories for the statistics
Land <- as.character(portale$Land)
Land[sel] <- "Sonstige"
Land <- factor(Land, levels = c("Deutschland", "Österreich","Schweiz", "Sonstige"), ordered = TRUE)
Typ <- portale$Typ_names
#levels(Typ) <- stringr::str_replace(string = levels(Typ), pattern = "GDI", replacement = "GDI / Geoportal")
ftab <- ftable(data.frame(Typ = Typ, Land = Land, Reichweite = portale$Reichweite))

#statistics per country:
country_stat <- paste0("(",paste(c("DE","AU","CH", "Sonst"),summary(Land), sep = ": ",collapse = ", "),")")

#table_font <- "Helvetica, Arial, Geneva, sans-serif"
options("ReporteRs-default-font"= "Helvetica, Arial, Geneva, sans-serif") 
#body.text.props = textProperties(font.family = table_font), header.text.props = textProperties(font.weight = "bold", font.family = table_font)
flext <- as.FlexTable(ftab) %>%
  addFooterRow(paste("Datenportale insgesamt:",dim(portale)[1], country_stat), colspan = 6) %>%
  setZebraStyle(even = tab_colors[2], odd = 'white' ) %>%
  setFlexTableBackgroundColors(colors = tab_colors[5],to = c("header")) %>%
  setFlexTableBackgroundColors(colors = tab_colors[5],to = c("footer"))

nfac <- length(levels(Land)) #number of categories for attribute land
#join rows of first column
for(i in seq(1,flext$numrow, by = nfac)){
  flext <- spanFlexTableRows(flext, j = "Typ", i, i+nfac-1 )
}

flext[,1, newpar = TRUE] <- paste0("(",sapply(summary(portale$Typ), function(x) rep(x,4)),")")

flext[,2] <- paste0(" (",as.numeric(rowSums(as.matrix(ftab))),")")

if(!dir.exists("out"))
  dir.create("out")

statdoc <- docx() %>%
  addTitle("Übersicht über das Open Data Suchportal", level = 1 ) %>%
  addFlexTable(flext) %>%
  addParagraph("Anzahl der Datenportale im Verzeichnis",stylename = "rTableLegend") %>%
  writeDoc(file="out/verzeichnis_statistik.docx")

#render statistics table
statistics_html <- as.html(flext)


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
