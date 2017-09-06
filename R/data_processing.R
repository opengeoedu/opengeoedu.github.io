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
portale <- read.csv("data/portale_geocoded.csv")


library(ggmap)

#geocode data if coordinates are is missing:
for(i in 1:dim(portale)[1]){
  if(is.null(portale[i,]$lat) || is.na(portale[i,]$lat) || !is.numeric(portale[i,]$lat)){
    loc <- geocode(paste0(portale[i,]$Ort,", ", portale[i,]$Land));loc
    portale[i,]$lat <- loc$lat
    portale[i,]$lon <- loc$lon
  }
}

write.csv(portale, file = "portale_geocoded.csv")
#------------------------------

#produce output in differnent formates
#--------------------
portale <- read.csv("portale_geocoded.csv")
library(rgdal)
library(sp)
library(ggmap)

portale.sp <- portale
coordinates(portale.sp) <- ~lon + lat
#proj4string(portale.sp) <- CRS("+proj=longlat +datum=WGS84")
plot(portale.sp)
writeOGR(portale.sp, dsn = "out_geodata/portale.geojson", layer = "portale", driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(portale.sp, dsn = "out_geodata/portale.shp", layer = "portale", driver = "ESRI Shapefile", overwrite_layer = TRUE)
