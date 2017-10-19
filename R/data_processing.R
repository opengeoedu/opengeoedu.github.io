FETCH_WEBMETA = FALSE

if(FALSE){
  install.packages("devtools")
  devtools::install_github("hrbrmstr/nominatim")
  install.packages("leafletR")
  install.packages("ggmap")
  #install.packages("leaflet")
  devtools::install_github("rstudio/leaflet")
  install.packages("rgdal")
  install.packages("htmltools")
  devtools::install_github("rstudio/crosstalk")
  devtools::install_github('bhaskarvk/leaflet.extras')
  #system dependencies libssl-dev  libcurl4-openssl-dev
}


portale <- read.csv("data/portale_geocoded2.csv", as.is = TRUE)
library(ggmap)

#dp geocoding if coordinates are missing
for(i in 1:dim(portale)[1]){
  if(is.null(portale[i,]$lat) || is.na(portale[i,]$lat) || !is.numeric(portale[i,]$lat)){
    loc <- geocode(paste0(portale[i,]$Ort,", ", portale[i,]$Land));loc
    portale[i,]$lat <- loc$lat
    portale[i,]$lon <- loc$lon
  }
}


#-------------------------------------------
# Optionally fetch web meta information (actually not very effective)
#-------------------------------------------
library(xml2)

if(FETCH_WEBMETA){
  webmeta <- data.frame()
  
  sapply(portale$URL, function(url) {
    cat("Get metadata from url: ",url,"\n")
    tree <- read_html(url)
    node <- xml_find_all(tree, ".//meta[@name='description']/@content")
    meta_description <- xml_text(node)
    meta_description <- paste(meta_description)
    meta_description <- paste(meta_description, collapse = " - ")
    if(length(meta_description) == 0){
      node <- xml_find_all(tree, ".//meta[@name='abstract']/@content")
      meta_description <- xml_text(node)
      meta_description <- paste(meta_description)
      meta_description <- paste(meta_description, collapse = " - ")
      
      if(length(meta_description) == 0)
        meta_description <- ""
    }
    
    node <- xml_find_all(tree, ".//meta[@name='author']/@content")
    meta_author <- xml_text(node)
    if(length(meta_author) == 0)
      meta_author <- ""
    meta_author <- paste(meta_author)
    meta_author <- paste(meta_author, collapse = " - ")
    node <- xml_find_all(tree, ".//title")
    html_title <- xml_text(node)
    if(length(html_title) == 0)
      html_title <- ""
    html_title <- paste(html_title)
    html_title <- paste(html_title, collapse = " - ")
    row <- data.frame(html_title = html_title, meta_description = meta_description, meta_author = meta_author, stringsAsFactors = FALSE)
    webmeta <<- rbind(webmeta, row)
    invisible()
  })
  
  portale2 <- portale
  portale2[, c("html_title", "meta_description", "meta_author")] <- webmeta
  write.csv(portale2, file = "data/portale_geocoded_webmeta.csv", row.names = FALSE)
  
  
  for(i in 1:dim(portale)[1]){
    if(portale[i,]$Beschreibung == "" || length(portale[i,]$Beschreibung) == 0){
      portale[i,]$Beschreibung <- portale2[i,]$meta_description
    }
  }
}

#------------------------------

#produce output in various formats
#--------------------
#portale <- read.csv("data/portale_geocoded2.csv")
library(rgdal)
library(sp)
library(ggmap)
library(geosphere)

portale.sp <- portale
coordinates(portale.sp) <- ~lon + lat
proj4string(portale.sp) <- CRS("+proj=longlat +datum=WGS84")
#plot(portale.sp)
writeOGR(portale.sp, dsn = "out_geodata/portale.geojson", layer = "portale", driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(portale.sp, dsn = "out_geodata/portale.shp", layer = "portale", driver = "ESRI Shapefile", overwrite_layer = TRUE)
write.csv(portale, file = "data/portale_geocoded2.csv", row.names = FALSE)

portale.gk3 <- spTransform(portale.sp, CRS("+init=epsg:31467")) #GK Zone 3
portale.gk3.shifted <- as.data.frame(portale.gk3)



#--------------
# Shift coordinates of overlapping points (re-positioning at an imaginary circle around the center)
#--------------
#rounded coordinates prcision
RDIST <- 10000
rcs <- round(coordinates(portale.gk3)/RDIST)*RDIST

#find identical coordinate pairs
rcpairs <- as.factor(paste(rcs[,"lat"], rcs[,"lon"]))
overlaps <- levels(rcpairs)[table(rcpairs) > 1]

sapply(overlaps, function(overlap){
  #find the ids of overlapping points
  point_ids <- which(rcpairs == overlap)
  x0 <- mean(coordinates(portale.gk3)[point_ids[1], 1]) # approximate center of the coordinates
  y0 <- mean(coordinates(portale.gk3)[point_ids[1], 2])
  #cat(x0, "- ",y0, "\n")
  n <- length(point_ids) #number of points to be shifted
  vec <- seq(from=0, by=2*pi/n, length.out = n)
  r <- RDIST/4
  xc <- c(x0+r*sin(vec))
  yc <- c(y0+r*cos(vec))
  portale.gk3.shifted[point_ids, c("lon","lat")] <<- data.frame(xc, yc)
  return(invisible())
})

coordinates(portale.gk3.shifted) <- ~lon + lat
proj4string(portale.gk3.shifted) <- CRS("+init=epsg:31467")

#plot(portale.gk3.shifted)

portale.lonlat.shifted <- spTransform(portale.gk3.shifted, CRS("+proj=longlat +datum=WGS84"))
write.csv(as.data.frame(portale.lonlat.shifted), "out_geodata/portale_shifted.csv", row.names = FALSE)
writeOGR(portale.lonlat.shifted, dsn = "out_geodata/portale_shifted.geojson", layer = "portale", driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(portale.lonlat.shifted, dsn = "out_geodata/portale_shifted.shp", layer = "portale", driver = "ESRI Shapefile", overwrite_layer = TRUE)
zip("portale_shifted-ESRI-Shapefile.zip", files = paste0("out_geodata/portale_shifted",c(".shp",".shx",".dbf", ".prj")))

