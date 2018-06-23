FETCH_WEBMETA = FALSE

if(!exists("EMAIL"))
  EMAIL = "" #pass email adress in order to use nominatim service of openstreetmap, 
# see terms of use: https://operations.osmfoundation.org/policies/nominatim/)
# more information: https://wiki.openstreetmap.org/wiki/Nominatim, http://nominatim.openstreetmap.org/
require(xml2)
require(htmltools)
require(stringr)

portale <- read.csv("data/portale_geocoded4.csv", as.is = TRUE)

#dp geocoding if coordinates are missing
for(i in 1:dim(portale)[1]){
  if(is.null(portale[i,]$lat) || is.na(portale[i,]$lat) || !is.numeric(portale[i,]$lat)){
    address <- portale$Adresse_Herausgeber[i]
    if(address != ""){
      #google geocoding

        ##for nomatim geocoding of open streetmap
        if((!exists("EMAIL") || is.null(EMAIL) || EMAIL == "") && requireNamespace("getPass")){
          EMAIL <- getPass::getPass("Please provide your e-mail adress for the nominatim-openstreetmap service:")
        }
        #address <- htmltools::htmlEscape(address, attribute = TRUE);address
        address <- stringr::str_replace_all(address, "ß","ss")
        #address <- "Europaplatz 1, A-7000 Eisenstadt" #for testing
       # address <- "Stadt Mannheim, Rathaus E 5, D-68159 Mannheim"
        address <- unlist(stringr::str_split(address,", "));address
        len <- length(address)
        if(len>1){
          
          city <- unlist(stringr::str_split(stringr::str_trim(address[len]),pattern = " "))
          if(length(city)>=2){
            city <- paste0("&city=",paste(city[2:length(city)], collapse = " "),"&postalcode=",city[1])
          }else{
            city <- stringr::str_trim(address[len])
          }
          query <- paste0("street=",address[len-1],city)
        }else{
          query <- paste0("q=",address)
        }
        .url <- paste0("http://nominatim.openstreetmap.org/search?",URLencode(query),"&format=xml&country_codes=de,ch,at,li&email=",EMAIL,"&country=",portale[i,]$Land)
        success <- TRUE
        desc <- NULL
        tryCatch({
          con <- url(.url)
          xres <- read_xml(con)
          xloc <- xml_find_first(xres, ".//place")
          lon <- xml_attr(xloc,"lon")
          lat <- xml_attr(xloc,"lat")
          message("Found coordinates ",lat,", ",lon," for address ", portale$Adresse_Herausgeber[i])
          if(!is.na(lon) && !is.na(lat)){
            portale$lat[i] <- as.numeric(lat)
            portale$lon[i] <- as.numeric(lon)
          }else{
            message("Request was not successfull:\n\t:",.url)
          }
            
        }, error = function(e){
          success <- FALSE
          print(e)
        },finally = {
          try(close(con), silent = TRUE)
          
        })
        if (!success) {
          warning("Failed determine geolocation of ", address)
        }
        Sys.sleep(2)# maximum 1 request per second alowed
    }
        
  }
      #--
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
library(geosphere)

if(!dir.exists("out_geodata"))
  dir.create("out_geodata")
portale.sp <- portale
coordinates(portale.sp) <- ~lon + lat
proj4string(portale.sp) <- CRS("+proj=longlat +datum=WGS84")
#plot(portale.sp)
writeOGR(portale.sp, dsn = "out_geodata/portale.geojson", layer = "portale", driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(portale.sp, dsn = "out_geodata/portale.shp", layer = "portale", driver = "ESRI Shapefile", overwrite_layer = TRUE)
write.csv(portale, file = "data/portale_geocoded4.csv", row.names = FALSE)

portale.gk3 <- spTransform(portale.sp, CRS("+init=epsg:31467")) #GK Zone 3
portale.gk3.shifted <- as.data.frame(portale.gk3)



#--------------
# Shift coordinates of overlapping points (re-positioning at an imaginary circle around the center)
#--------------
#rounded coordinates prcision
RDIST <- 20000
rcs <- round(coordinates(portale.gk3)/RDIST)*RDIST
#find identical coordinate pairs
rcpairs <- as.factor(paste(rcs[,"lat"], rcs[,"lon"]))
overlaps <- levels(rcpairs)[table(rcpairs) > 1]
point_id_groups <- sapply(overlaps, function(overlap){
  point_ids <- which(rcpairs == overlap)
  point_ids
})

rcs <- round((coordinates(portale.gk3)+(RDIST/2))/RDIST)*RDIST
rcpairs <- as.factor(paste(rcs[,"lat"], rcs[,"lon"]))
overlaps <- levels(rcpairs)[table(rcpairs) > 1]
all_point_ids <- unlist(point_id_groups,use.names = FALSE)

sapply(overlaps, function(overlap){
  point_ids <- which(rcpairs == overlap)
  id_group <- point_ids[!point_ids %in% all_point_ids]
  if(length(id_group)>1){
  }
    return(invisible())
})

#point_id_groups


sapply(point_id_groups, function(point_ids){
  #point_ids <- point_id_groups[[1]]
  point_ids <- unlist(point_ids)
  #find the ids of overlapping points
  #point_ids <- which(rcpairs == overlap)
  x0 <- mean(coordinates(portale.gk3)[point_ids, 1]) # approximate center of the coordinates
  y0 <- mean(coordinates(portale.gk3)[point_ids, 2])
  #cat(x0, "- ",y0, "\n")
  
  n <- length(point_ids) #number of points to be shifted
  vec <- 2*pi/(3*n)+seq(from=0, by=2*pi/n, length.out = n)
  r <- max(1, n/3)*RDIST/5
  xc <- c(x0+r*sin(vec))
  yc <- c(y0+r*cos(vec))
  
  pold <- coordinates(portale.gk3)[point_ids,]#shifted_points[point_ids, c(1,2)] 
  arcs <- mapply(function(xp, yp){
    arc <- atan2(xp-x0, yp-y0)*360/(2*pi)
    if(arc<0)
      arc <- arc + 360
    # text(xp,yp, labels = round(arc))
    arc
  },  xp=pold[,1],
  yp=pold[,2]
  )
  portale.gk3.shifted[point_ids[order(arcs)], c("lon","lat")] <<- data.frame(xc, yc)
  return(invisible())
})

coordinates(portale.gk3.shifted) <- ~lon + lat
proj4string(portale.gk3.shifted) <- CRS("+init=epsg:31467")

#plot(portale.gk3.shifted)

portale.lonlat.shifted <- spTransform(portale.gk3.shifted, CRS("+proj=longlat +datum=WGS84"))
write.csv(as.data.frame(portale.lonlat.shifted), "out_geodata/portale_shifted.csv", row.names = FALSE)
writeOGR(portale.lonlat.shifted, dsn = "out_geodata/portale_shifted.geojson", layer = "portale", driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(portale.lonlat.shifted, dsn = "out_geodata/portale_shifted.shp", layer = "portale", driver = "ESRI Shapefile", overwrite_layer = TRUE)
#dest <- file.path(tempdir(),"portale_shifted.sql")
#writeOGR(portale.lonlat.shifted, dsn = dest, layer = "portale", driver = "PostgreSQL", overwrite_layer = TRUE)
#file.copy(dest, file.path(getwd(),"out_geodata/portale_shifted.sql"),overwrite = TRUE)
#file.remove(dest)
writeOGR(portale.lonlat.shifted, dsn = "out_geodata/portale_shifted.kml", layer = "portale", driver = "KML", overwrite_layer = TRUE)
writeOGR(portale.lonlat.shifted, dsn = "out_geodata/portale_shifted.gml", layer = "portale", driver = "GML", overwrite_layer = TRUE)
## workaround for execution problems (create file in temporary directory and then copy)
dest <- file.path(tempdir(),"portale_shifted.gpkg")
writeOGR(portale.lonlat.shifted, dsn =  dest, layer = "portale", driver = "GPKG", overwrite_layer = TRUE,delete_dsn = TRUE)
file.copy(dest, file.path(getwd(),"out_geodata/portale_shifted.gpkg"),overwrite = TRUE)
file.remove(dest)
#writeOGR(portale.lonlat.shifted, dsn = "ou/portale_shifted.gpkg", layer = "portale", driver = "GPKG", overwrite_layer = TRUE)
zip("out_geodata/portale_shifted-ESRI-Shapefile.zip", files = paste0("out_geodata/portale_shifted",c(".shp",".shx",".dbf", ".prj")))
#--------------------------
# portals map data
#

#national, regional, local boundaries
if(!file.exists("data/auxiliary.RData")){
    require(rgdal)
    require(rgeos)
    library(rmapshaper)
    
    g4bounds <- readOGR("data/bounds/Germany_AL4.GeoJson")
    g5bounds <- readOGR("data/bounds/Germany_AL5.GeoJson")
    #g5bounds <- SpatialPolygonsDataFrame(data = g5bounds@data, Sr = gSimplify(g5bounds,tol= 0.02,topologyPreserve = TRUE))
    #g5bounds <- SpatialPolygonsDataFrame(data = g5bounds@data, Sr = ms_simplify(g5bounds))
    
    g6bounds <- readOGR("data/bounds/Germany_AL6.GeoJson")
    #g6bounds <- SpatialPolygonsDataFrame(data = g6bounds@data, Sr = ms_simplify(g6bounds))

    a4bounds <- readOGR("data/bounds/Austria_AL4.GeoJson")
    a6bounds <- readOGR("data/bounds/Austria_AL6.GeoJson")
    #a6bounds <- SpatialPolygonsDataFrame(data = a6bounds@data, Sr = ms_simplify(a6bounds))
   
    s4bounds <- readOGR("data/bounds/Switzerland_AL4.GeoJson")
    s5bounds <- readOGR("data/bounds/Switzerland_AL5.GeoJson")
    #s5bounds <- SpatialPolygonsDataFrame(data = s5bounds@data, Sr = ms_simplify(s5bounds))
    s6bounds <- readOGR("data/bounds/Switzerland_AL6.GeoJson")
    #s6bounds <- SpatialPolygonsDataFrame(data = s6bounds@data, Sr = ms_simplify(s6bounds))
    b_objs <- ls()[stringr::str_detect(ls(), "bounds$")]
    sapply(b_objs, function(.boundary){
      obj <- get(.boundary)
      assign(.boundary, SpatialPolygonsDataFrame(data = obj@data, Sr = ms_simplify(obj, keep = 0.015, keep_shapes=TRUE)), inherits = TRUE,)
      return(invisible())
    })
    g2bounds <- readOGR("data/bounds/Germany_AL2.GeoJson")
    g2bounds <- SpatialPolygonsDataFrame(data = g2bounds@data, Sr = gUnaryUnion(g4bounds, id = sapply(slot(g2bounds, "polygons"), function(x) slot(x, "ID"))))
    
    .geom <- gUnaryUnion(g4bounds)
    .data <- g2bounds@data
    
    
    noId <-function(x) {
      row.names(x) <- NULL
      return(x)
    }
    g2bounds <- SpatialPolygonsDataFrame(data = noId(g2bounds@data), Sr = gUnaryUnion(g4bounds))
    a2bounds <- readOGR("data/bounds/Austria_AL2.GeoJson")
    a2bounds <- SpatialPolygonsDataFrame(data = noId(a2bounds@data), gUnaryUnion(a4bounds))
    s2bounds <- readOGR("data/bounds/Switzerland_AL2.GeoJson")
    s2bounds <- SpatialPolygonsDataFrame(data = noId(s2bounds@data), gUnaryUnion(s4bounds))
    
    save(file = "data/auxiliary.RData", list = ls()[stringr::str_detect(ls(), "bounds$")])
}

if(FALSE){
  require(rgdal)
  library(rmapshaper)
  g6bounds <- readOGR("data/bounds/Germany_AL4.GeoJson")
  g6bounds <- SpatialPolygonsDataFrame(data = g6bounds@data, Sr = ms_simplify(g6bounds))
  g6bounds$val <- round(runif(16,0,100))
  writeOGR(g6bounds,dsn = "data/bounds/Germany_AL4_simple",layer = "germany_al4" ,driver="GeoJSON")
}
