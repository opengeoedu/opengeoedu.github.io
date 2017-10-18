library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(crosstalk)


# helper function that creates triangular icon files in differnet colors in folder tempicon
# use point symbols from base R graphics as icons
pchIcons <- function(col, width = 35, height = 35, pch = 24, file_prefix="gdi-icon-",...) {
  #see https://github.com/mylesmharrison/colorRampPaletteAlpha
  addalpha <- function(colors, alpha=1.0) {
    r <- col2rgb(colors, alpha=T)
    # Apply alpha
    r[4,] <- alpha*255
    r <- r/255.0
    return(rgb(r[1,], r[2,], r[3,], r[4,]))
  }
  
  n = length(col)
  files = character(n)
  # create a sequence of png images
  
  for (i in seq_len(n)) {
    #f = tempfile(tmpdir = "icontemp", fileext = '.png')
    col_transp <- addalpha(col[i], 0.3)
    f <- paste0("icontemp/",file_prefix,col[i],".png")
    if(!dir.exists("icontemp")){
      cat("Created directory 'icontemp' in order to store icon files. You can remove this folder manually after the output was created: \n\t",path.expand("./icontemp"))
      dir.create("icontemp")
    }
    file.create(f)
    png(f, width = width, height = height, bg = 'transparent')
    par(mar = c(0, 0, 0, 0))
    plot.new()
    #points(.5, .5, pch = 17, cex = min(width, height) / 8, ..., col=col)
    points(.5, .5, pch = pch, cex = (min(width, height) / 8) -1, ..., col=col[i], bg=col_transp, lwd=5)
    dev.off()
    files[i] = f
  }
  files
}

group_gdi <- NULL
group_nogdi <- NULL

createMap <- function(portale, crosstalk_group = "portale") {
  categories <- c("international","national","regional","kommunal")
  colorlf <- c("green", "yellow", "blue", "brown")
  names(colorlf) <- categories
  #portale$searchmeta <- paste(portale$Titel, portale$Ort, sep = " | ")

  portale_shared <- SharedData$new(portale, group = "portale")
  
  m <-
    leaflet(data = portale_shared, options = list(preferCanvas = TRUE))  %>% 
  #  addProviderTiles(providers$Stamen.TonerBackground) %>% 
    #addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addLegend(
      colors = colorlf,
      values = categories,
      labels = categories,
      title = "Open Data Portale"
    ) %>%
    addResetMapButton() %>%
    
    addControl(paste0("<img src=\"/",pchIcons(col = "grey"), "\"></img><b>GDI</b>"),position = "topright")
  
 
    
  sapply(categories, function(category) {
    group <- portale[portale$Bezug == category,]
    group$label = htmlEscape(paste(group$Titel, "|", group$Ort))
    cf <- paste0(
      "function (cluster) {
      var childCount = cluster.getChildCount();",
      "var c = '",
      colorlf[[category]],
      "';",
      "return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
  }"
    )
    
    iconfile <- pchIcons(colorlf[[category]])
    
    group_nogdi <<- SharedData$new(group[!group$GDI,], group = "portale")
    group_gdi <<- SharedData$new( group[group$GDI,], group = "p")
    
    if(dim(group_gdi$data())[1]>0)
      m <<-
        addMarkers(
          m,
          ~lon,
          ~lat,
          popup = ~popup,
          popupOptions = popupOptions(),
          group = category,
          icon =  ~ icons(
            iconUrl = iconfile,
            iconWidth = 30,
            iconHeight = 30
          ),
          #color =  colorlf[[category]],
          label = ~label,
          #options = markerOptions(alt = group$searchmeta),
          #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
          clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), removeOutsideVisibleBounds = FALSE),
          clusterId = category,
          labelOptions = labelOptions(noHide = FALSE),#, className = "needAbsolute",offset= c(-8, -8)),
          data =  group_gdi
        )
    
    if(dim(group_nogdi$data())[1]>0)
      m <<-
        addCircleMarkers(
          m,
          ~lon,
          ~lat,
          popup = ~popup,
          popupOptions = popupOptions(),
          group = category,
          color =  colorlf[[category]],
          label = ~label,
          #options = markerOptions(alt = group$searchmeta),
        #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
        clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), removeOutsideVisibleBounds = FALSE),
          clusterId = category,
          labelOptions = labelOptions(noHide = FALSE),#, className = "needAbsolute",offset= c(-8, -8)),
          data = group_nogdi
        )
      # m <<- addCircleMarkers(m, group$lon, group$lat, popup = group$popup, group = category, color =  colormarker[[category]], label = group$Titel)
      #m <<- addAwesomeMarkers(m, group$lon, group$lat, popup = group$popup, group = category, label = group$Titel)
       
       invisible()
     })
  
  m <-
    addLayersControl(m,
                     overlayGroups = levels(portale$Bezug),
                     options = layersControlOptions())
  
  
  ## some test functions
  # sdf <- function(text, x){
  #   print(paste("They called me with", text))
  #   return(c("1","2","3"))
  # }
  # 
  # sdf <- function(textSearch, allRecords){
  #   JS('alert("HEY!");')
  #   cat(paste("They called me with", textSearch))
  #   return(allRecords[1])
  # }
  
  m <- addSearchFeatures(m, targetGroups = levels(portale$Bezug), options = searchFeaturesOptions(openPopup = TRUE, propertyName = "label"))
  return(m)
}