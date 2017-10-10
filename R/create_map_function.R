library(leaflet)
library(leaflet.extras)
library(RColorBrewer)


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


createMap <- function(portale) {
  categories <- c("international","national","regional","kommunal")
  colorlf <- c("green", "yellow", "blue", "brown")
  names(colorlf) <- categories
  #portale$searchmeta <- paste(portale$Titel, portale$Ort, sep = " | ")
  
  m <-
    leaflet(data = portale, options = list(preferCanvas = TRUE))  %>% 
  #  addProviderTiles(providers$Stamen.TonerBackground) %>% 
    #addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
    addLegend(
      colors = colorlf,
      values = categories,
      labels = categories,
      title = "Open Data Portale"
    ) %>%
    addResetMapButton() %>%
    
    addControl(paste0("<img src=\"/",pchIcons(col = "grey"), "\"></img> Geodateninfrastruktur (GDI)</b>"),position = "topright")
  
 
    
  sapply(categories, function(category) {
    group <- portale[portale$Bezug == category,]
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
    
    group_nogdi <- group[group$GDI != "ja",]
    group <-  group[group$GDI == "ja",]
    m <<-
      addMarkers(
        m,
        group$lon,
        group$lat,
        popup = group$popup,
        popupOptions = popupOptions(),
        group = category,
        icon =  ~ icons(
          iconUrl = iconfile,
          iconWidth = 30,
          iconHeight = 30
        ),
        #color =  colorlf[[category]],
        label = htmlEscape(paste(group$Titel, "|", group$Ort)),
        #options = markerOptions(alt = group$searchmeta),
        #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
        clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), removeOutsideVisibleBounds = FALSE),
        clusterId = category,
        labelOptions = labelOptions(noHide = FALSE)#, className = "needAbsolute",offset= c(-8, -8)),
      )
    
    group <- group_nogdi
    
    m <<-
      addCircleMarkers(
        m,
        group$lon,
        group$lat,
        popup = group$popup,
        popupOptions = popupOptions(),
        group = category,
        color =  colorlf[[category]],
        label = htmlEscape(paste(group$Titel, "|", group$Ort)),
        #options = markerOptions(alt = group$searchmeta),
      #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
      clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), removeOutsideVisibleBounds = FALSE),
        clusterId = category,
        labelOptions = labelOptions(noHide = FALSE)#, className = "needAbsolute",offset= c(-8, -8)),
      )
    # m <<- addCircleMarkers(m, group$lon, group$lat, popup = group$popup, group = category, color =  colormarker[[category]], label = group$Titel)
    #m <<- addAwesomeMarkers(m, group$lon, group$lat, popup = group$popup, group = category, label = group$Titel)
     
     invisible()
   })
  
  m <-
    addLayersControl(m,
                     overlayGroups = levels(portale$Bezug),
                     options = layersControlOptions())
  
  sdf <- function(text, x){
    print(paste("They called me with", text))
    return(c("1","2","3"))
  }
  
  sdf <- function(textSearch, allRecords){
    JS('alert("HEY!");')
    cat(paste("They called me with", textSearch))
    return(allRecords[1])
  }
  
  m <- addSearchFeatures(m, targetGroups = levels(portale$Bezug), options = searchFeaturesOptions(openPopup = TRUE, propertyName = "label"))
  return(m)
}