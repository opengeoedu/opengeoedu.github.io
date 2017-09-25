library(leaflet)

createMap <- function(portale) {
  categories <- c("international","national","regional","kommunal")
  colorlf <- c("green", "yellow", "blue", "brown")
  names(colorlf) <- categories
  
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
    )
  
  
  
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
    
    m <<-
      addCircleMarkers(
        m,
        group$lon,
        group$lat,
        popup = group$popup,
        group = category,
        color =  colorlf[[category]],
        label = htmlEscape(group$Titel),
      #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
      clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), removeOutsideVisibleBounds = FALSE),
        clusterId = category,
        labelOptions = labelOptions(noHide = TRUE)#, className = "needAbsolute",offset= c(-8, -8))
      )
    # m <<- addCircleMarkers(m, group$lon, group$lat, popup = group$popup, group = category, color =  colormarker[[category]], label = group$Titel)
    #m <<- addAwesomeMarkers(m, group$lon, group$lat, popup = group$popup, group = category, label = group$Titel)
    
    invisible()
  })
  
  m <-
    addLayersControl(m,
                     overlayGroups = levels(portale$Bezug),
                     options = layersControlOptions())
  return(m)
}