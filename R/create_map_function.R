library(leaflet)
library(leaflet.extras)

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
    addResetMapButton()
    
  
  
  
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
        #options = markerOptions(alt = group$searchmeta),
      #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
      clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), removeOutsideVisibleBounds = FALSE),
        clusterId = category,
        labelOptions = labelOptions(noHide = FALSE)#, className = "needAbsolute",offset= c(-8, -8))
      )
    # m <<- addCircleMarkers(m, group$lon, group$lat, popup = group$popup, group = category, color =  colormarker[[category]], label = group$Titel)
    #m <<- addAwesomeMarkers(m, group$lon, group$lat, popup = group$popup, group = category, label = group$Titel)
    
    invisible()
  })
  
  m <-
    addLayersControl(m,
                     overlayGroups = levels(portale$Bezug),
                     options = layersControlOptions())
  
  m <- addSearchFeatures(m, targetGroups = levels(portale$Bezug), options = searchFeaturesOptions(openPopup = TRUE, zoom = 8, propertyName = "title"))
  return(m)
}