library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(crosstalk)
library(rgeos)


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

createMap <- function(portale, crosstalk_group = "portale", clustering = TRUE, layerControls = TRUE) {
  categories <- c("international","national","regional","kommunal")
  colorlf <- c("green", "#FFA500", "blue", "brown")
  names(colorlf) <- categories
  #portale$searchmeta <- paste(portale$Titel, portale$Ort, sep = " | ")

  portale_shared <- SharedData$new(portale, group = crosstalk_group)

  gdi_legend = paste0("<img src=\"/",pchIcons(col = "grey"), "\"></img> GDI")
  odp_legend = paste0("<img src=\"/",pchIcons(file_prefix = "portals_", col = "grey", pch = 21), "\"></img> Open Data Portale")

  labeladm6opts <- labelOptions(textOnly = TRUE, noHide = TRUE, direction = "bottom", opacity = 0.5, textsize = "10pt")
  labeladm4opts <- labelOptions(textOnly = TRUE, noHide = TRUE, direction = "bottom", opacity = 0.5, textsize = "13pt", style = "color:#03F")
  #labelPolyopts <- labelOptions(direction = "right", style = "color:yellow; text-shadow: 0 0 0.1em black, 0 0 0.1em black,
   #     0 0 0.1em black,0 0 0.1em black,0 0 0.1em;")
  m <-
    leaflet(data = portale_shared, options = list(preferCanvas = TRUE))  %>%
    #  addProviderTiles(providers$Stamen.TonerBackground) %>%
    #addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addLegend(
      colors = colorlf,
      values = categories,
      labels = categories,
      title = "Legende"
    ) %>%
    addResetMapButton() %>%
    addFullscreenControl() %>%
    addPolygons(data = g6bounds,color="#696969",weight = 1, group = "adm6",fill=TRUE,label = g6bounds$localname, fillOpacity = 0) %>%
    #addLabelOnlyMarkers(data = gCentroid(geometry(g6bounds), byid = TRUE), label = g6bounds$localname, group = "adm6_labels", labelOptions = labeladm6opts) %>%
    addPolygons(data = s6bounds,color="#696969",weight = 1, group  = "adm6",label = s6bounds$localname,fill=TRUE, fillOpacity = 0) %>%
    #addLabelOnlyMarkers(data = gCentroid(geometry(s6bounds), byid = TRUE), label = s6bounds$localname, group = "adm6_labels", labelOptions = labeladm6opts) %>%
    addPolygons(data = a6bounds,color="#696969",weight = 1,label = a6bounds$localname, group = "adm6",fill=TRUE, fillOpacity = 0) %>%
    #addLabelOnlyMarkers(data = gCentroid(geometry(a6bounds), byid = TRUE), label = a6bounds$localname, group = "adm6_labels", labelOptions = labeladm6opts) %>%
    addPolygons(data = g5bounds,color="#696969", weight = 1.5,label = g5bounds$localname, group = "adm5",fill=FALSE) %>%
    addPolygons(data = s5bounds,color="#696969",weight = 1.5,label = s5bounds$localname, group  = "adm5", fill=TRUE, fillOpacity = 0) %>%
    addPolygons(data = g4bounds,weight = 2, group = "adm4", fill=TRUE) %>%
    addPolygons(data = s4bounds,weight = 2, label = s4bounds$localname, group  = "adm4", fill=TRUE) %>%
    addPolygons(data = a4bounds,weight = 2, group = "adm4", fill=TRUE) %>%
    addLabelOnlyMarkers(data = gPointOnSurface(geometry(g4bounds), byid = TRUE), label = g4bounds$localname, group = "adm4_labels", labelOptions = labeladm4opts) %>%
    addLabelOnlyMarkers(data = gPointOnSurface(geometry(a4bounds), byid = TRUE), label = a4bounds$localname, group = "adm4_labels", labelOptions = labeladm4opts) %>%
    #addLabelOnlyMarkers(data = gCentroid(geometry(s4bounds), byid = TRUE), label = s4bounds$localname, group = "adm4_labels", labelOptions = labeladm4opts) %>%
    addPolygons(data = g2bounds,weight = 2, color= "black",group = "adm2", fill = FALSE) %>%
    addPolygons(data = s2bounds,weight = 2, color= "black",group  = "adm2", fill = FALSE) %>%
    addPolygons(data = a2bounds,weight = 2, color= "black",group = "adm2", fill = FALSE) %>%
    addControl(paste(gdi_legend, odp_legend, sep="<br/>\n"),position = "topright") %>%
    addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE, metric = TRUE)) %>%
    addLabelOnlyMarkers(data=pplc, label = pplc$name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, zoomAnimation = FALSE, textsize = "10pt")) %>%
    addLabelOnlyMarkers(data=ppla, label = ppla$name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, zoomAnimation = FALSE, textsize = "10pt"), group = "adm4_labels") %>%
    addLabelOnlyMarkers(data=ppl, label = ppl$name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, zoomAnimation = FALSE, textsize = "10pt"), group = "adm6_labels") %>%
    addCircleMarkers(data=pplc, color = "red",radius=1) %>%
    addCircleMarkers(data=ppla, color = "black", radius=1, group = "adm4_labels") %>%
    addCircleMarkers(data=ppl, color = "#696969",radius=1, group = "adm6_labels") %>%
    #addCircleMarkers(data=pplc, color = "red",radius=1, group = "adm2_labels", options = markerOptions(clickable = FALSE),label = pplc$name, labelOptions = labelOptions(noHide = TRUE, zoomAnimation = FALSE, textsize = 13, className = "ppl_label" )) %>%
    #addCircleMarkers(data=ppla, color = "black", radius=1, group = "adm4_labels", options = markerOptions(clickable = FALSE, zIndexOffset = -500), label = ppla$name, labelOptions = labelOptions(noHide = TRUE, zoomAnimation = FALSE, style = "z-index: -1")) %>%
    #addCircleMarkers(data=ppl, color = "#696969",radius=1, group = "adm6_labels", options = markerOptions(clickable = FALSE, zIndexOffset = -500), label = ppl$name, labelOptions = labelOptions(noHide = TRUE, zoomAnimation = FALSE, style = "z-index: -1")) %>%
    hideGroup("adm6") %>% hideGroup("adm5") %>% hideGroup("adm6_labels") %>% hideGroup("adm4_labels")  %>% 
    leaflet.extras::enableTileCaching()
      #addGeoJSON("data/bounds/Germany_AL2.GeoJson")


  

  sapply(categories, function(category) {
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

    group_nogdi <<- SharedData$new(portale[portale$Bezug==category & !portale$GDI,], group = crosstalk_group)
    group_gdi <<- SharedData$new(portale[portale$Bezug==category & portale$GDI,], group = crosstalk_group)

    clusterOptions <- NULL
    if(clustering)
      clusterOptions <- markerClusterOptions(iconCreateFunction = JS(cf), removeOutsideVisibleBounds = FALSE)

    if(dim(group_gdi$data())[1]>0)
      m <<-
      addMarkers(
        m,
        ~lon,
        ~lat,
        popup = ~popup,
        popupOptions = popupOptions(closeOnClick = TRUE),
        group = "portals",
        icon =  ~ icons(
          iconUrl = iconfile,
          iconWidth = 30,
          iconHeight = 30
        ),
        #color =  colorlf[[category]],
        label = ~label,
        #options = markerOptions(alt = group$searchmeta),
        #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
        clusterOptions = clusterOptions,
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
        popupOptions = popupOptions(closeOnClick = TRUE),
        group = "portals",
        color =  colorlf[[category]],
        label = ~label,
        #options = markerOptions(alt = group$searchmeta),
        #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
        clusterOptions = clusterOptions,
        clusterId = category,
        labelOptions = labelOptions(noHide = FALSE),#, className = "needAbsolute",offset= c(-8, -8)),
        data = group_nogdi,
        opacity = 0.7,
        fillOpacity = 0.2
      )
    # m <<- addCircleMarkers(m, group$lon, group$lat, popup = group$popup, group = category, color =  colormarker[[category]], label = group$Titel)
    #m <<- addAwesomeMarkers(m, group$lon, group$lat, popup = group$popup, group = category, label = group$Titel)

    invisible()
  })

  if(layerControls)
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

  m <- addSearchFeatures(m, targetGroups = "portals", options = searchFeaturesOptions(openPopup = TRUE, propertyName = "label"))
  return(m)
}




# createMap <- function(portale, crosstalk_group = "portale", clustering = TRUE, layerControls = TRUE) {
#   categories <- c("international","national","regional","kommunal")
#   colorlf <- c("green", "yellow", "blue", "brown")
#   names(colorlf) <- categories
#   #portale$searchmeta <- paste(portale$Titel, portale$Ort, sep = " | ")
#   
#   portale_shared <- SharedData$new(portale, group = crosstalk_group)
#   
#   gdi_legend = paste0("<img src=\"/",pchIcons(col = "grey"), "\"></img> GDI")
#   odp_legend = paste0("<img src=\"/",pchIcons(file_prefix = "portals_", col = "grey", pch = 21), "\"></img> Open Data Portale")
#   
#   
#   m <-
#     leaflet(data = portale_shared, options = list(preferCanvas = TRUE))  %>% 
#     #  addProviderTiles(providers$Stamen.TonerBackground) %>% 
#     #addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
#     addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
#     addLegend(
#       colors = colorlf,
#       values = categories,
#       labels = categories,
#       title = "Legende"
#     ) %>%
#     addResetMapButton() %>% 
#     addPolygons(data = gbounds,weight = 0.5,label = gbounds$localname, labelOptions = labelOptions(noHide = TRUE), group = "adm4") %>%
#     #addPolygons(data = sbounds,weight = 0.5,label = sbounds$localname, group  = "adm4") %>%
#     #addPolygons(data = abounds,weight = 0.5,label = abounds$localname, group = "adm4") %>%
#     addControl(paste(gdi_legend, odp_legend, sep="<br/>\n"),position = "topright") %>%
#     addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE, metric = TRUE)) %>%
#     leaflet.extras::enableTileCaching()
#   #addGeoJSON("data/bounds/Germany_AL2.GeoJson")
#   
#   
#   
#   
#   sapply(categories, function(category) {
#     cf <- paste0(
#       "function (cluster) {
#       var childCount = cluster.getChildCount();",
#       "var c = '",
#       colorlf[[category]],
#       "';",
#       "return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
#   }"
#     )
#     
#     iconfile <- pchIcons(colorlf[[category]])
#     
#     #group_nogdi <<- SharedData$new(portale[portale$Bezug==category & !portale$GDI,], group = crosstalk_group)
#     #group_gdi <<- SharedData$new(portale[portale$Bezug==category & portale$GDI,], group = crosstalk_group)
#     #group_nogdi <<- portale[portale$Bezug==category & !portale$GDI,]
#     group_nogdi <<- SharedData$new(portale[portale$Bezug==category & !portale$GDI,])
#     
#     clusterOptions <- NULL
#     if(clustering)
#       clusterOptions <- markerClusterOptions(iconCreateFunction = JS(cf), removeOutsideVisibleBounds = FALSE)
#     
#     # if(dim(group_gdi$data())[1]>0)
#     #   m <<-
#     #   addMarkers(
#     #     m,
#     #     ~lon,
#     #     ~lat,
#     #     popup = ~popup,
#     #     popupOptions = popupOptions(),
#     #     group = category,
#     #     icon =  ~ icons(
#     #       iconUrl = iconfile,
#     #       iconWidth = 30,
#     #       iconHeight = 30
#     #     ),
#     #     #color =  colorlf[[category]],
#     #     label = ~label,
#     #     #options = markerOptions(alt = group$searchmeta),
#     #     #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
#     #     clusterOptions = clusterOptions,
#     #     clusterId = category,
#     #     labelOptions = labelOptions(noHide = FALSE),#, className = "needAbsolute",offset= c(-8, -8)),
#     #     data =  group_gdi
#     #   )
#     
#     if(dim(group_nogdi$data())[1]>0)
#       m <<-
#       addCircleMarkers(
#         m,
#         ~lon,
#         ~lat,
#         popup = ~popup,
#         popupOptions = popupOptions(),
#         group = category,
#         color =  colorlf[[category]],
#         label = ~label,
#         #options = markerOptions(alt = group$searchmeta),
#         #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
#         clusterOptions = clusterOptions,
#         clusterId = category,
#         labelOptions = labelOptions(noHide = FALSE),#, className = "needAbsolute",offset= c(-8, -8)),
#         data = group_nogdi
#       )
#   #  m <<- addCircleMarkers(m, group$lon, group$lat, popup = group$popup, group = category, color =  colormarker[[category]], label = group$Titel)
#    # m <<- addAwesomeMarkers(m, group$lon, group$lat, popup = group$popup, group = category, label = group$Titel)
#     
#     invisible()
#   })
#   
#   if(layerControls)
#     m <-
#     addLayersControl(m,
#                      overlayGroups = levels(portale$Bezug),
#                      options = layersControlOptions())
#   
#   
#   ## some test functions
#   # sdf <- function(text, x){
#   #   print(paste("They called me with", text))
#   #   return(c("1","2","3"))
#   # }
#   # 
#   # sdf <- function(textSearch, allRecords){
#   #   JS('alert("HEY!");')
#   #   cat(paste("They called me with", textSearch))
#   #   return(allRecords[1])
#   # }
#   
#   m <- addSearchFeatures(m, targetGroups = levels(portale$Bezug), options = searchFeaturesOptions(openPopup = TRUE, propertyName = "label"))
#   return(m)
# }