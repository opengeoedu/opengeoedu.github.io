library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(crosstalk)
library(rgeos)

# for createing svgs for each portal type:
#mapply(function(pchnum, prefix){pchIcons(img_format = "svg", pch = pchnum, col="black", file_prefix = prefix)}, pchnum=as.numeric(table_meta$typ_pch), prefix = paste0(table_meta$typ,"_"))

# helper function that creates triangular icon files in differnet colors in folder tempicon
# use point symbols from base R graphics as icons
pchIcons <- function(col, width = 35, height = 35, pch = 24, file_prefix="gdi-icon-",plotOnly = FALSE, img_format = "png", ...) {
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
  
  if(plotOnly){
    plot((1:n-1)+.5, rep(1,n)*.5, pch = pch, cex = (min(width, height) / 8) -1, ..., col=col, bg=addalpha(col, 0.3), lwd=5,xlim = c(0,n+0.5))
    return()
  }
  
  for (i in seq_len(n)) {
    #f = tempfile(tmpdir = "icontemp", fileext = '.png')
    col_transp <- addalpha(col[i], 0.3)

    if(!dir.exists("icontemp")){
      cat("Created directory 'icontemp' in order to store icon files. You can remove this folder manually after the output was created: \n\t",path.expand("./icontemp"))
      dir.create("icontemp")
    }
    
    if(img_format == "png"){
      f <- paste0("icontemp/",file_prefix,stringr::str_replace(col[i],"#",""),".png")
      file.create(f)
      png(f, width = width, height = height, bg = 'transparent')

    } else if(img_format == "svg"){
      f <- paste0("icontemp/",file_prefix,stringr::str_replace(col[i],"#",""),".svg")
      file.create(f)
      svg(f, width = 1, height = 1, bg = 'transparent', pointsize = 30)
    }

    par(mar = c(0, 0, 0, 0))
    plot.new()
    #points(.5, .5, pch = 17, cex = min(width, height) / 8, ..., col=col)
    points(.5, .5, pch = pch, cex = (min(width, height) / 8) -1, ..., col=col[i], bg=col_transp, lwd=5)
    dev.off()
    files[i] = f
  }
  files
}


##function to add a set of markers as one layer
addPortalMarker <- function(m, datagroup, clusterOptions, iconfiles, iconWidth = 30, iconHeight = 30, group = "portals", clusterId = NULL){
  #print(inherits(datagroup, "SharedData"))
  #print(dim(datagroup))
  if( (!inherits(datagroup, "SharedData") && dim(datagroup)[1]>0) || (inherits(datagroup, "SharedData") && dim(datagroup$data())[1]>0)){
    m <-addMarkers(
      m,
      ~lon,
      ~lat,
      popup = ~popup,
      popupOptions = popupOptions(closeOnClick = TRUE),
      group = group,
      icon =  ~ icons(
        iconUrl = iconfiles,
        iconWidth = iconWidth,
        iconHeight = iconHeight
      ),
      #color =  colorlf[[clusterId]],
      label = ~label,
      options = markerOptions(),
      #options = markerOptions(alt = group$searchmeta),
      #  clusterOptions = markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE),
      clusterOptions = clusterOptions,
      clusterId = clusterId,
      labelOptions = labelOptions(textOnly = FALSE,noHide = FALSE,offset = c(3,-8)),#, className = "needAbsolute",offset= c(-8, -8)),
      data =  datagroup
    )
  }
  return(m)
}

# function initialized by createMap
addAllPortalMarkers <- NULL


createMap <- function(portale, table_meta, crosstalk_group = "portale", clustering = TRUE, layerControls = TRUE, polygon_fill_color = "#696969", polygon_fill_opacity = 0.2) {
  categories <- table_meta$reichw
  colorlf <- table_meta$reichw_col
  names(colorlf) <- categories
  
  
  create_legend_entry <- function(iconpath, entryname, iconwidth = "25px"){
    htmltools::tagList(
      tag("nobr",list(tags$input(type="checkbox", class="crosstalk_checkbox", value=entryname, checked=TRUE, onChange = paste0("toggleFilter('",entryname,"', this)"))  ,tags$img(src=paste0(iconpath), width=iconwidth), 
                      tags$span(entryname))), 
      tags$br(clear="all"))
  }
  
  type_legend_entries <-
    mapply(
      function(icon_prefix, icon_pch, entryname) {
        iconpath <-
          pchIcons(col = "grey",
                   file_prefix = icon_prefix,
                   pch = icon_pch)
        create_legend_entry(iconpath, entryname)
      },
      icon_prefix = paste0(table_meta$typ, "_"),
      icon_pch = as.numeric(table_meta$typ_pch),
      entryname = table_meta$typ_names,
      SIMPLIFY = FALSE
    )
  
  htmlLegend <- 
    tags$div(class="legend_div",  `aria-haspopup`="true", htmltools::tagList(
    #  tags$a(href='#', class="legend_link",
        div(class="legend_toggle info", style="width:40px; height:40px; text-align:center",icon("info", "fa-2x")),
    tags$div(class = "legend_map info legend", `aria-label`="submenu", `aria-hidden` = "true",
                         div(style = "margin-bottom:3px", tags$strong("Legende")),
                         htmltools::tagList(
                          type_legend_entries,
                           #optionally include controls in legend (would have to be synchronized with other controls on the map)
                           #filter_checkbox("bezug_portal2", "Portal-Art", sd, ~Typ, inline = TRUE),
                           #filter_checkbox("bezug_check2", "Räumlicher Bezug", sd, ~Bezug, inline = TRUE),
                          tags$table(
                          mapply(
                             function(color, label) {
                               tags$tr(tagList(
                                 tags$td(
                                  tags$input(type = "checkbox", checked=TRUE, class="crosstalk_checkbox", value = label,label="", onChange = paste0("toggleFilter('",label,"', this)"))
                                 ),
                                 tags$td(
                                   tagList(
                                    tags$i(
                                      style = paste0(
                                      "background:",
                                      color,
                                      "; opacity:0.5; margin:0; padding:0; margin-right:10px; margin-left:10px"
                                      )
                                    ),
                                 label)
                                 )
                               ))
                             },
                             color = colorlf,
                             label = categories,
                             SIMPLIFY = FALSE
                           )
                           
                         )
                         ))
    ))
  #portale$searchmeta <- paste(portale$Titel, portale$Ort, sep = " | ")

  portale_shared <- SharedData$new(portale, group = crosstalk_group)

  #label options for admin boundaries
  labeladm6opts <- labelOptions(textOnly = TRUE, noHide = TRUE, direction = "bottom", opacity = 0.5, textsize = "10pt")
  labeladm4opts <- labelOptions(textOnly = TRUE, noHide = TRUE, direction = "bottom", opacity = 0.5, textsize = "13pt", style = "color:#03F")
  #labelPolyopts <- labelOptions(direction = "right", style = "color:yellow; text-shadow: 0 0 0.1em black, 0 0 0.1em black,
   #     0 0 0.1em black,0 0 0.1em black,0 0 0.1emtags$input(type="checkbox", value=entryname);")
  
  m <-
    leaflet(data = portale_shared, options = leafletOptions(minZoom = 4, maxZoom = 12, preferCanvas = TRUE))  %>%
    # map view and max bounds
    fitBounds(5.86,45.5,15,55.1) %>%
    #setView(10.8418, 50.5, zoom = 6)  %>%
    setMaxBounds(2,40,22,65) %>%
    #Background map:
    #  addProviderTiles(providers$Stamen.TonerBackground) %>%
    # addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE, metric = TRUE)) %>%
    #addMeasure(position = "bottomright", primaryLengthUnit = "meters", localization = "de") %>%
    #Map legend
  # (build-in legend not suitable for symbols, html-legend used instead)
   # / addLegend(
   #    colors = colorlf,
   #    values = categories,
   #    labels = categories,
   #    title = "Legende"
   #  ) %>%
    addControl(htmlLegend ,position = "topright", className="") %>%
    addResetMapButton() %>%
    addFullscreenControl() %>%
    addPolygons(data = g6bounds, color = "grey", fillColor = polygon_fill_color, weight = 1, group = "adm6",fill=TRUE,label = g6bounds$localname, fillOpacity = 0, options = pathOptions(clickable = TRUE, className = "polygonShape")) %>%
    #addLabelOnlyMarkers(data = gCentroid(geometry(g6bounds), byid = TRUE), label = g6bounds$localname, group = "adm6_labels", labelOptions = labeladm6opts) %>%
    #addPolygons(data = s6bounds, color = "grey", fillColor = polygon_fill_color, weight = 1, group  = "adm6",label = s6bounds$localname,fill=TRUE, fillOpacity = 0) %>%
    #addLabelOnlyMarkers(data = gCentroid(geometry(s6bounds), byid = TRUE), label = s6bounds$localname, group = "adm6_labels", labelOptions = labeladm6opts) %>%
    addPolygons(data = a6bounds, color = "grey", fillColor = polygon_fill_color, weight = 1,label = a6bounds$localname, group = "adm6",fill=TRUE, fillOpacity = 0, options = pathOptions(clickable = TRUE, className = "polygonShape")) %>%
    #addLabelOnlyMarkers(data = gCentroid(geometry(a6bounds), byid = TRUE), label = a6bounds$localname, group = "adm6_labels", labelOptions = labeladm6opts) %>%
    addPolygons(data = g5bounds, color = "grey", fillColor = polygon_fill_color, weight = 1.5,label = g5bounds$localname, group = "adm5",fill=FALSE, options = pathOptions(clickable = TRUE, className = "polygonShape")) %>%
    #addPolygons(data = s5bounds, color = "grey", fillColor = polygon_fill_color, weight = 1.5,label = s5bounds$localname, group  = "adm5", fill=TRUE, fillOpacity = 0) %>%
    addPolygons(data = g4bounds, color = "grey", fillColor = polygon_fill_color, weight = 2, label = g4bounds$localname, group = "adm4", fill=TRUE, fillOpacity = polygon_fill_opacity, options = pathOptions(clickable = TRUE, className = "polygonShape")) %>%
    addPolygons(data = s4bounds, color = "grey", fillColor = polygon_fill_color, weight = 2, label = s4bounds$localname, group  = "adm4", fill=TRUE, fillOpacity = polygon_fill_opacity, options = pathOptions(clickable = TRUE, className = "polygonShape")) %>%
    addPolygons(data = a4bounds, color = "grey", fillColor = polygon_fill_color, weight = 2, label = a4bounds$localname, group = "adm4", fill=TRUE, fillOpacity = polygon_fill_opacity, options = pathOptions(clickable = TRUE, className = "polygonShape")) %>%
    addLabelOnlyMarkers(data = gPointOnSurface(geometry(g4bounds), byid = TRUE), label = g4bounds$localname, group = "adm4_labels", labelOptions = labeladm4opts, options = markerOptions(clickable = FALSE)) %>%
    addLabelOnlyMarkers(data = gPointOnSurface(geometry(a4bounds), byid = TRUE), label = a4bounds$localname, group = "adm4_labels", labelOptions = labeladm4opts, options = markerOptions(clickable = FALSE)) %>%
    #addLabelOnlyMarkers(data = gCentroid(geometry(s4bounds), byid = TRUE), label = s4bounds$localname, group = "adm4_labels", labelOptions = labeladm4opts) %>%
    addPolygons(data = g2bounds,weight = 2, color= "black",group = "adm2", fill = FALSE, options = pathOptions(clickable = FALSE)) %>%
    addPolygons(data = s2bounds,weight = 2, color= "black",group  = "adm2", fill = FALSE, options = pathOptions(clickable = FALSE)) %>%
    addPolygons(data = a2bounds,weight = 2, color= "black",group = "adm2", fill = FALSE, options = pathOptions(clickable = FALSE)) %>%
    addLabelOnlyMarkers(data=pplc, label = pplc$name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, zoomAnimation = FALSE, textsize = "10pt", style = "color:#03F"), options = markerOptions(clickable = FALSE)) %>%
    addLabelOnlyMarkers(data=ppla, label = ppla$name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, zoomAnimation = FALSE, textsize = "10pt", style = "color:#03F"), group = "adm4_labels", options = markerOptions(clickable = FALSE)) %>%
    addLabelOnlyMarkers(data=ppl, label = ppl$name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, zoomAnimation = FALSE, textsize = "10pt", style = "color:#03F"), group = "adm6_labels", options = markerOptions(clickable = FALSE)) %>%
    addCircleMarkers(data=pplc, color = "red",radius=1, options = markerOptions(clickable = FALSE)) %>%
    addCircleMarkers(data=ppla, color = "black", radius=1, group = "adm4_labels", options = markerOptions(clickable = FALSE)) %>%
    addCircleMarkers(data=ppl, color = polygon_fill_color ,radius=1, group = "adm6_labels", options = markerOptions(clickable = FALSE)) %>%
    #addCircleMarkers(data=pplc, color = "red",radius=1, group = "adm2_labels", options = markerOptions(clickable = FALSE),label = pplc$name, labelOptions = labelOptions(noHide = TRUE, zoomAnimation = FALSE, textsize = 13, className = "ppl_label" )) %>%
    #addCircleMarkers(data=ppla, color = "black", radius=1, group = "adm4_labels", options = markerOptions(clickable = FALSE, zIndexOffset = -500), label = ppla$name, labelOptions = labelOptions(noHide = TRUE, zoomAnimation = FALSE, style = "z-index: -1")) %>%
    #addCircleMarkers(data=ppl, color = polygon_fill_color ,radius=1, group = "adm6_labels", options = markerOptions(clickable = FALSE, zIndexOffset = -500), label = ppl$name, labelOptions = labelOptions(noHide = TRUE, zoomAnimation = FALSE, style = "z-index: -1")) %>%
    #hideGroup("adm6") %>% hideGroup("adm5") %>% hideGroup("adm6_labels") %>% hideGroup("adm4_labels")  %>%
    groupOptions("adm6_labels", zoomLevels = 9:18) %>% 
    groupOptions("adm4_labels", zoomLevels = 7:18) %>% 
    groupOptions("adm6", zoomLevels = 8:18) %>%
    groupOptions("adm5", zoomLevels = 8:18) %>% 
    leaflet.extras::enableTileCaching()  %>% 
  #  htmlwidgets::onRender("alert('test after Rendering');")
     htmlwidgets::onRender("onRenderMap()")
    
      #addGeoJSON("data/bounds/Germany_AL2.GeoJson")
  
  addAllPortalMarkers <<- function(m, portale, groupname, iconWidth, iconHeight, icon_prefix_extension =""){
    print(paste(sapply(list(m,portale,groupname,iconWidth,iconHeight, icon_prefix_extension), class), collapse = " "))

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
    
        clusterOptions <- NULL
        if(clustering)
          clusterOptions <- markerClusterOptions(iconCreateFunction = JS(cf), removeOutsideVisibleBounds = FALSE)
          #markerClusterOptions(iconCreateFunction = JS(cf), spiderfyOnMaxZoom = TRUE, freezeAtZoom = 8, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE)
        
        
        mapply(function(portal_typ, group_pch){
          
          group_data <- SharedData$new(portale[portale$Reichweite == category & portale$Typ==portal_typ,], group = groupname)
          group_iconfile <- pchIcons(colorlf[[category]],  pch = group_pch, file_prefix = paste0(portal_typ,"_",icon_prefix_extension), width = iconWidth, height = iconHeight)
         
          m <<- addPortalMarker(m, group_data, clusterOptions, group_iconfile, iconWidth = iconWidth, iconHeight = iconHeight, group = groupname, clusterId = category)
          
          # group_data <- SharedData$new(portale[portale$Reichweite == category & portale$Typ==portal_typ,], group = crosstalk_group)
          # group_iconfile <- pchIcons(colorlf[[category]],  pch = group_pch, file_prefix = paste0(portal_typ,"_"), width = 35, height = 35)
          # m <<- addPortalMarker(m, group_data, group_iconfile, iconWidth = 35, iconHeight = 35, group = "portals")
          
          # group_data <- SharedData$new(portale[portale$Reichweite == category & portale$Typ==portal_typ,], group = "portals_small")
          # group_iconfile <- pchIcons(colorlf[[category]],  pch = group_pch, file_prefix = paste0(portal_typ,"_small_"), width = 20, height = 20)
          # m <<- addPortalMarker(m, group_data, group_iconfile, iconWidth = 20, iconHeight = 20, group = "portals_small")
        #  m <<- addPortalMarker(m, portale[portale$Reichweite == category & portale$Typ==portal_typ,], group_iconfile, iconWidth = 20, iconHeight = 20, group = "portals_small")
        }, portal_typ = table_meta$typ, group_pch = as.numeric(table_meta$typ_pch))
        
        invisible()
      })

    return(m)
  }
  
  m <- addAllPortalMarkers(m, portale = portale, "portale", 35, 35)

  

  if(layerControls)
    m <-
      addLayersControl(m,
                       overlayGroups = levels(portale$Reichweite),
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

  m <- addSearchFeatures(m, targetGroups = "portale", options = searchFeaturesOptions(openPopup = TRUE, propertyName = "label", textPlaceholder = "Suche Portal...", textCancel="Abbruch", textErr = "Portal nicht gefunden", zoom="10", hideMarkerOnCollapse=TRUE))
 
# zoom-level depended rendering of small/large icons doesn't work well (yet)   
 #m <- hideGroup(m, "portale") %>%
#    groupOptions("portale", zoomLevels = 8:18) %>% 
#    groupOptions("portale_small", zoomLevels = 1:7)
    
  return(m)
}

