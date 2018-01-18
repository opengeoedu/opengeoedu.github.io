source("R/create_map_function.R")
setwd("material")

categories <- c("international","national","regional","kommunal")
colorlf <- c("#006400", "#FFA500", "#0000FF", "#8B0000")
names(colorlf) <- categories

odp_legend_icon <- pchIcons(file_prefix = "odp_", pch = 21, col = "grey")
gdi_legend_icon <- pchIcons(col = "grey")
stat_legend_icon <- pchIcons(file_prefix = "stat_", pch = 23, col = "grey")
umw_legend_icon <- pchIcons(file_prefix = "umw_", pch = 22, col = "grey")
for_legend_icon <- pchIcons(file_prefix = "for_", pch = 3, col = "grey")
cc_legend_icon <- pchIcons(file_prefix = "cc_", pch = 4, col = "grey")

bez_icons <- pchIcons(file_prefix = "bez_", pch = 15, col = colorlf)

htmlLegend2 <- 
  tags$div(class="legend_div",  htmltools::tagList(
    div(class="legend_toggle info", style="width:40px; height:40px; text-align:center",icon("info", "fa-2x")),
    tags$div(class = "legend_map info legend leaflet-control",
             div(style = "margin-bottom:3px", tags$strong("Legende")),
             htmltools::tagList(
               tag("nobr",list(tags$img(src=paste0("",odp_legend_icon), width="25px"), tags$span("Open Data Portal"))), tags$br(clear="all"),
               tag("nobr",list(tags$img(src=paste0("",gdi_legend_icon), width="25px"), tags$span("GDI/ Geoportal"))), tags$br(clear="all"),
               tag("nobr",list(tags$img(src=paste0("",stat_legend_icon), width="25px"), tags$span("Statistikamt"))), tags$br(clear="all"),
               tag("nobr",list(tags$img(src=paste0("",umw_legend_icon), width="25px"), tags$span("Umweltamt"))), tags$br(clear="all"),
               tag("nobr",list(tags$img(src=paste0("",for_legend_icon), width="25px"), tags$span("Forschungsdatenportal"))), tags$br(clear="all"),
               tag("nobr",list(tags$img(src=paste0("",cc_legend_icon), width="25px"), tags$span("Citizen Science Projekt"))), tags$br(clear="all"),
               #optionally include controls in legend (would have to be synchronized with other controls on the map)
               #filter_checkbox("bezug_portal2", "Portal-Art", sd, ~Typ, inline = TRUE),
               #filter_checkbox("bezug_check2", "Räumlicher Bezug", sd, ~Bezug, inline = TRUE),
             #   mapply(
             #     function(color, label) {
             #       htmltools::tagList(tag("nobr", list(tags$i(
             #         style = paste0("background:", color, "; opacity:0.5; margin:0; padding:0; margin-right:3px")
             #       ), label)), HTML("&nbsp;"), tags$br(clear = "all"))
             #     },
             #     color = colorlf,
             #     label = categories,
             #     SIMPLIFY = FALSE
             #   )
             #   
             # ))
             
             mapply(
               function(iconpath, label) {
                 htmltools::tagList(tag("nobr",list(tags$img(src=paste0("",iconpath), width="30px"), tags$span(label))), tags$br(clear="all"))
               },
               iconpath = bez_icons,
               label = categories,
               SIMPLIFY = FALSE
             )
             
             ))
    
    
    
  ))


htmltools::save_html(htmlLegend2, "symbology.html",libdir = "legende")
setwd("..")
