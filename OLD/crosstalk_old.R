# sd <- SharedData$new(portale)
# p_reg <- portale[portale$Bezug == "regional",]
# sd_regional <- SharedData$new(p_reg)
# p_komm <- portale[portale$Bezug == "kommunal",]
# sd_komm <- SharedData$new(p_komm)


sd <- SharedData$new(portale, group = "portale")
sd_regional <- SharedData$new(portale[portale$Bezug == "regional" & portale$GDI,], group = "portale")
sd_regional_gdi <- SharedData$new(portale[portale$Bezug == "regional" & !portale$GDI,], group = "portale")
sd_komm <- SharedData$new(portale[portale$Bezug == "kommunal",], group = "portale")
sd_table <- SharedData$new(portale[c("Link","Beschreibung","Ort","Bezug")], group = "portale")

m2 <- leaflet(sd) %>% addProviderTiles(providers$CartoDB.Positron) %>% addMarkers(data = sd_regional, label =  ~Titel, popup = ~Titel) %>% addCircleMarkers(data = sd_komm, label = ~Titel)

m2 <- leaflet(sd) %>% addProviderTiles(providers$CartoDB.Positron) %>% addMarkers(data=sd_regional, label =  ~Titel, popup = ~Titel)%>% addMarkers(data=sd_regional_gdi, label =  ~Titel, popup = ~Titel)



bscols(widths = c(NA,7),
       list(
         filter_checkbox("bezug_check", "Bezug", sd, ~Bezug, inline = TRUE),
         filter_checkbox("bezug_portal", "Portal-Art", sd, ~Typ, inline = TRUE), DT::datatable(sd_table, escape = FALSE, options = list(
           bPaginate = TRUE
         ))),
       m2
       
)




testfun <- function(){
  testfun2 <- function(t){
    sdx_regional <- SharedData$new(portale[portale$Bezug == "regional" & portale$GDI,], group = "portale")
    sdx_regional_gdi <- SharedData$new(portale[portale$Bezug == "regional" & !portale$GDI,], group = "portale")
    sdx_komm <- SharedData$new(portale[portale$Bezug == "kommunal",], group = "portale")
    m2 <- leaflet(sd) %>% addProviderTiles(providers$CartoDB.Positron) %>% addMarkers(data = sdx_regional, label =  ~Titel, popup = ~Titel) %>% addCircleMarkers(data = sdx_komm, label = ~Titel)
    
    m2 <<- leaflet(sd) %>% addProviderTiles(providers$CartoDB.Positron) %>% addCircleMarkers(data=sdx_regional, color="red", label =  ~Titel, popup = ~Titel)%>% addCircleMarkers(data=sdx_regional_gdi, label =  ~Titel, popup = ~Titel)
    
    rm("sdx_regional_gdi","sdx_komm", "sdx_regional")
    print(ls())
  }
  sapply("test",testfun2)
  m2
}
m2 <- testfun()
m2


# bscols(widths = c(NA,7),
#        list(
#          filter_checkbox("bezug_check", "Bezug", sd, ~Bezug, inline = TRUE),
#          filter_checkbox("bezug_portal", "Portal-Art", sd, ~Typ, inline = TRUE),
#        m2)
#        
# )