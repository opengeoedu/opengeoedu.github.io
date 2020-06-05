library(crosstalk)
library(leaflet)
library(DT)

# Wrap data frame in SharedData
sampledata <- quakes[sample(nrow(quakes), 10),]
sampledata$id <- 1:dim(sampledata)[1]
sd <- SharedData$new(sampledata, key= ~id)

largeLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95
)


smallLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 20, iconHeight = 60
)


m <- leaflet(sd) %>% addTiles() %>% 
  addMarkers(icon = ~smallLeafIcon, group = "small_scale") %>%
  addMarkers(icon = ~largeLeafIcon, group = "large_scale") %>%
  groupOptions("small_scale", zoomLevels = 1:3) %>%
  groupOptions("large_scale", zoomLevels = 4:18)

bscols(
  # Create a filter input
  filter_slider("mag", "Magnitude", sd, column=~mag, step=0.1, width=250),
  m,
  datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
            options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
)

