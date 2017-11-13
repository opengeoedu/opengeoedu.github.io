library(htmltools)
library(htmlwidgets)

portale <- read.csv("data/portale_geocoded3.csv", as.is = TRUE)

portale$Link <- paste0("<a href=\"",htmlEscape(portale$URL),"\" target=\"_blank\">",htmlEscape(portale$Titel),"</a>")

tabledt <- DT::datatable(portale[c("Link","Beschreibung","Ort","Bezug","Lizenz", "Staatlich_Öffentlich")], escape = FALSE, colnames = c("Link","Beschreibung","Ort","Bezug","Lizenz", "Staatlich/Öffentlich"), options = list(
  bPaginate = TRUE
))


#m <- prependContent(m, tags$meta(`http-equiv`="expires", content="0"))
#m <- attachDependencies(m, tags$meta(`http-equiv`="expires", content="0"))
#m$dependencies <- append(m$dependencies, tagList(tags$meta(`http-equiv`="expires", content="0")))
setwd("out")
saveWidget(tabledt, file="portale_tabelle.html",  selfcontained = FALSE)
setwd("..")
#saveWidget(m, file="portale.html", selfcontained = FALSE)
