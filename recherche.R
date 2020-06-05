library("sf")
library(dplyr)
library(stringr)
dt <- st_read("data/bounds/Austria_AL4.GeoJson", stringsAsFactors = FALSE)
portale <- read.csv("data/portale_geocoded4.csv", as.is = TRUE)



dtnames <- dt$localname %>% c(dt$name)  %>% str_split("[/, ]") %>% unlist()
dtnames <- list()

umw_orte <- sapply(portale_umw$Ort %>% str_split("[/,]"), 
                   function(names){names <- names[names != ""] %>% str_trim()
                   names}
)

sel_umw_missing <- c()
for(i in 1:dim(dt)[1]){
  dtnames[[i]] <- unique(unlist(c(dt$localname[i] %>% str_split("[/,]"), dt$name[i] %>% str_split("[/,]"))))
  sel_umw_missing <- append(sel_umw_missing, any(dtnames[[i]] %in% unlist(umw_orte)))
}
umw_missing <- dt$name[!sel_umw_missing]

sapply(umw_missing, function(location){
  paste("https://www.google.de?q=umweltdaten", location) %>% browseURL(browser = "firefox")
})



portale_umw <- portale[portale$Typ == "umw",]

umw_exists <- sapply(portale_umw$Ort %>% str_split("[/,]") %>% str_trim(), 
                     function(names){names <- names[names != ""]
                     any(names %in% dtnames)}
)






sapply(as.character(dt$localname), function(location){
  paste("https://www.google.de?q=umweltdaten", location) %>% browseURL(browser = "firefox")
})



portale_umw$Ort %in% dtnames



length(portale_umw$Ort %>% str_split("[/, ]") %in% dtnames)
 