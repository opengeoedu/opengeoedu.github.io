library(RCurl)
library(urltools)
library(reticulate)
library(stringr)

unorm <- import("url_normalize")
url_normalize <- function(str) as.character(sapply(as.character(str), unorm$url_normalize, USE.NAMES = FALSE, simplify = TRUE))
url_exists <- function(x) {
  x <- as.character(x)
  cat("testing URL:", x)
  success <- RCurl::url.exists(x, .opts = list(timeout=10))
  if (!success) {
    #double-check because of false-positives
    res <- try({
      con <- url(x, open = "r")
      if (isOpen(con))
        close(con)
    }
    , silent = TRUE)
    success <- class(res) != "try-error"
  }
  if (success)
    cat(" ....AVAILABLE!\n")
  else
    cat(" ....NOT AVAILABLE\n")
  return(success)
}

portale <- read.csv("data/portale_geocoded.csv")
portale$URL <- url_normalize(portale$URL)

ogidata <- read.csv("data/open-data-sources.csv", sep = ";")
ogidata$URL <- unlist(url_normalize(ogidata$URL))
german_data <- ogidata[ogidata$Country %in% c("Germany","Austria", "Switzerland"),]
german_data$Land <- str_replace(german_data$Country, "Germany", "Deutschland")
german_data$Land <- str_replace(german_data$Land, "Austria", "Österreich")
german_data$Land <- str_replace(german_data$Land, "Switzerland", "Schweiz")

uext <- sapply(german_data$URL, url_exists)

#show unavailable urls and write them in a separate table
german_data[!uext,c("URL","Name", "has_issue")]
error_data <- german_data[!uext,]
write.csv(x = error_data, file = "data/unavailable-sources.csv")

#filter the available data
german_data <- german_data[uext,]
dim(german_data)


urlvgl1 <- url_parse(german_data$URL)
#str1 <- paste(urlvgl1$domain,urlvgl1$path,sep="/")
str1 <- paste(urlvgl1$domain)
str1 <- str_replace(str1,"^www\\.","")
urlvgl2 <- url_parse(portale$URL)
str2 <- urlvgl2$domain
str2 <- str_replace(str2,"^www\\.","")

dist.urls <- adist(str1, str2, ignore.case = TRUE)
dim(dist.urls)


seldub <- c()
selun <- c()
for(i in 1:dim(dist.urls)[1]){
  row <- dist.urls[i,]
  sel <- which(row == min(row))[1]
  if(min(row)<2){
    cat("Entry ",paste(german_data[i,c("Name","URL")])," ",min(row))
    cat("\t\nIs similar to ", portale[sel,"URL"]," with distance ", min(row),"\n\n")
    seldub <- append(seldub, i) #dublicate entries
  }else
    selun <- append(selun, i) #unique entries
}

unique_entries <- german_data[selun,]
dublicate_entries <- german_data[seldub,]
write.csv(dublicate_entries, "data/dublicate-sources.csv")



#portale2 <- rbind(portale,
 
new_entries <-  data.frame(URL = unique_entries$URL, 
                            Titel = unique_entries$Name, 
                            Land = unique_entries$Land,
                            lat = str_trim(str_split(unique_entries$Location,",",simplify = TRUE)[,1]),
                            lon = str_trim(str_split(unique_entries$Location,",",simplify = TRUE)[,2])
                            #Bezug = rep("", dim(unique_entries))[1],
                            #Lizenz = rep("", dim(unique_entries))[1],
                            #Staatlich_Öffentlich =
                            )
portale2 <- merge(portale, new_entries, all = TRUE)

write.csv(portale2, "data/portale_geocoded2.csv")



# curlGetHeaders()
# 
# url("http://www.daten.halle.de/", open = "r")
# url.exists("")
# 
# t <- open
# url <- url_normalize(c("halle.de", "fooo.de"))
# url
# urlt <-
#   "http://opengeodatalandkreisdiepholz-geoweb-diepholz.opendata.arcgis.com/"
# URLdecode(urlt)
# 
# t <-
#   url(
#     "http://opengeodatalandkreisdiepholz-geoweb-diepholz.opendata.arcgis.com/",
#     open = "r"
#   )
# t
# 
# t <- url("http://data.groningen.nl/", open = "r")
# 
# url.show("http://opengeodatalandkreisdiepholz-geoweb-diepholz.opendata.arcgis.com/")
# 
# t <- url("http://offenedaten.kdvz-frechen.de/group/ker", open = "r")
# 
# 
# url.exists("http://data.groningen.nl/")
# 
# 
# url.exists("http://offenedaten.kdvz-frechen.de/group/ker")
# 
# 
# url.exists("http://opengeodatalandkreisdiepholz-geoweb-diepholz.opendata.arcgis.com/")
# 
# 
# 
# curl <-
#   getCurlHandle("--user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/534.30 (KHTML, like Gecko) Chrome/12.0.742.112 Safari/534.30")
# 
# x <- "https://www.govdata.de/"
last <- function(x) { return( tail(x, n = 1) ) }

outputDir <- "data/user_input"
files <- list.files(outputDir, full.names = TRUE)
files <- files[stringr::str_detect(files,"^((?!Beispiel).)*\\.csv$")];files
#if(length(files)==0)
#  return(NULL)
data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
# Concatenate all data together into one data.frame
data <- do.call(rbind, data)
names(data)[8] <- "Staatlich_Öffentlich"
names(data)
data <- data[names(data)!="Autor"]


portale <- read.csv("data/portale_geocoded4.csv", as.is = TRUE)
data$ID = (last(portale$ID)+1):(last(portale$ID)+dim(data)[1])
names(portale)
portale2 <- merge(portale, data, all = TRUE)
write.csv(portale2, file = "data/portale_geocoded4.csv", row.names = FALSE)
