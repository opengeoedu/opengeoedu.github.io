library(rjson)


getData <- function(.url){
  con <- url(.url)
  text <- readLines(.url, warn = FALSE)
  close(con)
  parser <- newJSONParser()
  parser$addData(text)
  return(parser$getObject())
}

.url <- "https://offenedaten.de/api/3/action/organization_list"
organizations <- getData(.url)

.name <- "verbandsgemeindecochem"


organizations_details <- NULL

getDetails <- function(.name){
  org_show <- paste0("https://offenedaten.de/api/3/action/organization_show?id=", .name)
  #print(org_show)
  details <- getData(org_show)
  sapply(details$result$extras, function(x){details[[x$key]] <<- x[["value"]]})
  #print(details)
  row_entry <- list(name = details$result$display_name , description = details$result$description, lon = details$longitude, lat = details$latitude, url=details$url, open_data_url=details$open_data_portal)
  row_entry <- sapply(row_entry, function(x) {if(is.null(x)) return(NA) else x})
  row_entry <- t(data.frame(row_entry))
  #print(row_entry)
   if(is.null(organizations_details)){
   organizations_details <<- row_entry
 }else{
   organizations_details <<- rbind(organizations_details, row_entry)
 }
  return(invisible())
}

sapply(organizations$result, getDetails);organizations_details


write.csv(organizations_details, "data/offenedatan.de_organizations.csv",row.names = FALSE)
