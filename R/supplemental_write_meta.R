#This script creates a table that describes the different categories, abbreviations and color schemes used
library(jsonlite)

reichw <- c("international","national","regional","kommunal")
reichw_col  <- c("#006400", "#FFA500", "#0000FF", "#8B0000")
typ <- c("odp","gdi","stat","umw", "for", "cc")
typ_names <- c("Open Data Portal","GDI/ Geoportal", "Statistikamt", "Umweltamt", "Forschungsdatenportal", "Citizen Science Projekt")
#pch - value for portal-Type symbole - see ?graphics::points
typ_pch <- c("21","24","23","22","3", "4")
staatl <- c("ja", "nein", "teilweise")

table_meta <- list(reichw = reichw, reichw_col = reichw_col, typ = typ, typ_names = typ_names, typ_pch=typ_pch, staatl=staatl)
write_json(table_meta, "data/table_meta.json", pretty = TRUE, simplifyVector = TRUE)

#how to read the data:
#read_json("data/table_meta.json", simplifyVector = TRUE)
