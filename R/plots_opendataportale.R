source("R/portal_prerendering.R")

portale.sp <- portale
coordinates(portale.sp) <- ~lon+lat
proj4string(portale.sp) <- CRS("+proj=longlat +datum=WGS84")

plot(g4bounds)
points(portale.sp[ (portale.sp$ID %in% country_map$Deutschland) & portale.sp$Typ == "odp" & portale$Reichweite == "kommunal",], pch=3)
country_map
