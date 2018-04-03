source("R/portal_prerendering.R")
#generate frequency table per country and portal type
ftab_country_type <- ftable(data.frame(Typ = Typ, Land = Land))
sum_per_country <- colSums(as.matrix(ftab_country_type))
footer_stat <- append(paste0("Gesamt (",sum(sum_per_country),")"),sum_per_country)
flext2 <- as.FlexTable(ftab_country_type) %>%
  addFooterRow(footer_stat, par.properties = parProperties(text.align = "center")) %>%
  setZebraStyle(even = tab_colors[2], odd = 'white' ) %>%
  setFlexTableBackgroundColors(colors = tab_colors[5],to = c("header")) %>%
  setFlexTableBackgroundColors(colors = tab_colors[5],to = c("footer"))

flext2[,1] <- paste0(" (",as.numeric(rowSums(as.matrix(ftab_country_type))),")")
flext2


statdoc2 <- docx() %>%
  addTitle("Übersicht über das Open Data Suchportal", level = 1 ) %>%
  addFlexTable(flext2) %>%
  addParagraph("Anzahl der Datenportale im Verzeichnis",stylename = "rTableLegend") %>%
  writeDoc(file="out/verzeichnis_weitere_statistiken.docx")
