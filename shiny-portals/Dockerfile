FROM rocker/geospatial
LABEL maintainer="OpenGeoEdu"
RUN apt-get update && apt-get install -y \
	gdebi-core && \
	wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.6.875-amd64.deb && \
	gdebi -n shiny-server-1.5.6.875-amd64.deb
RUN ["install2.r", "-r 'https://cloud.r-project.org'", "rtable", "ReporteRs", "ReporteRsjars", "xtable", "RColorBrewer", "rgeos", "sp", "shinyjs", "htmltools", "purrr", "rJava", "lattice", "yaml", "base64enc", "rlang", "R.oo", "pillar", "glue", "R.utils", "gdtools", "lambda.r", "uuid", "stringr", "futile.logger", "R.methodsS3", "zip", "htmlwidgets", "evaluate", "knitr", "httpuv", "markdown", "Rcpp", "backports", "jsonlite", "mime", "png", "digest", "stringi", "rprojroot", "magrittr", "lazyeval", "tibble", "futile.options", "tidyr", "xml2", "rvg", "rmarkdown", "officer", "R6", "remotes"]
RUN ["installGithub.r", "bhaskarvk/leaflet.extras@23de9ea", "rstudio/crosstalk@0a3b8f4", "rstudio/DT@a314d79", "rstudio/flexdashboard@89d711c", "rstudio/leaflet@d489e2c", "rstudio/shiny@39a7f63"]
EXPOSE 80

COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
CMD ["/usr/bin/shiny-server.sh"]
