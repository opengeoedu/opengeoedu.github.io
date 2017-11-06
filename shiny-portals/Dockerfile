FROM rocker/geospatial:3.4.2
RUN apt-get update && apt-get install -y \
	gdebi-core && \
	wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.5.872-amd64.deb && \
	gdebi -n shiny-server-1.5.5.872-amd64.deb

RUN ["install2.r", "-r 'https://cloud.r-project.org'", "RColorBrewer", "htmltools", "flexdashboard", "Rcpp", "knitr", "magrittr", "xtable", "R6", "stringr", "yaml", "rprojroot", "digest", "shiny", "htmlwidgets", "evaluate", "mime", "rmarkdown", "stringi", "backports", "jsonlite", "markdown", "httpuv", "remotes"]
RUN ["installGithub.r", "bhaskarvk/leaflet.extras@23de9ea", "rstudio/crosstalk@d8f29eb", "rstudio/DT@ea15ab9", "rstudio/leaflet@d489e2c", "hadley/lazyeval@c155c3d"]

EXPOSE 80

COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
CMD ["/usr/bin/shiny-server.sh"]
