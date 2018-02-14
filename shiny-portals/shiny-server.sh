#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server
# Pre-render data and generate export files
cd /srv/shiny-server
Rscript "R/generate_all_data.R"
exec shiny-server >> /var/log/shiny-server.log 2>&1
