#!/bin/bash
docker run --rm -d -p 85:80 \
	-v $(pwd):/srv/shiny-server \
	-v $(pwd)/shiny-portals/shinylog:/var/log/shiny-server/ \
	-v $(pwd)/shiny-portals/shiny-server.conf:/etc/shiny-server/shiny-server.conf \
	--name ogeportal \
	-u `id -u $USER` \
	shinyapp:2
