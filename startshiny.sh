#!/bin/bash
docker run --rm -d -p 85:80 \
	-v $(pwd):/srv/shiny-server \
	-v $(pwd)/shiny-portals/shinylog:/var/log/shiny-server/ \
	--name ogeportal \
	-u `id -u $USER` \
	shinyapp
