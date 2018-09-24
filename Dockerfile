FROM openanalytics/r-base

MAINTAINER Todd Bleess "todd.bleess@state.co.us"

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# packages needed for basic shiny functionality
RUN R -e "install.packages(c('devtools', 'shiny', 'rmarkdown', 'scales', 'readxl', 'readr', 'shinydashboard', 'rgdal', 'raster', 'stringr', 'tidyverse', 'knitr', 'kableExtra', 'RPostgeSQL', 'shinyjs', 'VennDiagram', 'geojsonio', 'gridExtra', 'ggthemes', 'maptools'))" && \
    R -e "devtools::install_github('ColoradoDemography/ProfileDashboard')" && \
    R -e "devtools::install_github('ColoradoDemography/codemogAPI')" && \
	R -e "devtools::install_github('ColoradoDemography/codemogLib')" && \
    R -e "devtools::install_github('ColoradoDemography/codemogProfile')"

EXPOSE 3838

RUN R -e "shiny::runApp(getwd())"
