# Use a base image with R and Shiny
FROM rocker/shiny:latest

# Install system libraries needed by some R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libgeos-dev \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages (leaflet.extras from GitHub — not on CRAN for R 4.5+)
RUN R -e "install.packages(c( \
    'shiny', 'leaflet', 'sf', 'dplyr', 'purrr', \
    'stringr', 'httr', 'geojsonsf', 'jsonlite', 'zip', 'here', 'terra', \
    'lubridate', 'tools', 'tibble', 'shinyjs', 'remotes' \
), repos='https://packagemanager.posit.co/cran/__linux__/noble/latest')" && \
    R -e "remotes::install_github('bhaskarvk/leaflet.extras')"

# Copy app code and data — retrieval_functions must land inside the app dir
# so that here::here() resolves paths correctly at runtime
COPY R/naturedatacube_app  /srv/shiny-server/naturedatacube_app
COPY R/retrieval_functions /srv/shiny-server/naturedatacube_app/R/retrieval_functions
COPY data                  /srv/shiny-server/naturedatacube_app/data

# Expose the port Shiny uses
EXPOSE 3838

# Command to run the app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/naturedatacube_app', host='0.0.0.0', port=3838)"]