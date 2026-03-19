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
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c(
    'shiny', 'leaflet', 'leaflet.extras', 'sf', 'dplyr', 'purrr',
    'stringr', 'httr', 'geojsonsf', 'jsonlite', 'zip', 'here', 'terra',
    'lubridate', 'tools', 'tibble', 'shinyjs'
), repos='https://cloud.r-project.org')"

# Copy your app code and data into the container
COPY R/naturedatacube_app /srv/shiny-server/naturedatacube_app
COPY R/retrieval_functions /srv/shiny-server/retrieval_functions
COPY data /srv/shiny-server/data

# Expose the port Shiny uses
EXPOSE 3838

# Command to run the app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/naturedatacube_app', host='0.0.0.0', port=3838)"]