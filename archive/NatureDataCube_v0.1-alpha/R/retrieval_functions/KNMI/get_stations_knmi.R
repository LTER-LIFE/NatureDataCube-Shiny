# Get weather station info from KNMI 
# Created     : 1 September 2025
# Last update : 17 December 2025

# Script to retrieve weather station info to be used when requesting daily or hourly weather data from KNMI

# Data from: https://dataplatform.knmi.nl/dataset/waarneemstations-csv-1-0
# Request the Open data API key here: https://developer.dataplatform.knmi.nl/apis

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(httr2)

# Arguments --------------------------------------------------------------------

# api_key   : necessary API key to access the KNMI data (request the Open data API key here: https://developer.dataplatform.knmi.nl/apis).

# Function to retrieve weather station info from KNMI --------------------------

get_stations_knmi <- function(api_key) {
  
  repeat({# If retrieving data with Open API fails, try again
    
    # List of available files
    req <- httr2::request("https://api.dataplatform.knmi.nl/open-data/v1/datasets/waarneemstations_csv/versions/1.0/files") %>%
      httr2::req_headers(Authorization = api_key) %>%
      httr2::req_perform()
  
    file_list <- httr2::resp_body_json(req)
  
    # If unsuccessful, print message (and try again)
    if(is.null(file_list$files)) message(paste0("KNMI Open API failed to retrieve station information from KNMI"))

    files <- file_list$files  
    files_df <- as.data.frame(do.call(rbind, lapply(files, as.data.frame)))
  
    # Find latest file
    files_df$lastModified <- as.POSIXct(files_df$lastModified, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    latest_file <- files_df[which.max(files_df$lastModified), ]
  
    # Request and download latest file
    file_url <- paste0(
      "https://api.dataplatform.knmi.nl/open-data/v1/datasets/waarneemstations_csv/versions/1.0/files/",
      latest_file$filename,
      "/url"
      )
    latest_resp <- httr2::request(file_url) %>%
      httr2::req_headers(Authorization = api_key) %>%
      httr2::req_perform() %>%
      httr2:: resp_body_json()
    
    # If unsuccessful, print message (and try again)
    if(is.null(latest_resp$temporaryDownloadUrl)) message(paste0("KNMI Open API failed to retrieve station information from KNMI"))
  
    download_url <- latest_resp$temporaryDownloadUrl
    stations_data <- read.csv(download_url)
  
    colnames(stations_data) <-
      c(
        "stationID", "WSI", "startdatum", "einddatum", "station_naam", "type", 
        "hoogte", "pos_x", "pos_y", "lat", "lon"
        )
    
    # If successful, end
    if(!is.null(latest_resp$temporaryDownloadUrl)) break()
  
    })
  
  return(stations_data)
}



