# Get hourly weather data from KNMI stations
# Created     : 1 September 2025
# Last update : 19 December 2025

# Script to retrieve hourly weather data from KNMI stations for a shapefile with a polygon.
# It returns all data for all weather stations within the polygon and if there are 0 stations within the polygon it will return data from the closest weather station (based on the centroid).

# Partly a code adaption from KNMIr package (from Bart van Hest) https://github.com/bvhest/KNMIr
# And information used from: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script
# Hourly weather data from: https://www.daggegevens.knmi.nl/klimatologie/uurgegevens
# KNMI weather station data from: https://dataplatform.knmi.nl/dataset/waarneemstations-csv-1-0
# Request the Open data API key here: https://developer.dataplatform.knmi.nl/apis

# Load packages ----------------------------------------------------------------

library(geosphere)
library(here)
library(sf)
library(tidyverse)

# Arguments --------------------------------------------------------------------

# area            : polygon for which to retrieve weather stations with weather data (shapefile (.shp) with one polygon).
# start_date_time : start date and hour of period for which to retrieve data. Date format: 'YYYY', 'YYYYMM', 'YYYYMMDD' or 'YYYYMMDDHH'.
# end_date_time   : end date and hour of period for which to retrieve data. Date format: 'YYYY', 'YYYYMM', 'YYYYMMDD' or 'YYYYMMDDHH'.
# weather_var     : weather variables for which to retrieve data, set to "ALL" to receive all of them (default). See KNMI list of variables for the names. Use format: c("FG", "TX").
# api_key         : anonymous or user specific api key to access the KNMI data about the weather stations (request the Open data API key here: https://developer.dataplatform.knmi.nl/apis). Character string.

# Function to retrieve hourly weather data from KNMI ---------------------------

get_hourly_data_knmi <- function(area,
                                 start_date_time,
                                 end_date_time,
                                 weather_var = "ALL",
                                 api_key) {
  
  # Source additional functions that are called later in this function
  source(here::here("R/retrieval_functions/KNMI/get_stations_knmi.R"))
  source(here::here("R/retrieval_functions/KNMI/rename_columns_knmi.R"))
  
  # Check that all inputs are in the correct format to ensure data can be retrieved
  if (!is.character(start_date_time) || !is.character(end_date_time) ||
      stringr::str_length(start_date_time) %% 2 == 1 | stringr::str_length(end_date_time) %% 2 == 1 ||
      !grepl("^\\d+$", start_date_time) || !grepl("^\\d+$", end_date_time)) {
    stop("The values for 'start_date_time' and 'end_date_time' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM', 'YYYYMMDD' or 'YYYYMMDDHH'.")
  } else {
    if (stringr::str_length(start_date_time) == 8) {
      start_date_time <- paste0(start_date_time, "01")
    } else if (stringr::str_length(start_date_time) == 6) {
      start_date_time <- paste0(start_date_time, "0101")
    } else if (stringr::str_length(start_date_time) == 4) {
      start_date_time <- paste0(start_date_time, "010101")
    } else if (stringr::str_length(start_date_time) == 2) {
      stop("The values for 'start_date_time' and 'end_date_time' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM', 'YYYYMMDD' or 'YYYYMMDDHH'.")
    } else if (stringr::str_length(start_date_time) > 10) {
      stop("The values for 'start_date_time' and 'end_date_time' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM', 'YYYYMMDD' or 'YYYYMMDDHH'.")
    }
    if (stringr::str_length(end_date_time) == 8) {
      end_date_time <- paste0(end_date_time, "24")
    } else if (stringr::str_length(end_date_time) == 6) {
      end_date_time <- paste0(end_date_time, "3124")
    } else if (stringr::str_length(end_date_time) == 4) {
      end_date_time <- paste0(end_date_time, "123124")
    } else if (stringr::str_length(end_date_time) == 2) {
      stop("The values for 'start_date_time' and 'end_date_time' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM', 'YYYYMMDD' or 'YYYYMMDDHH'.")
    } else if (stringr::str_length(end_date_time) > 10) {
      stop("The values for 'start_date_time' and 'end_date_time' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM', 'YYYYMMDD' or 'YYYYMMDDHH'.")
    }
  }
  if (as.numeric(end_date_time) < as.numeric(start_date_time)) {
    stop("The 'start_date_time' cannot be later than the 'end_date_time'. Please change the dates.")
  }
  if ((!inherits(area, "sf")) || (nrow(area) != 1)) {
    stop("Please provide a shapefile with one polygon.")
  }
  geom_type <- sf::st_geometry_type(area, by_geometry = FALSE)
  if ((!(geom_type == "POLYGON"))) {
    stop("Please provide a shapefile with one polygon.")
  }
  
  # Get station info
  stations_knmi <- get_stations_knmi(api_key)
  
  # Fix polygon
  area <- sf::st_make_valid(area)
  
  # Start and end dates and times in ymd_h format
  start_date_time_ymd_h <- lubridate::ymd_h(start_date_time)
  end_date_time_ymd_h <- lubridate::ymd_h(end_date_time)
  today <- Sys.Date()
  
  first_date_with_data <- stations_knmi %>%
    arrange(startdatum) %>%
    slice_head() %>%
    mutate(lubridate::ymd(startdatum))
  
  if ((!(start_date_time_ymd_h < today)) || (!(start_date_time_ymd_h > first_date_with_data$startdatum))) {
    stop("Please choose a ‘start_date_time’ between 25 December 1855 and today.")
  }
  
  # Select stations within time period and the area
  stations <- stations_knmi %>%
    filter((!(einddatum < start_date_time_ymd_h)) %>% tidyr::replace_na(TRUE)) %>%
    filter(startdatum <= end_date_time_ymd_h)
  stations_sf <- sf::st_as_sf(stations, coords = c("lon", "lat"), crs = 4326)
  stations_inside <- sf::st_within(stations_sf, area, sparse = FALSE)[,1]
  stations_inside <- stations_sf[stations_inside, ]
  weather_stations <- stations_inside$stationID
  
  if (dim(stations_inside)[1] == 0) {
    # If there are no weather stations in area, return data of the closest one 
    # Based on the centroid of area
    suppressWarnings(centroid_area <- sf::st_centroid(area))
    centroid_coords <- c(lon = sf::st_coordinates(centroid_area)[1], lat = sf::st_coordinates(centroid_area)[2])
    
    # Calculate which weather station is closest to the centroid of area
    closest_station <- stations %>%
      dplyr::mutate(distance = geosphere::distHaversine(centroid_coords, cbind(stations$lon, stations$lat))) %>%
      dplyr::arrange(distance) %>%
      dplyr::slice_head()
    
    # Check if variable(s) are available
    var_knmi <-  c("STN", "YYYYMMDD", "HH", "DD", "FH", "FF", "FX", "T", "T10N", 
                   "TD", "SQ", "Q", "DR", "RH", "P", "VV", "N", "U", "WW", "IX", 
                   "M", "R", "S", "O", "Y")
    
    for (variable in weather_var){
      if ((!("ALL" %in% weather_var)) & (!(variable %in% var_knmi))) {
        stop("Atleast one of the weather variables you provided does not exist, choose from the list of KNMI hourly data variables.")
      }
    }
    
    # Store correct column names
    if ("ALL" %in% weather_var) {
      colnames_knmi <- var_knmi
    } else {
      colnames_knmi <- c("STN", "YYYYMMDD", "HH", weather_var)
    }
    
    # Change weather variable format "c(x1, x2, x3)" to "x1:x2:x3" for retrieving from KNMI
    if (length(weather_var > 1)) {
      weather_var <- paste(weather_var, collapse = ":")
    }
    
    # Get data of this closest station
    baseURL <- "https://www.daggegevens.knmi.nl/klimatologie/uurgegevens"
    URL <- paste0(baseURL, "?start=", start_date_time, "&end=", end_date_time, "&stns=", closest_station$stationID, "&vars=", weather_var)
    
    df_hourly <-
      readr::read_csv(URL, col_names = FALSE, comment = "#", show_col_types = FALSE) %>%
      dplyr::as_tibble()
    
    # Give the correct column names to the retrieved data
    colnames(df_hourly) <- colnames_knmi
    
    # Rename for readability
    df_hourly <- rename_columns_knmi(df_hourly, "hourly")
    
    # Add locations and names of weather stations to data frame
    df_hourly <- dplyr::left_join(df_hourly, stations, by = join_by(stationID))
    
    # Output
    message("There are 0 weather stations in your area. Weather data of the closest station is provided.")
    return(df_hourly)
    
  } else {
    var_knmi <-  c("STN", "YYYYMMDD", "HH", "DD", "FH", "FF", "FX", "T", "T10N", 
                   "TD", "SQ", "Q", "DR", "RH", "P", "VV", "N", "U", "WW", "IX", 
                   "M", "R", "S", "O", "Y")
    
    for (variable in weather_var){
      if ((!("ALL" %in% weather_var)) & (!(variable %in% var_knmi))) {
        stop("Atleast one of the weather variables you provided does not exist, choose from the list of KNMI hourly data variables.")
      }
    }
    
    # Store correct column names
    if ("ALL" %in% weather_var) {
      colnames_knmi <- var_knmi
    } else {
      colnames_knmi <- c("STN", "YYYYMMDD", "HH", weather_var)
    }
    
    # Change weather variable format "c(x1, x2, x3)" to "x1:x2:x3" for retrieving from KNMI
    if (length(weather_var > 1)) {
      weather_var <- paste(weather_var, collapse = ":")
    }
    
    # Return data from all the weather stations in the area
    baseURL <- "https://www.daggegevens.knmi.nl/klimatologie/uurgegevens"
    URL <- paste0(baseURL, "?start=", start_date_time, "&end=", end_date_time, "&stns=", weather_stations, "&vars=", weather_var)
    
    df_hourly <-
      readr::read_csv(URL, col_names = FALSE, comment = "#", show_col_types = FALSE) %>%
      dplyr::as_tibble()
    
    # Give the correct column names to the retrieved data
    colnames(df_hourly) <- colnames_knmi
    
    # Rename for readability
    df_hourly <- rename_columns_knmi(df_hourly, "hourly")
    
    # Add locations and names of weather stations to data frame
    df_hourly <- dplyr::left_join(df_hourly, stations, by = join_by(stationID))
    
    # Output
    message("There are 1 or more weather stations in your area. Weather data of this or these stations is provided.")
    return(df_hourly)
    
  } 
}

