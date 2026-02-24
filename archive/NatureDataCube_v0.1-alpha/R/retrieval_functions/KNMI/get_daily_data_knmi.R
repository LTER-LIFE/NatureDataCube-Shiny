# Get daily weather data from KNMI stations
# Created     : 1 September 2025
# Last update : 19 December 2025

# Script to retrieve daily weather data from KNMI stations for a shapefile with a polygon.
# It returns all data for all weather stations within the polygon and if there are 0 stations within the polygon it will return data from the closest weather station (based on the centroid).

# Partly a code adaption from KNMIr package (from Bart van Hest) https://github.com/bvhest/KNMIr
# And information used from: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script
# Daily weather data from: https://www.daggegevens.knmi.nl/klimatologie/daggegevens
# KNMI weather station data from: https://dataplatform.knmi.nl/dataset/waarneemstations-csv-1-0
# Request the Open data API key here: https://developer.dataplatform.knmi.nl/apis

# Load packages ----------------------------------------------------------------

library(geosphere)
library(here)
library(sf)
library(tidyverse)

# Arguments --------------------------------------------------------------------

# area        : polygon for which to retrieve weather stations with weather data (shapefile (.shp) with one polygon).
# start_date  : start date of period for which to retrieve data. Date format: 'YYYY', 'YYYYMM' or 'YYYYMMDD'.
# end_date    : end date of period for which to retrieve data. Date format: 'YYYY', 'YYYYMM' or 'YYYYMMDD'.
# weather_var : weather variables for which to retrieve data, set to "ALL" to receive all of them (default). See KNMI list of variables for the names. Use format: c("FG", "TX").
# api_key     : anonymous or user specific api key to access the KNMI data about the weather stations (request the Open data API key here: https://developer.dataplatform.knmi.nl/apis). Character string.

# Function to get daily weather data from KNMI ---------------------------------

get_daily_data_knmi <- function(area,
                                start_date,
                                end_date,
                                weather_var = "ALL",
                                api_key) {
  
  # Source additional functions that are called later in this function
  source(here::here("R/retrieval_functions/KNMI/get_stations_knmi.R"))
  source(here::here("R/retrieval_functions/KNMI/rename_columns_knmi.R"))
  
  # Check that all inputs are in the correct format to ensure data can be retrieved
  if (!is.character(start_date) || !is.character(end_date) ||
      stringr::str_length(start_date) %% 2 == 1 | stringr::str_length(end_date) %% 2 == 1 ||
      !grepl("^\\d+$", start_date) || !grepl("^\\d+$", end_date)) {
    stop("The values for 'start_date' and 'end_date' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM' or 'YYYYMMDD'.")
  } else {
    if (stringr::str_length(start_date) == 6) {
      start_date <- paste0(start_date, "01")
    } else if (stringr::str_length(start_date) == 4) {
      start_date <- paste0(start_date, "0101")
    } else if (stringr::str_length(start_date) == 2) {
      stop("The values for 'start_date' and 'end_date' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM' or 'YYYYMMDD'.")
    } else if (stringr::str_length(start_date) > 8) {
      stop("The values for 'start_date' and 'end_date' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM' or 'YYYYMMDD'.")
    }
    if (stringr::str_length(end_date) == 6) {
      end_date <- paste0(end_date, "31")
    } else if (stringr::str_length(end_date) == 4) {
      end_date <- paste0(end_date, "1231")
    } else if (stringr::str_length(end_date) == 2) {
      stop("The values for 'start_date' and 'end_date' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM' or 'YYYYMMDD'.")
    } else if (stringr::str_length(end_date) > 8) {
      stop("The values for 'start_date' and 'end_date' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM' or 'YYYYMMDD'.")
    }
  }
  if (as.numeric(end_date) < as.numeric(start_date)) {
    stop("The 'start_date' cannot be later than the 'end_date'. Please change the dates.")
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
  
  # Start and end dates in ymd format
  start_date_ymd <- lubridate::ymd(start_date)
  end_date_ymd <- lubridate::ymd(end_date)
  today <- Sys.Date()
  
  first_date_with_data <- stations_knmi %>%
    arrange(startdatum) %>%
    slice_head() %>%
    mutate(lubridate::ymd(startdatum))
  
  if ((!(start_date_ymd < today)) || (!(start_date_ymd > first_date_with_data$startdatum))) {
    stop("Please choose a ‘start_date’ between 25 December 1855 and today.")
  }
  
  # Select stations within time period and the area
  stations <- stations_knmi %>%
    dplyr::filter((!(einddatum < start_date_ymd)) %>% tidyr::replace_na(TRUE)) %>%
    dplyr::filter(startdatum <= end_date_ymd)
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
    var_knmi <-  c("STN", "YYYYMMDD", "DDVEC", "FHVEC", "FG", "FHX", "FHXH", 
                   "FHN", "FHNH", "FXX", "FXXH", "TG", "TN", "TNH", "TX", "TXH", 
                   "T10N", "T10NH", "SQ", "SP", "Q", "DR", "RH", "RHX", "RHXH", 
                   "PG", "PX", "PXH", "PN", "PNH", "VVN", "VVNH", "VVX", "VVXH", 
                   "NG", "UG", "UX", "UXH", "UN", "UNH", "EV24")
    
    for (variable in weather_var){
      if ((!("ALL" %in% weather_var)) & (!(variable %in% var_knmi))) {
        stop("(One of) the weather variable(s) you provided does not exist. Choose from the list of KNMI daily data variables.")
      }
    }
    
    # Store correct column names
    if ("ALL" %in% weather_var) {
      colnames_knmi <- var_knmi
    } else {
      colnames_knmi <- c("STN", "YYYYMMDD", weather_var)
    }
    
    # Change weather variable format "c(x1, x2, x3)" to "x1:x2:x3" for retrieving from KNMI
    if (length(weather_var > 1)) {
      weather_var <- paste(weather_var, collapse = ":")
    }
    
    # Get data of this closest station
    baseURL <- "https://www.daggegevens.knmi.nl/klimatologie/daggegevens"
    URL <- paste0(baseURL, "?start=", start_date, "&end=", end_date, "&stns=", closest_station$stationID, "&vars=", weather_var)
    
    df_daily <-
      readr::read_csv(URL, col_names = TRUE, comment = "#", show_col_types = FALSE) %>%
      dplyr::as_tibble()
    
    # Give the correct column names to the retrieved data
    colnames(df_daily) <- colnames_knmi
    
    # Rename for readability
    df_daily <- rename_columns_knmi(df_daily, "daily")
    
    # Add locations and names of weather stations to data frame
    df_daily <- dplyr::left_join(df_daily, stations, by = join_by(stationID))
    
    # Output
    message("There are 0 weather stations in your area. Weather data of the closest station is provided.")
    return(df_daily)
    
  } else {
    var_knmi <-  c("STN", "YYYYMMDD", "DDVEC", "FHVEC", "FG", "FHX", "FHXH", 
                   "FHN", "FHNH", "FXX", "FXXH", "TG", "TN", "TNH", "TX", "TXH", 
                   "T10N", "T10NH", "SQ", "SP", "Q", "DR", "RH", "RHX", "RHXH", 
                   "PG", "PX", "PXH", "PN", "PNH", "VVN", "VVNH", "VVX", "VVXH", 
                   "NG", "UG", "UX", "UXH", "UN", "UNH", "EV24")
    
    for (variable in weather_var){
      if ((!("ALL" %in% weather_var)) & (!(variable %in% var_knmi))) {
        stop("Atleast one of the weather variables you provided does not exist, choose from the list of KNMI daily data variables.")
      }
    }
    
    # Store correct column names
    if ("ALL" %in% weather_var) {
      colnames_knmi <- var_knmi
    } else {
      colnames_knmi <- c("STN", "YYYYMMDD", weather_var)
    }
    
    # Change weather variable format "c(x1, x2, x3)" to "x1:x2:x3" for retrieving from KNMI
    if (length(weather_var > 1)) {
      weather_var <- paste(weather_var, collapse = ":")
    }
    
    # Return data from all the weather stations in the area
    baseURL <- "https://www.daggegevens.knmi.nl/klimatologie/daggegevens"
    URL <- paste0(baseURL, "?start=", start_date, "&end=", end_date, "&stns=", weather_stations, "&vars=", weather_var)
    
    df_daily <-
      readr::read_csv(URL, col_names = FALSE, comment = "#", show_col_types = FALSE) %>%
      dplyr::as_tibble()
    
    # Give the correct column names to the retrieved data
    colnames(df_daily) <- colnames_knmi
    
    # Rename for readability
    df_daily <- rename_columns_knmi(df_daily, "daily")
    
    # Add locations and names of weather stations to data frame
    df_daily <- dplyr::left_join(df_daily, stations, by = join_by(stationID))
    
    # Output
    message("There are 1 or more weather stations in your area. Weather data of this or these stations is provided.")
    return(df_daily)
    
  } 
}
