# Get daily grid weather data from KNMI
# Created     : 3 September 2025
# Last update : 17 December 2025

# Script to retrieve daily grid weather data from KNMI for a shapefile with a polygon.

# Partly a code adaption from Stefan Vriend: https://github.com/LTER-LIFE/VeluweProtoDT/blob/main/R/08_temperature_retrieveData-KNMI-EDR-API.R
# Data from Environmental Data Retrieval (EDR) from KNMI: https://developer.dataplatform.knmi.nl/edr-api

# Load packages ----------------------------------------------------------------

library(httr2)
library(jsonlite)
library(sf)
library(terra)
library(tidyverse)

# Arguments --------------------------------------------------------------------

# area        : polygon for which to retrieve weather data (shapefile (.shp) with one polygon).
# start_date  : start date of period for which to retrieve data. Date format: "yyyymmdd".
# end_date    : end date of period for which to retrieve data. Date format: "yyyymmdd".
# weather_var : weather variable(s) of interest. Choose from: "mean temperature", "max temperature", "min temperature" or "precipitation". When choosing multiple, use format: c("mean temperature", "max temperature")". Default: all four variables.
# api_key     : user-specific KNMI EDR API key (request the EDR API key here: https://developer.dataplatform.knmi.nl/apis). Character string.

# Function to get daily grid weather data from KNMI ----------------------------

get_daily_grid_data_knmi <- function(area,
                                     start_date,
                                     end_date,
                                     weather_var = c("mean temperature", "min temperature", "max temperature", "precipitation"),
                                     api_key) {
  
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
  
  knmi_var_lookup <- tibble::tibble(
    collection = c("Tg1", "Tx1", "Tn1", "Rd1"),
    parameter = c("temperature", "temperature", "temperature", "precipitation"),
    var_name = c("mean temperature", "max temperature", "min temperature", "precipitation")
  )
  
  for (variable in weather_var){
    if (!(variable %in% knmi_var_lookup$var_name)) {
      stop("Atleast one of the weather variables you provided does not exist, choose from: 'mean temperature', 'max temperature', 'min temperature', or 'precipitation'.")
    }
  }
  
  start_date_ymd <- lubridate::ymd(start_date)
  end_date_ymd <- lubridate::ymd(end_date)
  today <- Sys.Date()
  
  if ((!(start_date_ymd < today))) {
    stop("Please choose a ‘start_date’ before the date of today.")
  }
  
  # Make bounding box from polygon
  area <- sf::st_make_valid(area)
  bbox <- sf::st_bbox(area)
  
  results <- list()
  
  repeat({# If retrieving data with EDR API fails, try again
    
    for (var in weather_var) {
      collection <- knmi_var_lookup |> dplyr::filter(var_name == var) |> dplyr::pull("collection")
      parameter  <- knmi_var_lookup |> dplyr::filter(var_name == var) |> dplyr::pull("parameter")
      
      url <- paste0("https://api.dataplatform.knmi.nl/edr/v1/collections/",
                    collection, 
                    "/cube?f=CoverageJSON&bbox=", paste(bbox, collapse = "%2C"),
                    "&datetime=",
                    start_date_ymd, "T",
                    stringr::str_replace_all("00:00:00", ":", "%3A"), "Z%2F",
                    end_date_ymd, "T",
                    stringr::str_replace_all("23:59:59", ":", "%3A"), "Z",
                    "&parameter-name=", parameter)
    
      knmi_data <- httr2::request(url) |>
        httr2::req_headers(Authorization = api_key) |>
        httr2::req_perform() |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON()
    
      # If unsuccessful, print message (and try again)
      if(is.null(knmi_data$domain)) message(paste0("KNMI EDR API failed to retrieve daily grid data from KNMI"))
      
      x <- knmi_data$domain$axes$x$values
      y <- knmi_data$domain$axes$y$values
      z <- knmi_data$ranges[[parameter]]$values
      shape <- knmi_data$ranges[[parameter]]$shape  
    
      suppressWarnings(z_matrix <- matrix(z, nrow = shape[2], ncol = shape[3], byrow = TRUE))
      z_matrix <- z_matrix[nrow(z_matrix):1, ]
      
      # Create raster
      r <- terra::rast(z_matrix)
      ext(r) <- terra::ext(min(x), max(x), min(y), max(y))
      crs(r) <- "EPSG:4326"
      
      # Only select what is within polygon instead of bounding box
      r <- terra::mask(r, area)
    
      results[[var]] <- r
      }
   
     # If successful, end
    if(!is.null(knmi_data$domain)) break()
    
    })
  
  # Output
  message("Daily grid data is provided for your variables.")
  return(results)
  
}




