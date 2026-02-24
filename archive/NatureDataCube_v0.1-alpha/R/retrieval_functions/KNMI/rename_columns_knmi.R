# Rename data columns from KNMI
# Created     : 1 September 2025
# Last update : 17 December 2025

# Script to rename KNMI data columns for increased readability, for the daily and hourly variables (in Dutch!)

# Partly a code adaption/ inspiration/ more descriptive variable names from KNMIr package (from Bart van Hest) https://github.com/bvhest/KNMIr
# For full description of each variable see "knmi_variable_names.csv"

# Load packages ----------------------------------------------------------------

library(here)
library(tidyverse)

# Arguments --------------------------------------------------------------------

# knmi_data : the data frame for which the columns need to be renamed.
# time      : "daily" or "hourly". Indicates which data set it belongs to, because they have different columns.

# Function to rename KNMI data columns -----------------------------------------

rename_columns_knmi <- function(knmi_data, time){
  colnames <- read.csv(here::here("data/knmi_variable_names.csv"), header = TRUE, sep = ";")
  colnames_daily <- colnames %>%
    filter(dataset == "daily")
  colnames_hourly <- colnames %>%
    filter(dataset == "hourly")
  
  if (time == "daily") {
    col_names_knmi <- colnames_daily$variable_short
    col_names_descriptive <- colnames_daily$variable_long
    
    column_names <- setNames(col_names_descriptive, col_names_knmi)
    present_cols <- intersect(names(knmi_data), names(column_names))
    safe_map <- column_names[present_cols]
    
    knmi_renamed <- knmi_data %>%
      dplyr::rename_with(~ safe_map[.x], .cols = all_of(present_cols))
    
    return(knmi_renamed)
    
    } else if (time == "hourly") {
    col_names_knmi <- colnames_hourly$variable_short
    col_names_descriptive <- colnames_hourly$variable_long
    
    column_names <- setNames(col_names_descriptive, col_names_knmi)
    present_cols <- intersect(names(knmi_data), names(column_names))
    safe_map <- column_names[present_cols]
    
    knmi_renamed <- knmi_data %>%
      dplyr::rename_with(~ safe_map[.x], .cols = all_of(present_cols))
    
    return(knmi_renamed)
    
    } else {
      print("Please use 'daily' or 'hourly'.")
    }
}

