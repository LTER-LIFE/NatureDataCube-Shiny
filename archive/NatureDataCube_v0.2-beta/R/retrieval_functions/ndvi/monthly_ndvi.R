download_avg_ndvi_month <- function(poly, year, month, epsg = 32631) {
  
  library(sf)
  library(terra)
  
  # ---- build month date range ----
  start_date <- as.Date(sprintf("%04d-%02d-01", year, month))
  end_date   <- seq(start_date, length = 2, by = "month")[2] - 1
  
  message(
    "Processing ",
    format(start_date, "%B %Y"),
    " (", start_date, " to ", end_date, ")"
  )
  
  # ---- transform polygon + bbox ----
  poly_utm <- st_transform(poly, epsg)
  bb <- st_bbox(poly_utm)
  
  xmin <- bb["xmin"]
  xmax <- bb["xmax"]
  ymin <- bb["ymin"]
  ymax <- bb["ymax"]
  
  # ---- WCS base ----
  wcs_base <- "https://data.groenmonitor.nl/geoserver/wcs?"
  format_param <- "image/tiff"
  
  files <- character(0)
  
  # ---- download loop ----
  dates <- seq(start_date, end_date, by = "day")
  dates_str <- format(dates, "%Y%m%d")
  
  for (dstr in dates_str) {
    
    coverage_id <- paste0("groenmonitor__ndvi_", dstr)
    
    myurl <- paste0(
      wcs_base,
      "service=WCS&version=2.0.1&request=GetCoverage",
      "&coverageId=", coverage_id,
      "&subset=E(", xmin, ",", xmax, ")",
      "&subset=N(", ymin, ",", ymax, ")",
      "&format=", format_param
    )
    
    myfile <- paste0("ndvi_", dstr, ".tif")
    
    ok <- tryCatch(
      {
        suppressWarnings(
          download.file(myurl, myfile, mode = "wb", quiet = TRUE)
        )
        TRUE
      },
      error = function(e) FALSE
    )
    
    if (ok) {
      message("Downloaded ", dstr)
      files <- c(files, myfile)
    } else {
      message("Skipping ", dstr, " (not available)")
    }
  }
  
  # ---- nothing downloaded ----
  if (length(files) == 0) {
    message("No NDVI data available for the selected month")
    return(NULL)
  }
  
  message("Downloaded ", length(files), " NDVI layer(s) for the month")
  
  # ---- read + average ----
  r_stack <- rast(files)
  r_mean  <- app(r_stack, mean, na.rm = TRUE)
  
  # Replace NaN with NA
  r_mean[is.nan(r_mean)] <- NA
  
  # ---- check if raster has valid data ----
  vals <- values(r_mean)
  if (all(is.na(vals))) {
    message("All raster cells are empty (no NDVI to display)")
    return(NULL)
  }
  
  names(r_mean) <- paste0("ndvi_mean_", year, sprintf("%02d", month))
  
  return(r_mean)
}
