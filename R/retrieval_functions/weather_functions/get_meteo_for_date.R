# get_meteo_for_date.R
# Fetches Meteo_data for a single date and station id
# - stationid : station identifier (string or numeric)
# - date      : Date or string. Accepted formats: "YYYYMMDD" or Date object
# - token     : API token

get_meteo_for_date <- function(stationid, date, token, page_size = 500, page_offset = 0, output_epsg = "4326") {
  if (inherits(date, "Date")) {
    date_str <- format(date, "%Y%m%d")
  } else {
    # allow "YYYY-MM-DD" or "YYYYMMDD"
    date_try <- gsub("-", "", as.character(date))
    if (!grepl("^\\d{8}$", date_try)) stop("date must be Date or 'YYYYMMDD' / 'YYYY-MM-DD' string")
    date_str <- date_try
  }
  
  # Try two param names because examples used both stationid and meteostation
  params1 <- c(output_epsg = output_epsg, stationid = as.character(stationid), date = date_str,
               page_size = as.character(page_size), page_offset = as.character(page_offset))
  myurl1 <- ndc_url(option = "Meteo_data", params = params1)
  
  myres <- tryCatch(ndc_get(url = myurl1, token = token), error = function(e) NULL)
  
  if (is.null(myres) || length(myres$features) == 0) {
    # try alternative param name
    params2 <- c(output_epsg = output_epsg, meteostation = as.character(stationid), date = date_str,
                 page_size = as.character(page_size), page_offset = as.character(page_offset))
    myurl2 <- ndc_url(option = "Meteo_data", params = params2)
    myres <- tryCatch(ndc_get(url = myurl2, token = token), error = function(e) NULL)
  }
  
  if (is.null(myres) || length(myres$features) == 0) {
    warning("No meteo data returned for station ", stationid, " on date ", date_str)
    return(NULL)
  }
  
  out_sf <- tryCatch(geojson_sf(toJSON(myres, auto_unbox = TRUE)),
                     error = function(e) stop("Failed to convert meteo result: ", e$message))
  out_sf
}
