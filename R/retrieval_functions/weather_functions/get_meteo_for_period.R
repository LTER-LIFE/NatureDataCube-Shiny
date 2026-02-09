# get_meteo_for_period.R
# Fetches Meteo_data for a period for a station
# - meteostation: id (string or numeric)
# - fromdate, todate: Date or "YYYYMMDD" / "YYYY-MM-DD"
# - token

get_meteo_for_period <- function(meteostation, fromdate, todate, token,
                                 page_size = 500, page_offset = 0, output_epsg = "4326") {
  # normalize dates
  fmt_date <- function(d) {
    if (inherits(d, "Date")) return(format(d, "%Y%m%d"))
    ds <- gsub("-", "", as.character(d))
    if (!grepl("^\\d{8}$", ds)) stop("Dates must be Date or 'YYYYMMDD'/'YYYY-MM-DD' strings")
    ds
  }
  from_str <- fmt_date(fromdate)
  to_str   <- fmt_date(todate)
  
  params <- c(output_epsg = output_epsg,
              meteostation = as.character(meteostation),
              fromdate = from_str,
              todate = to_str,
              page_size = as.character(page_size),
              page_offset = as.character(page_offset))
  myurl <- ndc_url(option = "Meteo_data", params = params)
  
  myres <- tryCatch(ndc_get(url = myurl, token = token), error = function(e) NULL)
  
  if (is.null(myres) || length(myres$features) == 0) {
    warning("No meteo data returned for station ", meteostation, " between ", from_str, " and ", to_str)
    return(NULL)
  }
  
  out_sf <- tryCatch(geojson_sf(toJSON(myres, auto_unbox = TRUE)),
                     error = function(e) stop("Failed to convert meteo period result: ", e$message))
  out_sf
}
