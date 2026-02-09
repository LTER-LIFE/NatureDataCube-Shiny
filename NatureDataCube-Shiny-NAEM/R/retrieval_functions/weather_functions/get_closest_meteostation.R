# get_closest_meteostation.R
# Returns: list with stations_sf, closest_station_sf, closest_id, distances
# - polygon_wkt : WKT string (EPSG:4326) of the study area (centroid used)
# - token       : API token (string)
# - page_size/page_offset : paging for stations list

get_closest_meteostation <- function(polygon_wkt,
                                     token,
                                     page_size = 100,
                                     page_offset = 0,
                                     output_epsg = "4326") {
  stopifnot(is.character(polygon_wkt), nzchar(polygon_wkt))
  # Build stations URL
  myurl <- ndc_url(option = "Meteo_stations",
                   params = c(output_epsg = output_epsg,
                              page_size = as.character(page_size),
                              page_offset = as.character(page_offset)))
  # Get features
  myres <- tryCatch(
    ndc_get(url = myurl, token = token),
    error = function(e) stop("Failed to get meteostations: ", e$message)
  )
  
  # Convert to sf
  if (length(myres$features) == 0) {
    stop("No meteostations returned from API.")
  }
  
  stations_sf <- tryCatch(
    geojson_sf(toJSON(myres, auto_unbox = TRUE)),
    error = function(e) stop("Failed to parse stations geojson: ", e$message)
  )
  
  # Compute centroid of input polygon_wkt
  poly_sfc <- tryCatch(st_as_sfc(polygon_wkt, crs = 4326),
                       error = function(e) stop("Invalid WKT polygon: ", e$message))
  poly_centroid <- st_centroid(poly_sfc)
  
  # Distances (meters when CRS is projected; with EPSG:4326 this will be in degrees unless transformed)
  # Transform to a metric projection for distance computation (Netherlands: use EPSG:28992 or use st_distance with geosphere)
  # We'll transform to 3857 (meters) for a reliable metric distance if possible.
  suppressWarnings({
    stations_m <- st_transform(stations_sf, 3857)
    centroid_m <- st_transform(poly_centroid, 3857)
  })
  
  dists <- as.numeric(st_distance(centroid_m, stations_m)) # vector
  closest_idx <- which.min(dists)
  
  closest_station_sf <- stations_sf[closest_idx, , drop = FALSE]
  closest_id <- (closest_station_sf$meteostationid %||% closest_station_sf$stationid %||% closest_station_sf$station_id)
  # The %||% is not base R; to be safe:
  if (is.null(closest_id)) {
    # try several possible names
    possible_names <- c("meteostationid", "meteostation_id", "stationid", "station_id", "id")
    found <- NULL
    for (nm in possible_names) {
      if (nm %in% names(closest_station_sf)) { found <- closest_station_sf[[nm]]; break }
    }
    closest_id <- found
  }
  
  list(
    stations_sf = stations_sf,
    closest_station = closest_station_sf,
    closest_id = as.character(closest_id),
    distances = dists
  )
}
