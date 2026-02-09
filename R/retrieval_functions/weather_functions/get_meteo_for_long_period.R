get_meteo_for_long_period <- function(meteostation,
                                      fromdate,
                                      todate,
                                      token,
                                      by_days = 7,
                                      page_size = 500,
                                      output_epsg = "4326",
                                      sleep_sec = 0) {
  
  ranges <- split_date_range(fromdate, todate, by_days)
  results <- list()
  
  for (i in seq_len(nrow(ranges))) {
    
    message(
      sprintf(
        "Downloading %s → %s (%d/%d)",
        ranges$from[i],
        ranges$to[i],
        i,
        nrow(ranges)
      )
    )
    
    res <- get_meteo_for_period(
      meteostation = meteostation,
      fromdate     = ranges$from[i],
      todate       = ranges$to[i],
      token        = token,
      page_size    = page_size,
      output_epsg  = output_epsg
    )
    
    if (!is.null(res)) {
      results[[length(results) + 1]] <- res
    }
    
    if (sleep_sec > 0) {
      Sys.sleep(sleep_sec)
    }
  }
  
  if (length(results) == 0) {
    warning("No meteo data returned for the full requested period")
    return(NULL)
  }
  
  # Combine all sf objects
  do.call(rbind, results)
}
