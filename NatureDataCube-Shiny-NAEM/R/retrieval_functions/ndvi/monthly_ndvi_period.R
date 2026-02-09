download_avg_ndvi_stack <- function(poly, start_year, start_month, end_year, end_month, epsg = 32631) {
  library(sf); library(terra); library(lubridate)
  
  start_date <- as.Date(sprintf("%04d-%02d-01", start_year, start_month))
  end_date   <- as.Date(sprintf("%04d-%02d-01", end_year, end_month))
  month_seq  <- seq(start_date, end_date, by = "month")
  
  poly_utm <- st_transform(poly, epsg)
  bb <- st_bbox(poly_utm)
  xmin <- bb["xmin"]; xmax <- bb["xmax"]; ymin <- bb["ymin"]; ymax <- bb["ymax"]
  
  wcs_base <- "https://data.groenmonitor.nl/geoserver/wcs?"
  format_param <- "image/tiff"
  
  raster_list <- list()
  
  # create unique temp dir for daily tiles
  tmpdir <- file.path(tempdir(), paste0("ndvi_temp_", as.integer(Sys.time())))
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  
  for (m in month_seq) {
    m <- as.Date(m)
    year_m  <- year(m)
    month_m <- as.integer(format(m, "%m"))
    message("Processing ", format(m, "%B %Y"))
    
    start_day <- m
    end_day   <- ceiling_date(m, "month") - 1
    dates <- seq(start_day, end_day, by = "day")
    dates_str <- format(dates, "%Y%m%d")
    
    files <- character(0)
    
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
      
      myfile <- file.path(tmpdir, paste0("ndvi_", dstr, ".tif"))
      ok <- tryCatch({ suppressWarnings(download.file(myurl, myfile, mode = "wb", quiet = TRUE)); TRUE },
                     error = function(e) FALSE)
      if (ok && file.exists(myfile) && file.info(myfile)$size > 0) files <- c(files, myfile)
    }
    
    if (length(files) == 0) {
      message("No NDVI data available for ", format(m, "%B %Y"))
      next
    }
    
    # read daily rasters; catching small geometry mismatches and resampling to first layer if needed
    r_stack_daily <- tryCatch(rast(files), error = function(e) {
      # attempt progressive reading + resampling
      rs <- list()
      for (f in files) {
        r <- tryCatch(rast(f), error = function(e2) NULL)
        if (!is.null(r)) rs[[length(rs)+1]] <- r
      }
      if (length(rs) == 0) stop("Failed to read any daily rasters")
      # align all to the first raster
      base <- rs[[1]]
      for (j in seq_along(rs)) {
        if (!compareGeom(base, rs[[j]], stopOnError = FALSE)) {
          rs[[j]] <- tryCatch(resample(rs[[j]], base, method = "bilinear"), error = function(e) extend(rs[[j]], base))
        }
      }
      do.call(c, rs)
    })
    
    # compute monthly mean
    r_mean <- app(r_stack_daily, mean, na.rm = TRUE)
    r_mean[is.nan(r_mean)] <- NA
    names(r_mean) <- paste0("ndvi_mean_", year_m, sprintf("%02d", month_m))
    
    raster_list[[length(raster_list) + 1]] <- r_mean
    
    # cleanup daily tiles for this month
    try(unlink(files))
  }
  
  # final cleanup of tempdir could be left to system or done here
  # but keep the directory (it is unique) or remove it:
  # unlink(tmpdir, recursive = TRUE)
  
  if (length(raster_list) == 0) {
    message("No NDVI data available for the selected range")
    return(NULL)
  }
  
  # stack monthly means (should all have same geometry now)
  r_stack <- rast(raster_list)
  return(r_stack)
}
