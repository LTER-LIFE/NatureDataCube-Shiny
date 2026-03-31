# Nature Data Cube Land Use raster retrieval helpers
# ---------------------------------------------------
# Requires stac_raster_helpers.R and landuse_config.R to be sourced first.

landuse_normalize_year <- function(year) {
  if (is.null(year) || length(year) == 0) return(NA_integer_)
  year <- suppressWarnings(as.integer(as.character(year)[1]))
  if (is.na(year)) NA_integer_ else year
}

landuse_collect_metadata <- function(aoi, token = Sys.getenv("NDC_NATURE_TOKEN"),
                                     endpoint = landuse_endpoint,
                                     collection = landuse_collection,
                                     limit = 100) {
  meta <- stac_collect_metadata(
    aoi = aoi,
    token = token,
    endpoint = endpoint,
    collection = collection,
    asset_name = landuse_asset_name,
    limit = limit
  )

  if (nrow(meta) == 0) {
    return(meta)
  }

  meta$obs_date <- suppressWarnings(as.Date(meta$observation_date))
  meta$layer_lower <- tolower(meta$layer %||% "")
  meta$title_lower <- tolower(meta$title %||% "")
  meta$keyword_match <- vapply(
    meta$keywords,
    function(k) landuse_keyword %in% k || any(grepl(landuse_keyword, k, fixed = TRUE)),
    logical(1)
  )
  meta$layer_match <- grepl(landuse_keyword, meta$layer_lower, fixed = TRUE)
  meta$title_match <- grepl(landuse_keyword, meta$title_lower, fixed = TRUE)

  meta <- dplyr::filter(meta, keyword_match | layer_match | title_match)
  meta <- dplyr::filter(meta, !is.na(href), nzchar(href))
  meta <- dplyr::distinct(meta, href, .keep_all = TRUE)
  meta <- dplyr::arrange(meta, obs_date, title, id)
  meta
}

get_landuse_raster <- function(aoi,
                               year = landuse_default_year,
                               token = Sys.getenv("NDC_NATURE_TOKEN"),
                               endpoint = landuse_endpoint,
                               collection = landuse_collection,
                               out_dir = tempdir(),
                               overwrite = TRUE,
                               limit = 100,
                               file_prefix = NULL,
                               subset_crs = landuse_subset_crs,
                               min_file_size = landuse_min_file_size) {
  year <- landuse_normalize_year(year)
  if (is.na(year)) {
    stop("Please provide a valid year for Land Use.", call. = FALSE)
  }

  meta <- landuse_collect_metadata(
    aoi = aoi,
    token = token,
    endpoint = endpoint,
    collection = collection,
    limit = limit
  )

  if (nrow(meta) == 0) {
    stop("No Land Use raster items were found for the selected AOI.", call. = FALSE)
  }

  meta <- dplyr::filter(meta, lubridate::year(obs_date) == year)
  if (nrow(meta) == 0) {
    stop(
      paste0("No Land Use raster items matched year ", year, "."),
      call. = FALSE
    )
  }

  headers <- stac_make_headers(token)
  prefix <- stac_make_file_prefix(file_prefix)

  download_dir <- if (is.null(out_dir) || !nzchar(as.character(out_dir))) tempdir() else out_dir
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  subset_suffix <- stac_wcs_subset_suffix(aoi, target_crs = subset_crs, x_name = "E", y_name = "N")

  downloaded <- list()
  clipped <- list()

  for (i in seq_len(nrow(meta))) {
    obs <- meta$obs_date[i]
    base_nm <- if (!is.na(obs)) paste0("LGN_", format(obs, "%Y")) else paste0("LGN_", year)
    if (nrow(meta) > 1) base_nm <- paste0(base_nm, "_", i)

    fname <- paste0(prefix, base_nm, ".tif")
    fpath <- file.path(download_dir, fname)
    url_clip <- paste0(meta$href[i], subset_suffix)

    stac_download_one(
      url_clip,
      fpath,
      headers,
      overwrite = overwrite,
      retries = 3L,
      min_file_size = min_file_size
    )

    if (!file.exists(fpath)) {
      stop(
        paste0("Land Use download did not create a file for ", base_nm, "."),
        call. = FALSE
      )
    }

    file_size <- suppressWarnings(file.info(fpath)$size)
    if (is.na(file_size) || file_size < min_file_size) {
      if (file.exists(fpath)) unlink(fpath)
      stop(
        paste0(
          "Downloaded Land Use file looks invalid or incomplete for ", base_nm,
          " (size: ", file_size, " bytes)."
        ),
        call. = FALSE
      )
    }

    r <- terra::rast(fpath)
    r <- stac_clip_raster_to_aoi(r, aoi)
    names(r) <- base_nm

    downloaded[[base_nm]] <- fpath
    clipped[[base_nm]] <- r
  }

  stack <- stac_build_stack(clipped)

  list(
    rasters = clipped,
    stack = stack,
    files = downloaded,
    metadata = meta
  )
}
