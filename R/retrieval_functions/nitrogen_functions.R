# Nitrogen / Nature Data Cube retrieval helpers
# --------------------------------------------
# Depends on packages already loaded in app.R:
# rstac, httr, sf, terra, dplyr, purrr, tibble, jsonlite
# and on nitrogen_config.R being sourced first.

nitrogen_make_headers <- function(token) {
  token <- as.character(token)
  token <- trimws(token)
  if (!nzchar(token)) {
    stop("Nature Data Cube token is missing. Set NDC_NATURE_TOKEN (or pass token explicitly).", call. = FALSE)
  }
  httr::add_headers(
    "Authorization" = paste0("Bearer ", token),
    "token" = token,
    "Accept" = "application/json"
  )
}

nitrogen_normalize_aoi <- function(aoi) {
  if (inherits(aoi, "sf")) {
    aoi <- sf::st_geometry(aoi)
  }
  if (!inherits(aoi, c("sfc", "sfg"))) {
    stop("AOI must be an sf or sfc object.", call. = FALSE)
  }
  if (is.na(sf::st_crs(aoi))) {
    sf::st_crs(aoi) <- 4326
  }
  sf::st_transform(aoi, 4326)
}

nitrogen_collect_metadata <- function(aoi, token, endpoint = nitrogen_endpoint,
                                      collection = nitrogen_collection,
                                      layers = nitrogen_layer_choices,
                                      year = NULL,
                                      limit = 100) {
  aoi_4326 <- nitrogen_normalize_aoi(aoi)
  headers <- nitrogen_make_headers(token)

  items <- rstac::stac(endpoint) |>
    rstac::stac_search(
      collections = collection,
      intersects = aoi_4326,
      limit = limit
    ) |>
    rstac::post_request(headers) |>
    rstac::items_fetch(progress = FALSE)

  feats <- items$features
  if (is.null(feats) || length(feats) == 0) {
    stop("No Nature Data Cube raster items were found for the selected AOI.", call. = FALSE)
  }

  meta <- purrr::map_dfr(feats, function(feat) {
    lyr <- feat$properties$`ndc:layer_type`
    obs <- feat$properties$`ndc:observation_date`
    href <- feat$assets[[nitrogen_asset_name]]$href

    tibble::tibble(
      layer = as.character(lyr),
      observation_date = as.character(obs),
      year = substr(as.character(obs), 1, 4),
      href = as.character(href)
    )
  })

  meta <- dplyr::filter(meta, !is.na(layer), !is.na(year), layer %in% layers)

  if (!is.null(year)) {
    year_chr <- as.character(year)
    meta <- dplyr::filter(meta, year %in% year_chr)
  }

  meta <- dplyr::distinct(meta, layer, year, .keep_all = TRUE)
  if (nrow(meta) == 0) {
    stop(
      paste0(
        "No Nature Data Cube items matched the requested layer(s) (",
        paste(layers, collapse = ", "),
        ") and year (",
        paste(year, collapse = ", "),
        ")."
      ),
      call. = FALSE
    )
  }

  meta
}

nitrogen_download_one <- function(href, outfile, headers, overwrite = TRUE) {
  if (file.exists(outfile) && !overwrite) return(outfile)

  res <- httr::GET(
    href,
    headers,
    httr::write_disk(outfile, overwrite = overwrite)
  )

  if (httr::http_error(res)) {
    stop(
      paste0(
        "Failed to download Nature Data Cube raster: HTTP ",
        httr::status_code(res),
        " for ", href
      ),
      call. = FALSE
    )
  }

  outfile
}

nitrogen_clip_raster_to_aoi <- function(r, aoi) {
  if (is.null(r)) return(NULL)

  aoi_vect <- terra::vect(nitrogen_normalize_aoi(aoi))
  r_crs <- terra::crs(r)
  if (is.na(r_crs) || !nzchar(r_crs)) {
    stop("Downloaded raster has no CRS, so it cannot be clipped safely.", call. = FALSE)
  }

  aoi_proj <- terra::project(aoi_vect, r_crs)
  r_clip <- terra::crop(r, aoi_proj)
  r_clip <- terra::mask(r_clip, aoi_proj)
  r_clip
}

get_nitrogen_raster <- function(aoi, year, layers, token = Sys.getenv("NDC_NATURE_TOKEN"),
                                endpoint = nitrogen_endpoint,
                                collection = nitrogen_collection,
                                out_dir = tempdir(),
                                overwrite = TRUE,
                                limit = 100,
                                file_prefix = NULL) {
  year <- nitrogen_normalize_year(year)
  layers <- nitrogen_clean_layer_name(layers)

  if (!nitrogen_controls_are_valid(year, layers)) {
    stop("Please select a year and at least one nitrogen layer.", call. = FALSE)
  }

  meta <- nitrogen_collect_metadata(
    aoi = aoi,
    token = token,
    endpoint = endpoint,
    collection = collection,
    layers = layers,
    year = year,
    limit = limit
  )

  headers <- nitrogen_make_headers(token)
  downloaded <- list()
  clipped <- list()

  prefix <- if (is.null(file_prefix) || !nzchar(as.character(file_prefix))) "" else paste0(gsub("[^A-Za-z0-9_\\-]+", "_", as.character(file_prefix)), "_")

  for (i in seq_len(nrow(meta))) {
    lyr <- meta$layer[i]
    yr <- meta$year[i]
    href <- meta$href[i]

    fname <- paste0(prefix, lyr, "_", yr, ".tif")
    fpath <- file.path(out_dir, fname)

    nitrogen_download_one(href, fpath, headers, overwrite = overwrite)
    r <- terra::rast(fpath)
    r <- nitrogen_clip_raster_to_aoi(r, aoi)

    nm <- paste0(lyr, "_", yr)
    names(r) <- nm
    downloaded[[nm]] <- fpath
    clipped[[nm]] <- r
  }

  stack <- if (length(clipped) == 1) {
    clipped[[1]]
  } else {
    terra::rast(clipped)
  }

  list(
    rasters = clipped,
    stack = stack,
    files = downloaded,
    metadata = meta
  )
}

get_nitrogen_stats <- function(aoi, year, layers, token = Sys.getenv("NDC_NATURE_TOKEN"),
                               endpoint = nitrogen_endpoint,
                               collection = nitrogen_collection,
                               out_dir = tempdir(),
                               overwrite = TRUE,
                               limit = 100,
                               file_prefix = NULL) {
  raster_res <- get_nitrogen_raster(
    aoi = aoi,
    year = year,
    layers = layers,
    token = token,
    endpoint = endpoint,
    collection = collection,
    out_dir = out_dir,
    overwrite = overwrite,
    limit = limit,
    file_prefix = file_prefix
  )

  meta <- raster_res$metadata
  out <- tibble::tibble(ID = 1L)

  for (i in seq_len(nrow(meta))) {
    nm <- paste0(meta$layer[i], "_", meta$year[i], ".tif")
    rnm <- paste0(meta$layer[i], "_", meta$year[i])
    r <- raster_res$rasters[[rnm]]
    if (is.null(r)) next

    val <- terra::global(r, fun = mean, na.rm = TRUE)[1, 1]
    out[[nm]] <- as.numeric(val)
  }

  out
}
