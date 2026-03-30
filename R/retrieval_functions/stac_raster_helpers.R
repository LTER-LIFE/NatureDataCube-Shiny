# Shared helpers for STAC-based raster retrieval
# ---------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

stac_make_headers <- function(token) {
  token <- as.character(token)
  token <- trimws(token)
  if (!nzchar(token)) {
    stop("Nature Data Cube token is missing. Set the token explicitly before retrieval.", call. = FALSE)
  }
  httr::add_headers(
    "Authorization" = paste0("Bearer ", token),
    "token" = token,
    "Accept" = "application/json"
  )
}

stac_normalize_aoi <- function(aoi) {
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

stac_keywords_to_vec <- function(x) {
  if (is.null(x)) return(character(0))
  x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- trimws(x)
  x <- x[nzchar(x)]
  if (length(x) == 1L && grepl(",", x, fixed = TRUE)) {
    x <- trimws(unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE))
    x <- x[nzchar(x)]
  }
  unique(tolower(x))
}

stac_feature_meta <- function(feat, asset_name = "wcs") {
  props <- feat$properties %||% list()
  assets <- feat$assets %||% list()
  href <- NA_character_
  if (!is.null(assets[[asset_name]]) && !is.null(assets[[asset_name]]$href)) {
    href <- as.character(assets[[asset_name]]$href)
  }

  obs <- props$`ndc:observation_date` %||% props$datetime %||% NA_character_
  layer <- props$`ndc:layer_type` %||% props$`geoserver:layer_name` %||% NA_character_
  title <- props$title %||% NA_character_
  keywords <- stac_keywords_to_vec(props$keywords)

  tibble::tibble(
    id = as.character(feat$id %||% NA_character_),
    title = as.character(title),
    layer = as.character(layer),
    observation_date = as.character(obs),
    year = ifelse(!is.na(obs) & nzchar(as.character(obs)), substr(as.character(obs), 1, 4), NA_character_),
    keywords = list(keywords),
    href = href
  )
}

stac_collect_metadata <- function(aoi, token, endpoint, collection, asset_name = "wcs", limit = 100) {
  aoi_4326 <- stac_normalize_aoi(aoi)
  headers <- stac_make_headers(token)

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
    return(tibble::tibble())
  }

  purrr::map_dfr(feats, stac_feature_meta, asset_name = asset_name)
}

stac_bbox_in_crs <- function(aoi, target_crs = 32631L) {
  aoi_4326 <- stac_normalize_aoi(aoi)
  aoi_sf <- sf::st_as_sf(aoi_4326)
  aoi_proj <- sf::st_transform(aoi_sf, target_crs)
  sf::st_bbox(aoi_proj)
}

stac_wcs_subset_suffix <- function(aoi, target_crs = 32631L, x_name = "E", y_name = "N") {
  bbox <- stac_bbox_in_crs(aoi, target_crs = target_crs)
  fmt <- function(x) format(as.numeric(x), scientific = FALSE, trim = TRUE, digits = 12)

  paste0(
    "&subset=", x_name, "(", fmt(bbox[["xmin"]]), ",", fmt(bbox[["xmax"]]), ")",
    "&subset=", y_name, "(", fmt(bbox[["ymin"]]), ",", fmt(bbox[["ymax"]]), ")"
  )
}

stac_download_one <- function(href, outfile, headers, overwrite = TRUE, retries = 3L, min_file_size = NULL) {
  if (file.exists(outfile) && !overwrite) return(outfile)

  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)

  last_status <- NA_integer_
  last_error <- NULL

  for (attempt in seq_len(max(1L, as.integer(retries)))) {
    res <- tryCatch(
      httr::RETRY(
        "GET",
        href,
        headers,
        httr::write_disk(outfile, overwrite = overwrite),
        times = 1,
        pause_base = 0.5,
        pause_cap = 2,
        quiet = TRUE
      ),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (inherits(res, "response")) {
      last_status <- httr::status_code(res)

      if (!httr::http_error(res)) {
        if (!is.null(min_file_size) && file.exists(outfile)) {
          file_size <- suppressWarnings(file.info(outfile)$size)
          if (is.na(file_size) || file_size < as.numeric(min_file_size)) {
            if (file.exists(outfile)) unlink(outfile)
            last_error <- simpleError(
              paste0("Downloaded file is too small (", file_size, " bytes) for ", href)
            )
          } else {
            return(outfile)
          }
        } else {
          return(outfile)
        }
      } else if (file.exists(outfile)) {
        unlink(outfile)
      }
    }

    if (attempt < max(1L, as.integer(retries))) {
      Sys.sleep(min(2 ^ (attempt - 1L), 4))
      next
    }
  }

  if (!is.null(last_error)) {
    stop(last_error)
  }

  if (!is.na(last_status)) {
    stop(
      paste0("Failed to download raster: HTTP ", last_status, " for ", href),
      call. = FALSE
    )
  }

  stop(
    paste0("Failed to download raster for ", href),
    call. = FALSE
  )
}

stac_clip_raster_to_aoi <- function(r, aoi) {
  if (is.null(r)) return(NULL)

  aoi_vect <- terra::vect(stac_normalize_aoi(aoi))
  r_crs <- terra::crs(r)
  if (is.na(r_crs) || !nzchar(r_crs)) {
    stop("Downloaded raster has no CRS, so it cannot be clipped safely.", call. = FALSE)
  }

  aoi_proj <- terra::project(aoi_vect, r_crs)
  r_clip <- terra::crop(r, aoi_proj)
  r_clip <- terra::mask(r_clip, aoi_proj)
  r_clip
}

stac_make_file_prefix <- function(x) {
  if (is.null(x) || !nzchar(as.character(x))) return("")
  paste0(gsub("[^A-Za-z0-9_\\-]+", "_", as.character(x)), "_")
}

stac_build_stack <- function(clipped) {
  if (length(clipped) == 0) return(NULL)
  if (length(clipped) == 1) clipped[[1]] else terra::rast(clipped)
}
