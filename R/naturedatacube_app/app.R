# app.R
# R Shiny NatureDataCube interface - Dummy Version 
# --------------------
# Packages
# --------------------
load_pkgs <- function(pkgs) {
  missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
  invisible(lapply(pkgs, library, character.only = TRUE))
}

pkgs <- c(
  "shiny", "leaflet", "leaflet.extras", "sf", "dplyr", "purrr",
  "stringr", "httr", "geojsonsf", "jsonlite", "zip", "here", "terra",
  "lubridate", "tools", "tibble", "shinyjs"
)
load_pkgs(pkgs)


# --------------------
# Hardcoded dataset list & mapping
# --------------------
available_datasets <- list(
  "Atmosphere" = c("Weather", "Nitrogen"),
  "Biosphere" = c("NDVI", "Vegetation structure"),
  "Hydrosphere"= c("Ground water table"),
  "Geosphere" = c("Soil map", "AHN"),
  "Anthroposphere" = c("Agricultural fields", "Land Use")
)

dataset_api_map <- list(
  "Agricultural fields" = "Fields",
  "AHN" = "AHN",
  "Soil map" = "Soiltypes",
  "Weather" = NA,
  "NDVI" = NA
)

# Flatten dataset names for tab id generation & observers
all_dataset_names <- unique(unlist(available_datasets))
make_tab_id <- function(name) {
  paste0(gsub("[^a-z0-9]+", "_", tolower(name)), "_tab")
}
make_target_id <- function(name, tab) {
  paste0("tab_target_", gsub("[^a-z0-9]+", "_", tolower(name)), "_", tolower(tab))
}
# layer input id helper -> e.g. Weather -> weather_layers
make_layer_input_id <- function(name) {
  paste0(gsub("[^a-z0-9]+", "_", tolower(name)), "_layers")
}

# --------------------
# Per-dataset layer choices (hard-coded for Weather; placeholders for others)
# --------------------
dataset_layer_choices <- list(
  "Weather" = c(
    "max_humidity", "mean_humidity", "meteostationid", "mean_daily_cloud_cover",
    "global_radiation", "min_temperature", "mean_sea_level_pressure",
    "sunshine_duration", "potential_evapotranspiration", "mean_temperature",
    "datum", "max_hourly_mean_windspeed", "hourly_division_max_windspeed",
    "precipitation", "max_temperature", "min_hourly_mean_windspeed",
    "vector_mean_windspeed", "hourly_division_max_gust_windspeed",
    "daily_mean_windspeed", "hourly_division_min_windspeed", "max_gust_windspeed"
  ),
  "NDVI" = c("ndvi_mean", "ndvi_max", "ndvi_pixel_count"),
  # placeholders for other datasets — replace with real lists when available
  "Agricultural fields" = c("field_boundaries", "crop_type", "management"),
  "AHN" = c("elevation", "slope", "aspect"),
  "Soil map" = c("soiltype_code", "soil_description"),
  "Nitrogen" = c("n_deposition", "n_content"),
  "Vegetation structure" = c("canopy_height", "leaf_area_index"),
  "Ground water table" = c("gwt_depth", "gwt_confidence"),
  "Land Use" = c("landuse_class")
)

# Ensure every dataset in all_dataset_names has an entry (empty if not provided)
for (ds in all_dataset_names) {
  if (!(ds %in% names(dataset_layer_choices))) dataset_layer_choices[[ds]] <- character(0)
}

# --------------------
# Source helper for optional retrieval functions
# --------------------
# AgroDataCube
source(here::here("R", "retrieval_functions", "ndc_url.R"))
source(here::here("R", "retrieval_functions", "ndc_get.R"))

# GroenMonitor
source(here::here("R", "retrieval_functions", "gm_url.R"))
source(here::here("R", "retrieval_functions", "gm_get.R"))

# weather helper functions
source(here::here("R", "retrieval_functions", "weather_functions", "get_closest_meteostation.R"))
source(here::here("R", "retrieval_functions", "weather_functions", "get_meteo_for_date.R"))
source(here::here("R", "retrieval_functions", "weather_functions", "get_meteo_for_period.R"))
source(here::here("R", "retrieval_functions", "weather_functions", "get_meteo_for_long_period.R"))
source(here::here("R", "retrieval_functions", "weather_functions", "split_date_range.R"))

# NDVI
source(here::here("R","retrieval_functions", "ndvi", "monthly_ndvi.R"))
source(here::here("R","retrieval_functions", "ndvi", "monthly_ndvi_period.R"))

# --------------------
# Token
# --------------------
mytoken <- "get your token"
myheaders <- c("Accept" = "application/json;charset=utf-8", "token" = mytoken)

# --------------------
# Load fixed polygon layers from a geopackage 
# --------------------
gpkg <- here::here("data", "study_sites.gpkg")
layers <- tryCatch(sf::st_layers(gpkg)$name, error = function(e) NULL)

all_layers <- NULL
if (!is.null(layers)) {
  all_layers <- set_names(
    purrr::map(layers, ~ sf::st_read(gpkg, layer = .x, quiet = TRUE)),
    layers
  )
}

# handle missing/optional layers 
nutnet <- if (!is.null(all_layers) && "nutnet_poly" %in% names(all_layers)) all_layers[["nutnet_poly"]] else NULL
nestboxes <- if (!is.null(all_layers)) {
  nm <- names(all_layers)
  keep <- nm[stringr::str_detect(nm, "^nestkasten_")]
  if (length(keep) > 0) purrr::reduce(all_layers[keep], rbind) else NULL
} else NULL
loobos <- if (!is.null(all_layers) && "loobos" %in% names(all_layers)) all_layers[["loobos"]] else NULL
lights <- if (!is.null(all_layers) && "20251008_licht_op_natuur_lantaarnpalen" %in% names(all_layers)) all_layers[["20251008_licht_op_natuur_lantaarnpalen"]] else NULL

# add WKT column helper
add_wkt_column <- function(sf_obj, col_name = "wkt") {
  sf_obj <- sf::st_transform(sf_obj, 4326)
  sf_obj[[col_name]] <- sf::st_as_text(sf::st_geometry(sf_obj))
  sf_obj
}

nutnet_wkt <- if (!is.null(nutnet)) add_wkt_column(nutnet) else NULL
nestboxes_wkt <- if (!is.null(nestboxes)) add_wkt_column(nestboxes) else NULL
loobos_wkt <- if (!is.null(loobos)) add_wkt_column(loobos) else NULL
lights_wkt <- if (!is.null(lights)) add_wkt_column(lights) else NULL

# --------------------
# Helper: assign sequential, unique user-visible names (source_name)
# Checks current app state (overview, fixed, uploaded, drawn) to avoid collisions.
# --------------------
assign_sequential_source_names <- function(sf_obj, base_name, overview_df, fixed_df, uploaded_df, drawn_df) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(sf_obj)
  # collect existing names from app state
  existing <- character(0)
  if (!is.null(overview_df) && nrow(overview_df) > 0) existing <- c(existing, as.character(overview_df$polygon))
  if (!is.null(fixed_df) && nrow(fixed_df) > 0 && "source_name" %in% names(fixed_df)) existing <- c(existing, as.character(fixed_df$source_name))
  if (!is.null(uploaded_df) && nrow(uploaded_df) > 0 && "source_name" %in% names(uploaded_df)) existing <- c(existing, as.character(uploaded_df$source_name))
  if (!is.null(drawn_df) && nrow(drawn_df) > 0 && "source_name" %in% names(drawn_df)) existing <- c(existing, as.character(drawn_df$source_name))
  existing <- existing[!is.na(existing) & nzchar(existing)]
  
  # helper: parse existing index for this base_name
  esc_base <- gsub("([\\W])", "\\\\\\1", base_name) # escape non-word chars for regex
  parse_index <- function(name) {
    if (identical(name, base_name)) return(1L)
    m <- regmatches(name, regexec(paste0("^", esc_base, "_(\\d+)$"), name))[[1]]
    if (length(m) == 2) return(as.integer(m[2]))
    return(0L)
  }
  
  if (length(existing) == 0) {
    max_idx <- 0L
  } else {
    idxs <- vapply(existing, parse_index, integer(1))
    max_idx <- ifelse(length(idxs) > 0, max(idxs, na.rm = TRUE), 0L)
  }
  
  start_idx <- if (max_idx >= 1L) max_idx + 1L else 1L
  n <- nrow(sf_obj)
  out_names <- character(n)
  for (i in seq_len(n)) {
    if (start_idx == 1L) {
      # assign plain base name for the first new item when no existing ones
      out_names[i] <- base_name
      start_idx <- 2L
    } else {
      out_names[i] <- paste0(base_name, "_", start_idx)
      start_idx <- start_idx + 1L
    }
  }
  
  sf_obj$source_name <- out_names
  sf_obj
}

# --------------------
# Helpers to convert drawn features / geojson features to sf
# --------------------
convert_drawn_to_sf <- function(feat, start_layer_id = 1) {
  if (is.null(feat) || is.null(feat$geometry) || feat$geometry$type != "Polygon") return(NULL)
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(rbind, lapply(coords, function(x) as.numeric(unlist(x))))
  sf_obj <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
  sf_obj$wkt <- sf::st_as_text(sf::st_geometry(sf_obj))
  sf_obj$layer_id <- as.integer(start_layer_id)
  # default source_name will be assigned later via assign_sequential_source_names
  sf_obj
}

convert_geojson_feature_to_sf <- function(feat) {
  if (is.null(feat) || is.null(feat$geometry) || feat$geometry$type != "Polygon") return(NULL)
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(rbind, lapply(coords, function(x) as.numeric(unlist(x))))
  sf_obj <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
  sf_obj$wkt <- sf::st_as_text(sf::st_geometry(sf_obj))
  # try to preserve a layer id if leaflet provided it (safe fallback to NA)
  layer_id_val <- NA_integer_
  if (!is.null(feat$properties) && !is.null(feat$properties$layerId)) {
    layer_id_val <- as.integer(feat$properties$layerId)
  }
  sf_obj$layer_id <- layer_id_val
  # source_name will be assigned later if needed
  sf_obj
}

# --------------------
# Upload helpers (reading different formats)
# --------------------
read_polygons_from_path <- function(path, layer = NULL) {
  sf_obj <- tryCatch({
    if (!is.null(layer)) sf::st_read(path, layer = layer, quiet = TRUE) else sf::st_read(path, quiet = TRUE)
  }, error = function(e) NULL)
  if (is.null(sf_obj)) return(NULL)
  poly_only <- sf_obj[sf::st_is(sf_obj, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE]
  if (nrow(poly_only) == 0) return(NULL)
  if (is.na(sf::st_crs(poly_only))) sf::st_crs(poly_only) <- 4326
  poly_only <- sf::st_transform(poly_only, 4326)
  poly_only
}

process_uploaded_files <- function(files_df, start_layer_id = 1) {
  if (is.null(files_df) || nrow(files_df) == 0) return(list(imported = NULL, pending_gpkg = NULL))
  
  imported_list <- list()
  pending_gpkg <- list()
  
  for (i in seq_len(nrow(files_df))) {
    f <- files_df[i, ]
    fname <- f$name; datapath <- f$datapath
    ext <- tolower(tools::file_ext(fname))
    
    if (ext == "zip") {
      tmp <- tempfile("unzip_"); dir.create(tmp)
      utils::unzip(datapath, exdir = tmp)
      gpkg_files    <- list.files(tmp, pattern = "\\.gpkg$", full.names = TRUE, ignore.case = TRUE)
      shp_files     <- list.files(tmp, pattern = "\\.shp$",  full.names = TRUE, ignore.case = TRUE)
      geojson_files <- list.files(tmp, pattern = "\\.(geojson|json)$", full.names = TRUE, ignore.case = TRUE)
      kml_files     <- list.files(tmp, pattern = "\\.kml$", full.names = TRUE, ignore.case = TRUE)
      
      if (length(gpkg_files) > 0) {
        for (g in gpkg_files) {
          lyr_info <- tryCatch(sf::st_layers(g), error = function(e) NULL)
          if (!is.null(lyr_info) && length(lyr_info$name) > 1) {
            pending_gpkg[[length(pending_gpkg) + 1]] <- list(name = paste0(fname, " -> ", basename(g)), datapath = g, layers = lyr_info$name, original_name = fname)
          } else {
            sf_obj <- read_polygons_from_path(g)
            if (!is.null(sf_obj)) { sf_obj$source_name <- fname; imported_list[[length(imported_list) + 1]] <- sf_obj }
          }
        }
      } else if (length(shp_files) > 0) {
        sf_obj <- tryCatch(sf::st_read(shp_files[1], quiet = TRUE), error = function(e) NULL)
        sf_obj <- if (!is.null(sf_obj)) sf_obj[sf::st_is(sf_obj, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE] else NULL
        if (!is.null(sf_obj) && nrow(sf_obj) > 0) {
          sf_obj <- sf::st_transform(sf_obj, 4326); sf_obj$source_name <- fname; imported_list[[length(imported_list) + 1]] <- sf_obj
        }
      } else if (length(geojson_files) > 0) {
        sf_obj <- read_polygons_from_path(geojson_files[1])
        if (!is.null(sf_obj)) { sf_obj$source_name <- fname; imported_list[[length(imported_list) + 1]] <- sf_obj }
      } else if (length(kml_files) > 0) {
        lyr_info <- tryCatch(sf::st_layers(kml_files[1]), error = function(e) NULL)
        if (!is.null(lyr_info) && length(lyr_info$name) > 0) {
          sf_obj <- tryCatch(sf::st_read(kml_files[1], layer = lyr_info$name[1], quiet = TRUE), error = function(e) NULL)
          sf_obj <- if (!is.null(sf_obj)) sf_obj[sf::st_is(sf_obj, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE] else NULL
          if (!is.null(sf_obj) && nrow(sf_obj) > 0) {
            sf_obj <- sf::st_transform(sf_obj, 4326); sf_obj$source_name <- fname; imported_list[[length(imported_list) + 1]] <- sf_obj
          }
        }
      }
      
    } else if (ext %in% c("gpkg")) {
      lyr_info <- tryCatch(sf::st_layers(datapath), error = function(e) NULL)
      if (!is.null(lyr_info) && length(lyr_info$name) > 1) {
        pending_gpkg[[length(pending_gpkg) + 1]] <- list(name = fname, datapath = datapath, layers = lyr_info$name, original_name = fname)
      } else {
        sf_obj <- read_polygons_from_path(datapath)
        if (!is.null(sf_obj)) { sf_obj$source_name <- fname; imported_list[[length(imported_list) + 1]] <- sf_obj }
      }
      
    } else if (ext %in% c("geojson", "json", "kml", "shp")) {
      sf_obj <- read_polygons_from_path(datapath)
      if (!is.null(sf_obj)) { sf_obj$source_name <- fname; imported_list[[length(imported_list) + 1]] <- sf_obj }
    } else {
      # unsupported - skip
    }
  } # end files loop
  
  imported_sf <- NULL
  if (length(imported_list) > 0) imported_sf <- dplyr::bind_rows(imported_list)
  
  list(imported = imported_sf, pending_gpkg = pending_gpkg)
}

# --------------------
# Safe color for uploaded layers
# --------------------
assign_uploaded_colors <- function(up_sf, drawn_features_val = NULL, fixed_polys_val = NULL) {
  if (is.null(up_sf) || nrow(up_sf) == 0) return(up_sf)
  up_sf$color <- "#2b8cbe" # default uploaded colour
  
  # Determine unique layer_ids first (so all features of layer keep same colour)
  layer_ids <- unique(up_sf$layer_id)
  for (lid in layer_ids) {
    idxs <- which(up_sf$layer_id == lid)
    # default for this layer
    col <- "#2b8cbe"
    # drawn overlap?
    df <- drawn_features_val
    if (!is.null(df) && nrow(df) > 0) {
      ints_drawn <- sf::st_intersects(up_sf[idxs, ], df, sparse = FALSE)
      if ((is.matrix(ints_drawn) && any(ints_drawn)) || (is.logical(ints_drawn) && any(ints_drawn))) col <- "#444444"
    }
    # fixed override
    fp <- fixed_polys_val
    if (!is.null(fp) && nrow(fp) > 0) {
      ints_fixed <- sf::st_intersects(up_sf[idxs, ], fp, sparse = FALSE)
      if ((is.matrix(ints_fixed) && any(ints_fixed)) || (is.logical(ints_fixed) && any(ints_fixed))) col <- "black"
    }
    up_sf$color[idxs] <- col
  }
  up_sf
}

# --------------------
# UI
# --------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body { background-color: #f5f7fb; font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif; }

      /* header */
      .app-header { background: linear-gradient(135deg, #1f5a8a, #2d7da6); color: white; padding: 20px 30px; margin-bottom: 20px; display: flex; align-items: center; gap: 25px; }
      .app-header img { height: 60px; }
      .app-title { font-size: 30px; font-weight: 600; }

      /* panels/buttons */
      .well { background-color: white; border-radius: 6px; border: none; box-shadow: 0 2px 6px rgba(0,0,0,0.08); }
      .btn-custom { background-color: #1f5a8a !important; color: white !important; border: none !important; border-radius: 5px; padding: 8px 14px; font-weight: 500; transition: all 0.2s ease; }
      .btn-custom:hover { background-color: #17476c !important; transform: translateY(-1px); }
      .btn-download { background-color: #1f5a8a !important; color: white !important; border: none !important; }
      .small-btn { padding: 6px 10px; font-size: 13px; }
      .radio label { display: block; background: white; border: 1px solid #d8e1ec; padding: 8px 12px; border-radius: 5px; margin-bottom: 6px; cursor: pointer; transition: all 0.2s ease; }
      .radio label:hover { background: #eef4fb; border-color: #1f5a8a; }
      .radio input[type='radio']:checked + span { font-weight: 600; color: #1f5a8a; }
      .table thead { background-color: #1f5a8a; color: white; }
      .btn-delete { background-color: #c94c4c; color: white; border: none; border-radius: 4px; padding: 2px 6px; }
      .btn-delete:hover { background-color: #a83d3d; }
      h4 { color: #1f5a8a; font-weight: 600; }
      .ndc-category { margin-bottom: 8px; background: white; border: 1px solid #e6eef6; border-radius: 6px; padding: 6px 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.03); }
      .ndc-category summary { font-weight: 600; color: #1f5a8a; list-style: none; cursor: pointer; outline: none; padding: 6px; }
      .ndc-ds { display: block; padding: 6px 10px; margin: 4px 0; border-radius: 5px; color: #233043; text-decoration: none; }
      .ndc-ds:hover { background: #eef4fb; border-color: #1f5a8a; }
      .ndc-ds.selected { background: linear-gradient(90deg, rgba(29,92,140,0.08), rgba(29,92,140,0.04)); border-left: 4px solid #1f5a8a; padding-left: 6px; font-weight: 600; }
      details[open] > summary::after { content: \" \\25BC\"; float: right; }
      details > summary::after { content: \" \\25B6\"; float: right; }

      /* fixed selector item styling */
      .fixed-item { display: block; padding: 6px 10px; margin: 4px 0; border-radius: 5px; color: #233043; text-decoration: none; }
      .fixed-item:hover { background: #eef4fb; border-color: #1f5a8a; }
      .fixed-item.selected { background: linear-gradient(90deg, rgba(29,92,140,0.08), rgba(29,92,140,0.04)); border-left: 4px solid #1f5a8a; padding-left: 6px; font-weight: 600; }

      /* responsive columns for layer checkboxes */
      .layer-checkboxes { column-gap: 18px; -webkit-column-gap: 18px; }

      /* style for disabled tab anchor */
      .nav-tabs a.disabled { color: #999 !important; pointer-events: none; cursor: default; opacity: 0.6; }

      /* visually indicate disabled add_dataset button */
      .btn-custom[disabled] { opacity: 0.55 !important; cursor: not-allowed !important; box-shadow: none !important; }

      /* small circular info button (dataset metadata) */
      .btn-info-circle {
          background-color: #FFFFFF !important;
          color: #2C7BE5 !important;
          border: 1px solid #2C7BE5;
          width: 24px;
          height: 24px;
          padding: 0 !important;
          font-size: 1.1rem;
          font-weight: bold;
          border-radius: 50% !important;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          line-height: 1;
          transition: all 0.2s;
        }
        
        .btn-info-circle:hover,
        .btn-info-circle:focus {
          background-color: #2C7BE5 !important;
          color: #FFFFFF !important;
          border-color: #2C7BE5 !important;
        }
      .btn-info-circle:hover, .btn-info-circle:focus {
        background-color: #1f5a8a !important;
        color: #ffffff !important;
        border-color: #1f5a8a !important;
      }
      .dataset-row { display:flex; align-items:center; justify-content:space-between; gap:8px; }
      .dataset-row .ndc-ds { flex:1; margin:0; }

      /* Floating help button bottom-left */
      .help-button-container {
        position: fixed;
        bottom: 20px;
        left: 20px;
        z-index: 9999;
      }
      .btn-help-circle {
        background-color: #FFFFFF !important;
        color: #2C7BE5 !important;
        border: 1px solid #2C7BE5;
        width: 40px;
        height: 40px;
        font-size: 1.2rem;
        font-weight: bold;
        border-radius: 50% !important;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 0 !important;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        transition: all 0.2s;
      }
      .btn-help-circle:hover {
        background-color: #2C7BE5 !important;
        color: #FFFFFF !important;
      }

      /* ---------------------
         Dataset metadata / tab visuals (made to look like Projects list)
         --------------------- */
      .nav-tabs > li > a {
        color: #233043 !important;
        font-weight: 600;
        background: white;
        border: 1px solid #e6eef6;
        border-radius: 6px;
        margin-right: 8px;
        padding: 8px 12px;
      }
      .nav-tabs > li.active > a, .nav-tabs > li > a:hover {
        background: linear-gradient(90deg, rgba(29,92,140,0.08), rgba(29,92,140,0.04));
        color: #1f5a8a !important;
        border-left: 4px solid #1f5a8a;
        padding-left: 10px;
      }
      .nav-tabs { margin-bottom: 18px; }

      .dataset-controls label, .dataset-controls .control-label {
        color: #1f5a8a;
        font-weight: 600;
      }
      .dataset-controls .form-group { margin-bottom: 16px; }
      .dataset-controls .radio { margin-bottom: 14px; }
      .dataset-controls .shiny-date-input, .dataset-controls .shiny-date-range-input { margin-bottom: 16px; }
      .dataset-controls .small-btn { padding: 6px 10px; font-size: 12px; }
      .checkbox-label-lg { font-size: 14px; font-weight: 600; color: #1f5a8a; }
      .checkbox-label-xl { font-size: 16px; font-weight: 700; color: #1f5a8a; }
      .btn-disabled {
        background-color: #e9ecef !important;
        color: #6c757d !important;
        border: none !important;
        cursor: not-allowed !important;
        box-shadow: none !important;
      }
    ")),
    # JS handlers
    tags$script(HTML("
      Shiny.addCustomMessageHandler('ndc_select_fixed', function(value) {
        document.querySelectorAll('.fixed-item').forEach(function(el){ el.classList.remove('selected'); });
        if (!value) return;
        var el = document.getElementById('fixed-' + String(value).split(' ').join('_'));
        if (el) el.classList.add('selected');
      });
      Shiny.addCustomMessageHandler('ndc_force_clear_fixed_sidebar', function() {
        document.querySelectorAll('.fixed-item').forEach(function(el){ el.classList.remove('selected'); });
      });
      Shiny.addCustomMessageHandler('ndc_select_dataset', function(value) {
        // Important: only clear dataset items, not fixed project items (fixed items also have .ndc-ds class)
        document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(function(el){ el.classList.remove('selected');});
        if (!value) return;
        var el = document.getElementById('ndc-ds-' + String(value).split(' ').join('_'));
        if (el) el.classList.add('selected');
      });

      Shiny.addCustomMessageHandler('ndc_toggle_statistics', function(disable) {
        var anchors = Array.from(document.querySelectorAll('ul.nav-tabs a[data-value]')).filter(function(a){
          return a.getAttribute('data-value') === 'Statistics';
        });
        anchors.forEach(function(a){
          if (disable) {
            a.classList.add('disabled');
            a.setAttribute('aria-disabled', 'true');
            if (a.parentElement.classList.contains('active')) {
              var ul = a.closest('ul.nav-tabs');
              if (ul) {
                var rasterAnchor = Array.from(ul.querySelectorAll('a[data-value]')).find(function(x){ return x.getAttribute('data-value') === 'Raster'; });
                if (rasterAnchor) { rasterAnchor.click(); }
              }
            }
          } else {
            a.classList.remove('disabled');
            a.removeAttribute('aria-disabled');
          }
        });
      });

      // NEW: Enable/disable 'Add to overview' button per selected dataset/layer
      Shiny.addCustomMessageHandler('ndc_toggle_add_button', function(enabled) {
        var btn = document.getElementById('add_dataset');
        if (!btn) return;
        btn.disabled = !enabled;
      });

      // NEW: Toggle the deselect-all button disabled/enabled via server messages
      Shiny.addCustomMessageHandler('ndc_toggle_deselect', function(msg) {
        if (!msg || !msg.id) return;
        var btn = document.getElementById(msg.id);
        if (!btn) return;
        if (msg.enabled) {
          btn.disabled = false;
          btn.classList.remove('btn-disabled');
        } else {
          btn.disabled = true;
          btn.classList.add('btn-disabled');
        }
      });
    "))
  ),
  
  # Header with logo
  tags$div(
    class = "app-header",
    tags$img(src = "LTER-LIFE-logo.png", height = "70px"),
    tags$div(tags$div(class = "app-title", "Nature Data Cube - Dummy Version"))
  ),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      tags$div(
        id = "fixed_selector"
      ),
      
      # UPDATED: main title + subheader styling per your request
      tags$h4("Define area of interest", style = "color: #1f5a8a; font-weight: 600; margin-top: 6px; margin-bottom: 6px;"),
      tags$h5("Select a project, upload a polygon, or draw your own polygon", style = "color: #1f5a8a; font-weight: 600; margin-top: 4px; margin-bottom: 8px;"),
      
      # Projects box
      tags$details(class = "ndc-category",
                   tags$summary("Projects"),
                   tags$a(
                     id = "fixed-NutNet",
                     class = "fixed-item ndc-ds",
                     href = "#",
                     onclick = HTML("
                       var was = this.classList.contains('selected');
                       document.querySelectorAll('.fixed-item').forEach(e=>e.classList.remove('selected'));
                       if (!was) { this.classList.add('selected'); Shiny.setInputValue('fixed_layer', 'NutNet', {priority: 'event'}); } else { Shiny.setInputValue('fixed_layer', '', {priority: 'event'}); }
                       return false;
                     "),
                     "NutNet"
                   ),
                   tags$a(
                     id = "fixed-Nestboxes",
                     class = "fixed-item ndc-ds",
                     href = "#",
                     onclick = HTML("
                       var was = this.classList.contains('selected');
                       document.querySelectorAll('.fixed-item').forEach(e=>e.classList.remove('selected'));
                       if (!was) { this.classList.add('selected'); Shiny.setInputValue('fixed_layer', 'Nestboxes', {priority: 'event'}); } else { Shiny.setInputValue('fixed_layer', '', {priority: 'event'}); }
                       return false;
                     "),
                     "Nestboxes"
                   ),
                   tags$a(
                     id = "fixed-Loobos",
                     class = "fixed-item ndc-ds",
                     href = "#",
                     onclick = HTML("
                       var was = this.classList.contains('selected');
                       document.querySelectorAll('.fixed-item').forEach(e=>e.classList.remove('selected'));
                       if (!was) { this.classList.add('selected'); Shiny.setInputValue('fixed_layer', 'Loobos', {priority: 'event'}); } else { Shiny.setInputValue('fixed_layer', '', {priority: 'event'}); }
                       return false;
                     "),
                     "Loobos"
                   ),
                   tags$a(
                     id = "fixed-Light_on_Nature",
                     class = "fixed-item ndc-ds",
                     href = "#",
                     onclick = HTML("
                       var was = this.classList.contains('selected');
                       document.querySelectorAll('.fixed-item').forEach(e=>e.classList.remove('selected'));
                       if (!was) { this.classList.add('selected'); Shiny.setInputValue('fixed_layer', 'Light on Nature', {priority: 'event'}); } else { Shiny.setInputValue('fixed_layer', '', {priority: 'event'}); }
                       return false;
                     "),
                     "Light on Nature"
                   )
      ),
      
      # Upload box 
      tags$details(class = "ndc-category",
                   tags$summary("Upload your own polygon(s)"),
                   tags$div(style = "margin-top:8px;",
                            helpText("Supported file formats: .gpkg, .geojson, .kml, .shp"),
                            fileInput(
                              "upload",
                              label = "Upload polygon file (single or multiple):",
                              multiple = TRUE,
                              accept = c('.zip', '.gpkg', '.geojson', '.json', '.kml', '.shp')
                            ),
                            uiOutput("upload_panel"),
                            tags$div(style = "margin-top:6px;")
                   )
      ),
      
      # Draw box
      tags$details(class = "ndc-category",
                   tags$summary("Draw your own polygon"),
                   tags$div(style = "margin-top:8px;",
                            helpText("Use the draw toolbar on the map below to create polygon(s). Click a polygon to select or deselect it. Use the edit/remove tools on the map toolbar to delete drawn shapes."),
                            tags$div(style = "margin-top:6px;")
                   )
      ),
      
      # Shared map
      leafletOutput("map", height = "400px"),
      br(),
      
      h4("Choose dataset(s)"),
      tags$div(
        # Atmosphere
        tags$details(class = "ndc-category", 
                     tags$summary("Atmosphere"),
                     tags$div(class = "dataset-row",
                              tags$a(
                                id = "ndc-ds-Weather",
                                class = "ndc-ds",
                                href = "#",
                                onclick = HTML(
                                  "document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected'));
                                   this.classList.add('selected');
                                   Shiny.setInputValue('selected_dataset', 'Weather', {priority: 'event'});
                                   return false;"
                                ),
                                "Weather"
                              ),
                              actionButton("info_ds_Weather", "i", class = "btn-info-circle")
                     ),
                     tags$div(class = "dataset-row",
                              tags$a(
                                id = "ndc-ds-Nitrogen",
                                class = "ndc-ds",
                                href = "#",
                                onclick = HTML(
                                  "document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected'));
                                   this.classList.add('selected');
                                   Shiny.setInputValue('selected_dataset', 'Nitrogen', {priority: 'event'});
                                   return false;"
                                ),
                                "Nitrogen"
                              ),
                              actionButton("info_ds_Nitrogen", "i", class = "btn-info-circle")
                     )
        ),
        
        # Biosphere
        tags$details(class = "ndc-category", 
                     tags$summary("Biosphere"),
                     tags$div(class = "dataset-row",
                              tags$a(
                                id = "ndc-ds-NDVI",
                                class = "ndc-ds",
                                href = "#",
                                onclick = HTML(
                                  "document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected'));
                                   this.classList.add('selected');
                                   Shiny.setInputValue('selected_dataset', 'NDVI', {priority: 'event'});
                                   return false;"
                                ),
                                "NDVI (greenness)"
                              ),
                              actionButton("info_ds_NDVI", "i", class = "btn-info-circle")
                     ),
                     tags$div(class = "dataset-row",
                              tags$a(
                                id = "ndc-ds-Vegetation_structure",
                                class = "ndc-ds",
                                href = "#",
                                onclick = HTML(
                                  "document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected'));
                                   this.classList.add('selected');
                                   Shiny.setInputValue('selected_dataset', 'Vegetation structure', {priority: 'event'});
                                   return false;"
                                ),
                                "Vegetation structure"
                              ),
                              actionButton("info_ds_Vegetation_structure", "i", class = "btn-info-circle")
                     )
        ),
        
        # Hydrosphere
        tags$details(class = "ndc-category", 
                     tags$summary("Hydrosphere"),
                     tags$div(class = "dataset-row",
                              tags$a(
                                id = "ndc-ds-Ground_water_table",
                                class = "ndc-ds",
                                href = "#",
                                onclick = HTML(
                                  "document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected'));
                                   this.classList.add('selected');
                                   Shiny.setInputValue('selected_dataset', 'Ground water table', {priority: 'event'});
                                   return false;"
                                ),
                                "Ground water table"
                              ),
                              actionButton("info_ds_Ground_water_table", "i", class = "btn-info-circle")
                     )
        ),
        
        # Geosphere
        tags$details(class = "ndc-category",
                     tags$summary("Geosphere"),
                     tags$div(class = "dataset-row",
                              tags$a(
                                id = "ndc-ds-Soil_map",
                                class = "ndc-ds",
                                href = "#",
                                onclick = HTML(
                                  "document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected'));
                                   this.classList.add('selected');
                                   Shiny.setInputValue('selected_dataset', 'Soil map', {priority: 'event'});
                                   return false;"
                                ),
                                "Soil Map"
                              ),
                              actionButton("info_ds_Soil_map", "i", class = "btn-info-circle")
                     ),
                     tags$div(class = "dataset-row",
                              tags$a(
                                id = "ndc-ds-AHN",
                                class = "ndc-ds",
                                href = "#",
                                onclick = HTML(
                                  "document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected'));
                                   this.classList.add('selected');
                                   Shiny.setInputValue('selected_dataset', 'AHN', {priority: 'event'});
                                   return false;"
                                ),
                                "Elevation (AHN)"
                              ),
                              actionButton("info_ds_AHN", "i", class = "btn-info-circle")
                     )
        ),
        
        # Anthroposphere
        tags$details(class = "ndc-category", 
                     tags$summary("Anthroposphere"),
                     tags$div(class = "dataset-row",
                              tags$a(
                                id = "ndc-ds-Agricultural_fields",
                                class = "ndc-ds",
                                href = "#",
                                onclick = HTML(
                                  "document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected'));
                                   this.classList.add('selected');
                                   Shiny.setInputValue('selected_dataset', 'Agricultural fields', {priority: 'event'});
                                   return false;"
                                ),
                                "Agricultural fields"
                              ),
                              actionButton("info_ds_Agricultural_fields", "i", class = "btn-info-circle")
                     ),
                     tags$div(class = "dataset-row",
                              tags$a(
                                id = "ndc-ds-Land_Use",
                                class = "ndc-ds",
                                href = "#",
                                onclick = HTML(
                                  "document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected'));
                                   this.classList.add('selected');
                                   Shiny.setInputValue('selected_dataset', 'Land Use', {priority: 'event'});
                                   return false;"
                                ),
                                "Land Use"
                              ),
                              actionButton("info_ds_Land_Use", "i", class = "btn-info-circle")
                     )
        )
      )
    ),
    
    mainPanel(
      h4("Overview of selected datasets"),
      uiOutput("overview_table"),
      actionButton("clear_overview", "Clear overview", class = "btn-custom"),
      br(),
      tags$div(style = "margin-top:12px;"),
      uiOutput("download_ui"),
      br(),
      uiOutput("download_messages"),
      hr(),
      uiOutput("dataset_metadata"),
      tags$div(style = "margin-top:18px;",
               actionButton("add_dataset", "Add to overview", class = "btn-custom")
      )
    )
  ),
  
  # Floating help button (fixed bottom-left)
  div(
    class = "help-button-container",
    actionButton("open_guide", "?", class = "btn-help-circle")
  )
)

# --------------------
# Server
# --------------------
server <- function(input, output, session) {
  # disable Statistics tabs by default (target tabs with value "Statistics")
  shinyjs::disable(selector = "a[data-value='Statistics']")
  
  # persistent storage
  drawn_features <- reactiveVal(NULL)    # sf collection of drawn polygons (uploaded/drawn/fixed are separate)
  selected_polygons <- reactiveVal(NULL) # sf collection (zero or many)
  fixed_polys <- reactiveVal(NULL)
  uploaded_polys <- reactiveVal(NULL)
  pending_gpkg <- reactiveVal(list())
  
  # track which dynamic observers we've created (so we don't create duplicates)
  created_observers <- reactiveVal(character(0))
  
  # Track the currently active Statistics tabs for each dataset
  stats_tabs_present <- reactiveVal(character(0))
  
  # NOTE: added `view` column to store "Raster" / "Statistics"
  # NOTE: added `layers` list-column to store the selected finer-level layers
  overview <- reactiveVal(
    tibble::tibble(
      dataset = character(),
      view = character(),       # "Raster" or "Statistics"
      year = integer(),
      polygon = character(),
      wkt = character(),
      polygon_sf = list(),
      date_from = as.Date(character()),
      date_to = as.Date(character()),
      layers = list(),          # list-column: character vectors for layer names
      layers_sig = character()  # collapsed signature (sorted, joined) used for duplicate detection
    )
  )
  
  download_msgs <- reactiveVal(character(0))
  
  # ------------ Dataset metadata info (for the (i) buttons) ------------
  dataset_info <- list(
    "Weather" = list(
      title = "Weather",
      description = "KNMI weather data for the selected area (daily aggregates, station interpolations).",
      variables = dataset_layer_choices[["Weather"]],
      notes = "Choose a date or a period. Supports weather variables such as temperature, precipitation and wind-related fields."
    ),
    "Nitrogen" = list(
      title = "Nitrogen",
      description = "Nitrogen deposition / content data relevant for environmental assessments.",
      variables = dataset_layer_choices[["Nitrogen"]],
      notes = "May be provided as raster or tabular statistics depending on the source."
    ),
    "NDVI" = list(
      title = "NDVI",
      description = "Monthly average NDVI (Normalized Difference Vegetation Index) for the selected area.",
      variables = dataset_layer_choices[["NDVI"]],
      notes = "Supports single-month or range queries (month granularity)."
    ),
    "Vegetation structure" = list(
      title = "Vegetation structure",
      description = "Structural vegetation measurements for the selected area (e.g. canopy height, leaf area).",
      variables = dataset_layer_choices[["Vegetation structure"]],
      notes = "Data are often derived from lidar or field surveys and can be delivered as raster summaries."
    ),
    "Ground water table" = list(
      title = "Ground water table",
      description = "Groundwater depth information for the selected area.",
      variables = dataset_layer_choices[["Ground water table"]],
      notes = "May include measured and modelled depth-to-water and confidence layers."
    ),
    "Soil map" = list(
      title = "Soil map",
      description = "Soil classification and description layers for the selected area (Soiltypes).",
      variables = dataset_layer_choices[["Soil map"]],
      notes = "Useful for understanding substrate and land suitability."
    ),
    "AHN" = list(
      title = "AHN",
      description = "Elevation data of the Netherlands (AHN) — high-resolution DEM products.",
      variables = dataset_layer_choices[["AHN"]],
      notes = "Typically returned as rasters or elevation points; good for slope/aspect derivations."
    ),
    "Agricultural fields" = list(
      title = "Agricultural fields",
      description = "Data about agricultural fields e.g. crop type. (AgroDataCube Fields endpoint)",
      variables = dataset_layer_choices[["Agricultural fields"]],
      notes = "Requires selection of a year; often linked to AgroDataCube Fields endpoint."
    ),
    "Land Use" = list(
      title = "Land Use",
      description = "Land use classification layers for the selected area.",
      variables = dataset_layer_choices[["Land Use"]],
      notes = "May include national or European land use schemes depending on data source."
    )
  )
  
  # create observers for info buttons (one per dataset) to show modal with metadata
  lapply(names(dataset_info), function(ds_name) {
    btn_id <- paste0("info_ds_", gsub("[^A-Za-z0-9]", "_", ds_name))
    observeEvent(input[[btn_id]], {
      info <- dataset_info[[ds_name]]
      vars_txt <- if (!is.null(info$variables) && length(info$variables) > 0) paste0("<b>Example variables:</b> ", paste(info$variables, collapse = ", ")) else ""
      showModal(modalDialog(
        title = info$title,
        HTML(paste0("<p>", info$description, "</p>",
                    if (nzchar(vars_txt)) paste0("<p>", vars_txt, "</p>") else "",
                    "<p><b>Notes:</b> ", info$notes, "</p>")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)
  })
  
  # Helper: assign colours (per-layer)
  assign_uploaded_colors_server <- function(up_sf) {
    assign_uploaded_colors(up_sf, drawn_features_val = drawn_features(), fixed_polys_val = fixed_polys())
  }
  
  # --------------------
  # Helper to re-queue pending GeoPackage layers when uploaded layers are removed by switching modes
  # --------------------
  restore_pending_from_uploaded <- function(up_sf) {
    if (is.null(up_sf) || nrow(up_sf) == 0) return(invisible(NULL))
    if (!all(c("pending_pid", "pending_layer") %in% names(up_sf))) return(invisible(NULL))
    
    cur_pending <- pending_gpkg()
    if (is.null(cur_pending)) cur_pending <- list()
    
    restore_df <- up_sf[
      !is.na(up_sf$pending_pid) & nzchar(as.character(up_sf$pending_pid)) &
        !is.na(up_sf$pending_layer) & nzchar(as.character(up_sf$pending_layer)),
      , drop = FALSE
    ]
    if (nrow(restore_df) == 0) return(invisible(NULL))
    
    restore_key <- paste(as.character(restore_df$pending_pid), as.character(restore_df$pending_layer), sep = "::")
    restore_df <- restore_df[!duplicated(restore_key), , drop = FALSE]
    
    for (i in seq_len(nrow(restore_df))) {
      pid <- as.character(restore_df$pending_pid[i])
      layer_nm <- as.character(restore_df$pending_layer[i])
      if (!nzchar(pid) || !nzchar(layer_nm)) next
      if (is.null(cur_pending[[pid]])) next
      
      prev_imported <- cur_pending[[pid]]$imported
      if (is.null(prev_imported)) prev_imported <- character(0)
      cur_pending[[pid]]$imported <- setdiff(unique(prev_imported), layer_nm)
      
      all_layers_list <- cur_pending[[pid]]$all_layers
      if (is.null(all_layers_list)) all_layers_list <- cur_pending[[pid]]$layers
      cur_pending[[pid]]$layers <- setdiff(all_layers_list, cur_pending[[pid]]$imported)
    }
    
    pending_gpkg(cur_pending)
    invisible(NULL)
  }
  
  # --------------------
  # helper to clear polygon groups and reactive stores when switching input type.
  # `except` is a vector of: "fixed", "uploaded", "drawn" (groups to keep).
  # --------------------
  clear_map_polygons <- function(except = character(0)) {
    groups_all <- c("fixed", "uploaded", "drawn", "highlight_fixed", "highlight_uploaded", "highlight_drawn")
    to_clear <- setdiff(groups_all, except)
    proxy <- leaflet::leafletProxy("map")
    for (g in to_clear) {
      proxy <- suppressWarnings(leaflet::clearGroup(proxy, g))
    }
    proxy <- suppressWarnings(leaflet::clearPopups(proxy))
    
    # If uploaded layers are being removed from the map, push them back into the pending list if needed.
    if (!("uploaded" %in% except)) {
      restore_pending_from_uploaded(uploaded_polys())
    }
    
    # reset reactives for the types that were cleared
    if (!("fixed" %in% except)) fixed_polys(NULL)
    if (!("uploaded" %in% except)) uploaded_polys(NULL)
    if (!("drawn" %in% except)) drawn_features(NULL)
    
    # always clear selected polygons when switching types so highlighting doesn't persist
    selected_polygons(NULL)
    invisible(NULL)
  }
  
  # Explicitly clear project-layer selection before switching to draw/upload mode.
  clear_project_selection_state <- function() {
    session$sendCustomMessage("ndc_clear_fixed_input", NULL)
    session$sendCustomMessage("ndc_select_fixed", NULL)
    sel <- selected_polygons()
    if (is.null(sel) || nrow(sel) == 0) {
      selected_polygons(NULL)
    } else if ("source" %in% names(sel)) {
      sel <- sel[sel$source != "fixed", , drop = FALSE]
      if (nrow(sel) == 0) sel <- NULL
      selected_polygons(sel)
    } else {
      selected_polygons(NULL)
    }
    update_selected_highlights()
    invisible(NULL)
  }
  
  # ensure initial disabled state on session start
  session$sendCustomMessage("ndc_toggle_add_button", FALSE)
  
  # ---- Enable/disable "Add to overview" button based on app state ----
  observe({
    ds <- NULL
    try({ ds <- input$selected_dataset }, silent = TRUE)
    
    sel <- selected_polygons()
    enabled <- FALSE
    
    if (!is.null(ds) && nzchar(ds) && !is.null(sel) && nrow(sel) > 0) {
      layer_input_id <- make_layer_input_id(ds)
      layers_sel <- NULL
      try({ layers_sel <- input[[layer_input_id]] }, silent = TRUE)
      
      choices_for_ds <- if (!is.null(dataset_layer_choices[[ds]])) dataset_layer_choices[[ds]] else character(0)
      needs_layers <- length(choices_for_ds) > 0
      
      date_ok <- TRUE
      if (ds == "Weather") {
        try({
          mode <- input$weather_mode
          if (is.null(mode) || mode == "single") {
            date_ok <- !is.null(input$weather_date) && !is.na(as.Date(input$weather_date))
          } else {
            pr <- input$weather_period
            date_ok <- !is.null(pr) && length(pr) == 2 && !is.na(as.Date(pr[1])) && !is.na(as.Date(pr[2]))
          }
        }, silent = TRUE)
      } else if (ds == "NDVI") {
        try({
          mode <- input$ndvi_mode
          if (is.null(mode) || mode == "single") {
            date_ok <- !is.null(input$ndvi_year) && !is.null(input$ndvi_month)
          } else {
            date_ok <- !is.null(input$ndvi_from_year) && !is.null(input$ndvi_from_month) &&
              !is.null(input$ndvi_to_year) && !is.null(input$ndvi_to_month)
          }
        }, silent = TRUE)
      } else if (ds == "Agricultural fields") {
        try({ date_ok <- !is.null(input$selected_year) }, silent = TRUE)
      }
      
      if (needs_layers) {
        enabled <- !is.null(layers_sel) && length(layers_sel) > 0 && isTRUE(date_ok)
      } else {
        enabled <- isTRUE(date_ok)
      }
    }
    
    session$sendCustomMessage("ndc_toggle_add_button", enabled)
  })
  
  # --------------------
  # Observe selected polygons and insert/remove Statistics tab per-dataset when a project polygon is selected
  # --------------------
  observe({
    ds <- input$selected_dataset
    if (is.null(ds) || ds == "") return(NULL)
    tid <- make_tab_id(ds)
    if (is.null(input[[tid]])) return(NULL)
    
    sel <- selected_polygons()
    project_selected <- FALSE
    if (!is.null(sel) && nrow(sel) > 0 && "source" %in% names(sel)) {
      project_selected <- any(sel$source == "fixed")
    }
    
    present <- stats_tabs_present()
    has_it <- tid %in% present
    
    if (project_selected && !has_it) {
      insertTab(
        inputId = tid,
        tab = tabPanel(title = "Statistics", value = "Statistics", uiOutput(make_target_id(ds, "Statistics"))),
        target = "Raster",
        position = "after",
        select = FALSE
      )
      stats_tabs_present(unique(c(present, tid)))
    } else if (!project_selected && has_it) {
      tryCatch({
        removeTab(inputId = tid, target = "Statistics")
      }, error = function(e) NULL)
      stats_tabs_present(setdiff(present, tid))
    }
  })
  
  # Handle toggling the tab when selecting a project polygon
  observe({
    sel <- selected_polygons()
    project_selected <- FALSE
    if (!is.null(sel) && nrow(sel) > 0 && "source" %in% names(sel)) {
      project_selected <- any(sel$source == "fixed")
    }
    session$sendCustomMessage("ndc_toggle_statistics", !project_selected)
  })
  
  # helper to update highlight layers to reflect selected_polygons
  update_selected_highlights <- function() {
    sel <- selected_polygons()
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("highlight_fixed") %>%
      leaflet::clearGroup("highlight_uploaded") %>%
      leaflet::clearGroup("highlight_drawn")
    if (is.null(sel) || nrow(sel) == 0) return(invisible(NULL))
    if (!("source" %in% names(sel))) sel$source <- "drawn"
    
    if (any(sel$source == "fixed")) {
      toadd <- sel[sel$source == "fixed", , drop = FALSE]
      leaflet::leafletProxy("map") %>% leaflet::addPolygons(
        data = toadd,
        group = "highlight_fixed",
        color = "#007bff",
        weight = 4,
        fillOpacity = 0.5,
        layerId = ~layer_id,
        label = ~source_name,
        labelOptions = leaflet::labelOptions(direction = "auto")
      )
    }
    if (any(sel$source == "uploaded")) {
      toadd <- sel[sel$source == "uploaded", , drop = FALSE]
      if ("color" %in% names(toadd)) {
        leaflet::leafletProxy("map") %>% leaflet::addPolygons(
          data = toadd,
          group = "highlight_uploaded",
          color = ~color,
          fillColor = ~color,
          weight = 4,
          fillOpacity = 0.5,
          layerId = ~layer_id,
          label = ~source_name,
          labelOptions = leaflet::labelOptions(direction = "auto")
        )
      } else {
        leaflet::leafletProxy("map") %>% leaflet::addPolygons(
          data = toadd,
          group = "highlight_uploaded",
          color = "#007bff",
          fillColor = "#007bff",
          weight = 4,
          fillOpacity = 0.5,
          layerId = ~layer_id,
          label = ~source_name,
          labelOptions = leaflet::labelOptions(direction = "auto")
        )
      }
    }
    if (any(sel$source == "drawn")) {
      toadd <- sel[sel$source == "drawn", , drop = FALSE]
      leaflet::leafletProxy("map") %>% leaflet::addPolygons(
        data = toadd,
        group = "highlight_drawn",
        color = "#007bff",
        weight = 4,
        fillOpacity = 0.5,
        layerId = ~layer_id,
        label = ~source_name,
        labelOptions = leaflet::labelOptions(direction = "auto")
      )
    }
    invisible(NULL)
  }
  
  # sync dataset highlight
  observeEvent(input$selected_dataset, {
    val <- if (is.null(input$selected_dataset) || identical(input$selected_dataset, "")) NULL else input$selected_dataset
    session$sendCustomMessage("ndc_select_dataset", val)
  }, ignoreNULL = FALSE)
  
  # sync fixed highlight
  observeEvent(input$fixed_layer, {
    val <- if (is.null(input$fixed_layer) || identical(input$fixed_layer, "")) NULL else input$fixed_layer
    session$sendCustomMessage("ndc_select_fixed", val)
  }, ignoreNULL = FALSE)
  
  # render base map and draw toolbar
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = 5.3, lat = 52.1, zoom = 7) %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polylineOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = drawPolygonOptions(showArea = TRUE, repeatMode = FALSE),
        editOptions = editToolbarOptions(edit = FALSE, remove = TRUE)
      )
  })
  
  # fixed polygons: show / clear, toggling behaviour
  observeEvent(input$fixed_layer, {
    if (is.null(input$fixed_layer) || input$fixed_layer == "") {
      clear_map_polygons(except = character(0))
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("fixed") %>%
        leaflet::clearGroup("highlight_fixed") %>%
        leaflet::clearPopups()
      session$sendCustomMessage("ndc_select_fixed", NULL)
      return(NULL)
    }
    
    clear_map_polygons(except = c("fixed"))
    
    poly <- dplyr::mutate(switch(
      input$fixed_layer,
      "NutNet" = nutnet_wkt,
      "Nestboxes" = nestboxes_wkt,
      "Loobos" = loobos_wkt,
      "Light on Nature" = lights_wkt,
      NULL
    ), layer_id = as.integer(dplyr::row_number()))
    if (is.null(poly)) return(NULL)
    
    poly <- assign_sequential_source_names(poly, base_name = input$fixed_layer, overview(), fixed_polys(), uploaded_polys(), drawn_features())
    fixed_polys(poly)
    
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("fixed") %>%
      leaflet::clearGroup("highlight_fixed") %>%
      leaflet::clearPopups() %>%
      leaflet::addPolygons(
        data = poly,
        group = "fixed",
        color = "black",
        fillOpacity = 0.3,
        weight = 2,
        layerId = ~layer_id,
        label = ~source_name,
        labelOptions = leaflet::labelOptions(direction = "auto")
      )
    
    if (is.null(selected_polygons()) || nrow(selected_polygons()) == 0) {
      centroid <- sf::st_centroid(sf::st_geometry(poly[nrow(poly), ]))
      coords <- sf::st_coordinates(centroid)
      leaflet::leafletProxy("map") %>%
        leaflet::addPopups(lng = coords[1], lat = coords[2], popup = "⚠ You have selected a project but still need to select or draw a project area.", options = leaflet::popupOptions(closeButton = TRUE))
    }
    
  }, ignoreNULL = FALSE)
  
  # click handler: id-based match first, then spatial fallback for all sources
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(!is.null(click$lng), !is.null(click$lat))
    
    found_poly <- NULL
    found_source <- NULL
    
    if (!is.null(click$id) && click$id != "") {
      fp <- fixed_polys()
      if (!is.null(fp) && "layer_id" %in% names(fp)) {
        match_idx <- which(as.integer(fp$layer_id) == as.integer(click$id))
        if (length(match_idx) > 0) { found_poly <- fp[match_idx, , drop = FALSE]; found_source <- "fixed" }
      }
      if (is.null(found_poly)) {
        up <- uploaded_polys()
        if (!is.null(up) && "layer_id" %in% names(up)) {
          match_idx <- which(as.integer(up$layer_id) == as.integer(click$id))
          if (length(match_idx) > 0) { found_poly <- up[match_idx, , drop = FALSE]; found_source <- "uploaded" }
        }
      }
    }
    
    if (is.null(found_poly)) {
      pt_sf <- sf::st_as_sf(data.frame(id = 1, x = click$lng, y = click$lat), coords = c("x", "y"), crs = 4326)
      
      fp <- fixed_polys()
      if (!is.null(fp) && nrow(fp) > 0) {
        ints <- sf::st_intersects(fp, pt_sf, sparse = FALSE)
        if (any(ints)) { idx <- which(ints)[1]; found_poly <- fp[idx, , drop = FALSE]; found_source <- "fixed" }
      }
      
      if (is.null(found_poly)) {
        up <- uploaded_polys()
        if (!is.null(up) && nrow(up) > 0) {
          ints <- sf::st_intersects(up, pt_sf, sparse = FALSE)
          if (any(ints)) { idx <- which(ints)[1]; found_poly <- up[idx, , drop = FALSE]; found_source <- "uploaded" }
        }
      }
      
      if (is.null(found_poly)) {
        df <- drawn_features()
        if (!is.null(df) && nrow(df) > 0) {
          ints <- sf::st_intersects(df, pt_sf, sparse = FALSE)
          if (any(ints)) { idx <- which(ints)[1]; found_poly <- df[idx, , drop = FALSE]; found_source <- "drawn" }
        }
      }
    }
    
    if (is.null(found_poly) || nrow(found_poly) == 0) return(NULL)
    
    cur_sel <- selected_polygons()
    if (!is.null(cur_sel) && nrow(cur_sel) > 0) {
      eq_matrix <- sf::st_equals(cur_sel, found_poly, sparse = FALSE)
      if (is.matrix(eq_matrix) && any(as.logical(eq_matrix))) {
        rem_idx <- which(as.logical(eq_matrix), arr.ind = FALSE)
        if (length(rem_idx) > 0) {
          new_sel <- cur_sel[-rem_idx, , drop = FALSE]
          if (nrow(new_sel) == 0) new_sel <- NULL
          selected_polygons(new_sel)
          update_selected_highlights()
          return(NULL)
        }
      }
    }
    
    found_poly$source <- found_source
    if (is.null(cur_sel) || nrow(cur_sel) == 0) {
      selected_polygons(found_poly)
    } else {
      if (!("source" %in% names(cur_sel))) cur_sel$source <- "drawn"
      selected_polygons(dplyr::bind_rows(cur_sel, found_poly))
    }
    
    update_selected_highlights()
    
  })
  
  # when user draws a new polygon: persist in drawn_features and select it
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    cur_fixed_max <- if (!is.null(fixed_polys())) max(as.integer(fixed_polys()$layer_id), na.rm = TRUE) else 0
    cur_uploaded_max<- if (!is.null(uploaded_polys())) max(as.integer(uploaded_polys()$layer_id), na.rm = TRUE) else 0
    cur_drawn_max <- if (!is.null(drawn_features())) max(as.integer(drawn_features()$layer_id), na.rm = TRUE) else 0
    start_id <- max(cur_fixed_max, cur_uploaded_max, cur_drawn_max, 0) + 1
    
    # when drawing starts, clear project selection state and remove fixed polygons
    clear_project_selection_state()
    session$sendCustomMessage("ndc_force_clear_fixed_sidebar", NULL)
    clear_map_polygons(except = c("drawn"))
    session$sendCustomMessage("ndc_select_fixed", NULL)
    
    poly_sf <- convert_drawn_to_sf(feat, start_layer_id = start_id)
    req(poly_sf)
    
    poly_sf <- assign_sequential_source_names(poly_sf, base_name = "Own polygon", overview(), fixed_polys(), uploaded_polys(), drawn_features())
    
    if (is.null(drawn_features())) drawn_features(poly_sf) else drawn_features(dplyr::bind_rows(drawn_features(), poly_sf))
    
    leaflet::leafletProxy("map") %>% leaflet::clearGroup("drawn")
    leaflet::leafletProxy("map") %>%
      leaflet::addPolygons(
        data = drawn_features(),
        group = "drawn",
        layerId = ~layer_id,
        color = "#444444",
        weight = 2,
        fillOpacity = 0.3,
        label = ~source_name,
        labelOptions = leaflet::labelOptions(direction = "auto")
      )
    
    poly_sf$source <- "drawn"
    cur_sel <- selected_polygons()
    if (is.null(cur_sel) || nrow(cur_sel) == 0) {
      selected_polygons(poly_sf)
    } else {
      if (!("source" %in% names(cur_sel))) cur_sel$source <- "drawn"
      selected_polygons(dplyr::bind_rows(cur_sel, poly_sf))
    }
    
    update_selected_highlights()
  })
  
  # when user deletes drawn features via the draw toolbar
  observeEvent(input$map_draw_deleted_features, {
    deleted <- input$map_draw_deleted_features
    df <- drawn_features()
    if (is.null(df) || nrow(df) == 0) {
      selected_polygons(NULL)
      leaflet::leafletProxy("map") %>% leaflet::clearGroup("highlight_drawn")
      return(NULL)
    }
    
    deleted_feats <- list()
    if (!is.null(deleted$features) && length(deleted$features) > 0) {
      for (f in deleted$features) {
        sf_f <- convert_geojson_feature_to_sf(f)
        if (!is.null(sf_f)) deleted_feats[[length(deleted_feats) + 1]] <- sf_f
      }
    }
    
    if (length(deleted_feats) == 0) {
      drawn_features(NULL)
      sel <- selected_polygons()
      if (!is.null(sel) && "source" %in% names(sel)) sel <- sel[sel$source != "drawn", , drop = FALSE]
      if (is.null(sel) || nrow(sel) == 0) sel <- NULL
      selected_polygons(sel)
      update_selected_highlights()
      return(NULL)
    }
    
    remaining <- df
    for (del in deleted_feats) {
      eq_idx <- integer(0)
      if (nrow(remaining) > 0) {
        eq_matrix <- sf::st_equals(remaining, del, sparse = FALSE)
        if (is.matrix(eq_matrix)) eq_idx <- which(as.logical(eq_matrix))
        if (length(eq_idx) == 0) {
          inters <- sf::st_intersects(remaining, del, sparse = FALSE)
          if (is.matrix(inters)) eq_idx <- which(as.logical(inters))
        }
      }
      if (length(eq_idx) > 0) {
        remaining <- remaining[-eq_idx, , drop = FALSE]
      }
    }
    
    if (nrow(remaining) == 0) drawn_features(NULL) else drawn_features(remaining)
    
    sel <- selected_polygons()
    if (!is.null(sel) && nrow(sel) > 0) {
      if ("source" %in% names(sel)) {
        keep_idx <- rep(TRUE, nrow(sel))
        for (i in seq_len(nrow(sel))) {
          if (sel$source[i] == "drawn") {
            present <- FALSE
            if (!is.null(remaining) && nrow(remaining) > 0) {
              eq_mat <- sf::st_equals(sel[i, , drop = FALSE], remaining, sparse = FALSE)
              if (is.matrix(eq_mat) && any(as.logical(eq_mat))) present <- TRUE
            }
            if (!present) keep_idx[i] <- FALSE
          }
        }
        new_sel <- sel[keep_idx, , drop = FALSE]
        if (nrow(new_sel) == 0) new_sel <- NULL
        selected_polygons(new_sel)
      }
    }
    
    update_selected_highlights()
  })
  
  # --- Upload handling ---
  observeEvent(input$upload, {
    files <- input$upload; req(files)
    
    cur_fixed_max    <- if (!is.null(fixed_polys())) max(as.integer(fixed_polys()$layer_id), na.rm = TRUE) else 0
    cur_uploaded_max <- if (!is.null(uploaded_polys())) max(as.integer(uploaded_polys()$layer_id), na.rm = TRUE) else 0
    cur_drawn_max    <- if (!is.null(drawn_features())) max(as.integer(drawn_features()$layer_id), na.rm = TRUE) else 0
    start_id <- max(cur_fixed_max, cur_uploaded_max, cur_drawn_max, 0) + 1
    
    # clear project selection state so project layers disappear correctly when switching to upload mode
    clear_project_selection_state()
    session$sendCustomMessage("ndc_force_clear_fixed_sidebar", NULL)
    clear_map_polygons(except = c("uploaded"))
    session$sendCustomMessage("ndc_select_fixed", NULL)
    
    res <- process_uploaded_files(files, start_layer_id = start_id)
    imported <- res$imported; pending <- res$pending_gpkg
    
    # Imported features (non-gpkg/simple files)
    if (!is.null(imported) && nrow(imported) > 0) {
      existing_max <- if (!is.null(uploaded_polys())) max(as.integer(uploaded_polys()$layer_id), na.rm = TRUE) else 0
      new_layer_id <- as.integer(existing_max + 1)
      imported$layer_id <- new_layer_id
      imported$wkt <- sf::st_as_text(sf::st_geometry(imported))
      imported$source <- "uploaded"
      imported$pending_pid <- NA_character_
      imported$pending_layer <- NA_character_
      base_name <- if (!is.null(files$name) && length(files$name) >= 1) files$name[1] else "Uploaded"
      imported <- assign_sequential_source_names(imported, base_name = base_name, overview(), fixed_polys(), uploaded_polys(), drawn_features())
      
      if (is.null(uploaded_polys())) {
        tmp_up <- imported
      } else {
        tmp_up <- dplyr::bind_rows(uploaded_polys(), imported)
      }
      tmp_up <- assign_uploaded_colors_server(tmp_up)
      uploaded_polys(tmp_up)
      
      zoom_to_sf(imported)
    }
    
    # Pending gpkg entries
    if (length(pending) > 0) {
      cur_pending <- pending_gpkg()
      for (p in pending) {
        pid_hash <- tryCatch({
          as.character(tools::md5sum(p$datapath)[[1]])
        }, error = function(e) {
          paste0("tmp_", as.integer(runif(1, 1e5, 1e6)))
        })
        pid <- paste0("pg_", pid_hash)
        
        if (!is.null(cur_pending) && pid %in% names(cur_pending)) {
          cur_pending[[pid]]$name <- p$name
          cur_pending[[pid]]$datapath <- p$datapath
          cur_pending[[pid]]$all_layers <- p$layers
          prev_imported <- cur_pending[[pid]]$imported
          if (is.null(prev_imported)) prev_imported <- character(0)
          cur_pending[[pid]]$layers <- setdiff(p$layers, prev_imported)
          cur_pending[[pid]]$original_name <- p$original_name
        } else {
          cur_pending[[pid]] <- list(
            id = pid,
            name = p$name,
            datapath = p$datapath,
            all_layers = p$layers,
            layers = p$layers,
            imported = character(0),
            original_name = p$original_name
          )
        }
      }
      pending_gpkg(cur_pending)
    }
    
    if (!is.null(uploaded_polys())) {
      leaflet::leafletProxy("map") %>% leaflet::clearGroup("uploaded") %>%
        leaflet::addPolygons(data = uploaded_polys(), group = "uploaded", color = ~color, fillColor = ~color, fillOpacity = 0.25, weight = 2, layerId = ~layer_id, label = ~source_name, labelOptions = leaflet::labelOptions(direction = "auto"))
    }
    
    n_imp <- if (!is.null(imported)) nrow(imported) else 0
    n_pending <- length(pending)
    msg_parts <- c()
    if (n_imp > 0) msg_parts <- c(msg_parts, paste0("Imported ", n_imp, " polygon(s)."))
    if (n_pending > 0) msg_parts <- c(msg_parts, paste0(n_pending, " geopackage(s) require layer selection. See upload panel below."))
    if (length(msg_parts) > 0) showNotification(paste(msg_parts, collapse = " "), type = "message", duration = 6)
  })
  
  # upload panel UI
  output$upload_panel <- renderUI({
    pending <- pending_gpkg()
    uploaded <- uploaded_polys()
    
    tagList(
      if (length(pending) > 0) {
        wellPanel(
          h5("GeoPackage layer selection"),
          p("One or more uploaded GeoPackage files have multiple layers. Choose which layer to import and click Import."),
          lapply(names(pending), function(pid) {
            pinfo <- pending[[pid]]
            ns_import_id <- paste0("import_gpkg_", pid)
            select_id <- paste0("select_gpkg_", pid)
            tagList(
              tags$div(style = "margin-bottom:8px;",
                       strong(pinfo$name),
                       br(),
                       selectInput(select_id, "Choose layer:", choices = pinfo$layers, selected = ifelse(length(pinfo$layers)>0, pinfo$layers[1], "")),
                       actionButton(ns_import_id, "Import selected layer", class = "btn-custom")
              )
            )
          })
        )
      } else NULL,
      
      wellPanel(
        h5("Uploaded layers"),
        if (is.null(uploaded) || nrow(uploaded) == 0) {
          p("No uploaded/imported polygon layers yet.", style = "color: grey; font-style: italic;")
        } else {
          uniq <- dplyr::distinct(uploaded, layer_id, .keep_all = TRUE)
          tags$table(class = "table table-condensed",
                     tags$thead(tags$tr(tags$th("Source"), tags$th("Features"), tags$th("Remove"))),
                     tags$tbody(
                       lapply(seq_len(nrow(uniq)), function(i) {
                         row <- uniq[i, ]
                         tags$tr(
                           tags$td(row$source_name),
                           tags$td("1"),
                           tags$td(actionButton(paste0("remove_uploaded_", row$layer_id), "Remove", onclick = sprintf("Shiny.setInputValue('remove_uploaded', %d, {priority: 'event'})", row$layer_id), class = "btn-delete"))
                         )
                       })
                     )
          )
        }
      )
    )
  })
  
  # dynamic import observers for pending gpkg layers
  observe({
    pending <- pending_gpkg()
    if (length(pending) == 0) {
      created_observers(character(0))
      return(NULL)
    }
    
    existing_created <- created_observers()
    pids <- names(pending)
    for (pid in pids) {
      if (pid %in% existing_created) next
      
      local({
        my_pid <- pid
        import_btn_id <- paste0("import_gpkg_", my_pid)
        select_id     <- paste0("select_gpkg_", my_pid)
        
        observeEvent(input[[import_btn_id]], {
          cur_pending_all <- pending_gpkg()
          p <- cur_pending_all[[my_pid]]
          if (is.null(p)) {
            showNotification("Pending file not found (it may have been imported already).", type = "error")
            return(NULL)
          }
          
          chosen_layer <- input[[select_id]]
          if (is.null(chosen_layer) || chosen_layer == "") {
            showNotification("Please select a layer before importing.", type = "error")
            return(NULL)
          }
          
          sf_obj <- tryCatch(sf::st_read(p$datapath, layer = chosen_layer, quiet = TRUE), error = function(e) NULL)
          if (is.null(sf_obj)) {
            showNotification(paste0("Failed to read layer ", chosen_layer, " from ", p$name), type = "error")
            return(NULL)
          }
          
          poly_only <- sf_obj[sf::st_is(sf_obj, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE]
          if (nrow(poly_only) == 0) {
            showNotification(paste0("Layer ", chosen_layer, " does not contain polygon features."), type = "error")
            return(NULL)
          }
          
          poly_only <- sf::st_transform(poly_only, 4326)
          poly_only$source <- "uploaded"
          poly_only$pending_layer <- chosen_layer
          base_name <- paste0(p$name, " :: ", chosen_layer)
          existing_max <- if (!is.null(uploaded_polys())) max(as.integer(uploaded_polys()$layer_id), na.rm = TRUE) else 0
          new_layer_id <- as.integer(existing_max + 1)
          poly_only$layer_id <- new_layer_id
          poly_only$wkt <- sf::st_as_text(sf::st_geometry(poly_only))
          poly_only$pending_pid <- my_pid
          
          poly_only <- assign_sequential_source_names(poly_only, base_name = base_name, overview(), fixed_polys(), uploaded_polys(), drawn_features())
          
          if (is.null(uploaded_polys())) {
            tmp_up <- poly_only
          } else {
            tmp_up <- dplyr::bind_rows(uploaded_polys(), poly_only)
          }
          tmp_up <- assign_uploaded_colors_server(tmp_up)
          uploaded_polys(tmp_up)
          
          prev_imported <- cur_pending_all[[my_pid]]$imported
          prev_imported <- if (is.null(prev_imported)) character(0) else prev_imported
          new_imported <- unique(c(prev_imported, chosen_layer))
          cur_pending_all[[my_pid]]$imported <- new_imported
          all_layers_list <- cur_pending_all[[my_pid]]$all_layers
          remaining_layers <- setdiff(all_layers_list, new_imported)
          cur_pending_all[[my_pid]]$layers <- remaining_layers
          pending_gpkg(cur_pending_all)
          
          leaflet::leafletProxy("map") %>%
            leaflet::clearGroup("uploaded") %>%
            {
              if (!is.null(uploaded_polys())) leaflet::addPolygons(., data = uploaded_polys(), group = "uploaded", color = ~color, fillColor = ~color, fillOpacity = 0.25, weight = 2, layerId = ~layer_id, label = ~source_name, labelOptions = leaflet::labelOptions(direction = "auto")) else .
            }
          
          zoom_to_sf(poly_only)
          showNotification(paste0("Imported layer '", chosen_layer, "' from ", p$name), type = "message", duration = 5)
        }, ignoreNULL = TRUE)
      })
      
      created_observers(unique(c(created_observers(), pid)))
    } # end for
  })
  
  # remove uploaded polygon / row by layer_id
  observeEvent(input$remove_uploaded, {
    lid <- as.integer(input$remove_uploaded)
    if (is.na(lid)) return(NULL)
    cur <- uploaded_polys()
    if (is.null(cur) || nrow(cur) == 0) return(NULL)
    lip <- which(as.integer(cur$layer_id) == lid)
    if (length(lip) > 0) {
      removed_source_name <- cur$source_name[lip[1]]
      removed_pid <- if ("pending_pid" %in% names(cur)) cur$pending_pid[lip[1]] else NA_character_
      removed_pending_layer <- if ("pending_layer" %in% names(cur)) cur$pending_layer[lip[1]] else NA_character_
      
      new <- cur[-lip, , drop = FALSE]
      if (nrow(new) == 0) new <- NULL
      uploaded_polys(new)
      leaflet::leafletProxy("map") %>% leaflet::clearGroup("uploaded")
      if (!is.null(new)) leaflet::leafletProxy("map") %>% leaflet::addPolygons(data = new, group = "uploaded", color = ~color, fillColor = ~color, fillOpacity = 0.25, weight = 2, layerId = ~layer_id, label = ~source_name, labelOptions = leaflet::labelOptions(direction = "auto"))
      
      sel <- selected_polygons()
      if (!is.null(sel) && nrow(sel) > 0) {
        if ("source" %in% names(sel)) {
          keep_idx <- !(sel$source == "uploaded" & as.integer(sel$layer_id) == lid)
          new_sel <- sel[keep_idx, , drop = FALSE]
          if (nrow(new_sel) == 0) new_sel <- NULL
          selected_polygons(new_sel)
          update_selected_highlights()
        }
      }
      
      if (!is.na(removed_pid) && nzchar(removed_pid)) {
        cur_pending <- pending_gpkg()
        if (!is.null(cur_pending) && removed_pid %in% names(cur_pending)) {
          chosen_layer <- removed_pending_layer
          if (is.na(chosen_layer) || !nzchar(chosen_layer)) {
            chosen_layer <- sub("^.*::\\s*", "", removed_source_name)
          }
          cur_imported <- cur_pending[[removed_pid]]$imported
          if (is.null(cur_imported)) cur_imported <- character(0)
          cur_pending[[removed_pid]]$imported <- setdiff(cur_imported, chosen_layer)
          all_layers_list <- cur_pending[[removed_pid]]$all_layers
          if (is.null(all_layers_list)) all_layers_list <- cur_pending[[removed_pid]]$layers
          cur_pending[[removed_pid]]$layers <- setdiff(all_layers_list, cur_pending[[removed_pid]]$imported)
          pending_gpkg(cur_pending)
        } else {
          pid_new <- paste0("pg_readd_", as.integer(runif(1, 1e5, 1e6)))
          chosen_layer <- removed_pending_layer
          if (is.na(chosen_layer) || !nzchar(chosen_layer)) chosen_layer <- sub("^.*::\\s*", "", removed_source_name)
          tmp_entry <- list(
            id = pid_new,
            name = removed_source_name,
            datapath = NA_character_,
            all_layers = chosen_layer,
            layers = chosen_layer,
            imported = character(0),
            original_name = removed_source_name
          )
          cur_pending <- pending_gpkg()
          cur_pending[[pid_new]] <- tmp_entry
          pending_gpkg(cur_pending)
        }
      }
      
      showNotification("Uploaded layer removed.", type = "message", duration = 4)
    }
  }, ignoreInit = TRUE)
  
  # Metadata UI — now includes a per-dataset tabset with uiOutput targets where server will render the controls
  output$dataset_metadata <- renderUI({
    req(input$selected_dataset)
    ds <- input$selected_dataset
    
    make_ds_tabset <- function(ds_name) {
      tid <- make_tab_id(ds_name)
      header_text <- paste0("Configure data request - ", ds_name)
      header_tag <- tags$h4(header_text, style = "color: #1f5a8a; margin-top: 6px; margin-bottom: 8px;")
      tagList(
        header_tag,
        tabsetPanel(
          id = tid,
          type = "tabs",
          tabPanel(title = "Raster", value = "Raster", uiOutput(make_target_id(ds_name, "Raster")))
        )
      )
    }
    
    make_ds_tabset(ds)
  })
  
  # ---------- Helper: controls UI builder ----------
  build_controls_for <- function(ds) {
    if (is.null(ds) || ds == "") return(NULL)
    
    layer_input_id <- make_layer_input_id(ds)
    choices <- if (!is.null(dataset_layer_choices[[ds]])) dataset_layer_choices[[ds]] else character(0)
    
    n_choices <- length(choices)
    ncols <- if (n_choices <= 10) 1 else if (n_choices <= 30) 2 else 3
    colstyle <- paste0("column-count:", ncols, "; -webkit-column-count:", ncols, ";")
    
    select_buttons <- tags$div(style = "margin-top:6px; margin-bottom:6px; display:flex; gap:8px;",
                               actionButton(paste0(layer_input_id, "_select_all"), "Select all", class = "btn-custom small-btn"),
                               actionButton(paste0(layer_input_id, "_deselect_all"), "Deselect all", class = "btn-delete small-btn", disabled = "disabled")
    )
    
    checkbox_label_generic <- tags$span(class = "checkbox-label-lg", "Choose sub-layers (placeholder):")
    checkbox_label_weather <- tags$span(class = "checkbox-label-xl", "Choose weather variables to download:")
    checkbox_label_ndvi <- tags$span(class = "checkbox-label-lg", "Choose NDVI sub-layers (placeholder):")
    
    if (ds == "Agricultural fields") {
      tagList(
        tags$div(class = "dataset-controls",
                 numericInput("selected_year", "Select year:", value = 2025, min = 2020, max = 2025),
                 tags$div(class = "layer-checkboxes", style = colstyle,
                          checkboxGroupInput(layer_input_id, label = checkbox_label_generic, choices = choices, selected = character(0))
                 ),
                 select_buttons
        )
      )
    } else if (ds == "Weather") {
      tagList(
        tags$div(class = "dataset-controls",
                 radioButtons("weather_mode", label = NULL, choices = c("Single date" = "single", "Period" = "period"), selected = "single", inline = TRUE),
                 conditionalPanel(condition = "input.weather_mode == 'single'",
                                  dateInput("weather_date", "Date (single):", value = as.Date("2025-01-01"), min = as.Date("2017-01-01"), max = as.Date("2025-01-01"))
                 ),
                 conditionalPanel(condition = "input.weather_mode == 'period'",
                                  dateRangeInput("weather_period", "From - To:", start = as.Date("2024-12-01"), end = as.Date("2025-01-01"), min = as.Date("2017-01-01"), max = as.Date("2025-01-01"))
                 ),
                 tags$div(class = "layer-checkboxes", style = colstyle,
                          checkboxGroupInput(layer_input_id, label = checkbox_label_weather, choices = choices, selected = character(0))
                 ),
                 select_buttons
        )
      )
    } else if (ds == "NDVI") {
      tagList(
        tags$div(class = "dataset-controls",
                 radioButtons("ndvi_mode", "NDVI query type:", choices = c("Single month" = "single", "Range of months" = "range"), selected = "single", inline = TRUE),
                 conditionalPanel(condition = "input.ndvi_mode == 'single'", numericInput("ndvi_year", "Year:", value = 2025, min = 2017, max = 2025), numericInput("ndvi_month", "Month (1-12):", value = 1, min = 1, max = 12) ),
                 conditionalPanel(condition = "input.ndvi_mode == 'range'", fluidRow(
                   column(6, numericInput("ndvi_from_year", "From Year:", value = 2025, min = 2017, max = 2025), numericInput("ndvi_from_month", "From Month (1-12):", value = 1, min = 1, max = 12)),
                   column(6, numericInput("ndvi_to_year", "To Year:", value = 2025, min = 2017, max = 2025), numericInput("ndvi_to_month", "To Month (1-12):", value = 12, min = 1, max = 12))
                 )),
                 tags$div(class = "layer-checkboxes", style = colstyle,
                          checkboxGroupInput(layer_input_id, label = checkbox_label_ndvi, choices = choices, selected = character(0))
                 ),
                 select_buttons
        )
      )
    } else {
      tagList(
        tags$div(class = "dataset-controls",
                 tags$div(class = "layer-checkboxes", style = colstyle,
                          checkboxGroupInput(layer_input_id, label = checkbox_label_generic, choices = choices, selected = character(0))
                 ),
                 select_buttons
        )
      )
    }
  }
  
  # Create renderUI outputs for every dataset/tab target so controls can be server-rendered into them.
  for (ds_name in all_dataset_names) {
    for (tab_val in c("Raster", "Statistics")) {
      local({
        dsn <- ds_name
        tv <- tab_val
        tgt <- make_target_id(dsn, tv)
        tab_input_id <- make_tab_id(dsn)
        output[[tgt]] <- renderUI({
          if (is.null(input$selected_dataset) || input$selected_dataset != dsn) return(NULL)
          current_tab <- if (!is.null(input[[tab_input_id]])) input[[tab_input_id]] else "Raster"
          if (is.null(current_tab) || tolower(as.character(current_tab)) != tolower(tv)) return(NULL)
          build_controls_for(dsn)
        })
      })
    }
  }
  
  # Create observers for select-all / deselect-all for each dataset using predetermined choice lists.
  for (ds in all_dataset_names) {
    local({
      my_ds <- ds
      layer_input_id <- make_layer_input_id(my_ds)
      select_id <- paste0(layer_input_id, "_select_all")
      deselect_id <- paste0(layer_input_id, "_deselect_all")
      choices <- dataset_layer_choices[[my_ds]]
      
      observeEvent(input[[select_id]], {
        if (length(choices) > 0) {
          updateCheckboxGroupInput(session, layer_input_id, selected = choices)
        } else {
          updateCheckboxGroupInput(session, layer_input_id, selected = character(0))
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input[[deselect_id]], {
        updateCheckboxGroupInput(session, layer_input_id, selected = character(0))
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observe({
        sel_vals <- NULL
        try({
          sel_vals <- input[[layer_input_id]]
        }, silent = TRUE)
        enabled <- !is.null(sel_vals) && length(sel_vals) > 0
        session$sendCustomMessage("ndc_toggle_deselect", list(id = deselect_id, enabled = enabled))
      })
    })
  }
  
  observeEvent(input$selected_dataset, {
    NULL
  }, ignoreNULL = FALSE)
  

  observeEvent(input$add_dataset, {
    sel <- selected_polygons()
    if (is.null(sel) || nrow(sel) == 0) {
      showNotification("⚠ Please select or draw at least one polygon first.", type = "error", duration = 5)
      return(NULL)
    }
    req(input$selected_dataset)
    
    year_val <- NA_integer_; date_from_val <- as.Date(NA); date_to_val <- as.Date(NA)
    
    if (input$selected_dataset == "Agricultural fields") {
      req(input$selected_year); year_val <- as.integer(input$selected_year)
    } else if (input$selected_dataset == "NDVI") {
      if (input$ndvi_mode == "single") {
        date_from_val <- as.Date(sprintf("%04d-%02d-01", input$ndvi_year, input$ndvi_month))
        date_to_val <- (as.Date(sprintf("%04d-%02d-01", input$ndvi_year, input$ndvi_month)) + months(1)) - 1
      } else {
        date_from_val <- as.Date(sprintf("%04d-%02d-01", input$ndvi_from_year, input$ndvi_from_month))
        next_month <- as.Date(sprintf("%04d-%02d-01", input$ndvi_to_year, input$ndvi_to_month)) + months(1)
        date_to_val <- next_month - 1
      }
      year_val <- NA_integer_
    } else if (input$selected_dataset == "Weather") {
      if (is.null(input$weather_mode) || input$weather_mode == "single") { date_from_val <- as.Date(input$weather_date); date_to_val <- as.Date(input$weather_date) } else { date_from_val <- as.Date(input$weather_period[1]); date_to_val <- as.Date(input$weather_period[2]) }
      year_val <- NA_integer_
    } else {
      year_val <- NA_integer_
    }
    
    ds_label_base <- input$selected_dataset
    tab_input_id <- make_tab_id(ds_label_base)
    cur_tab_val <- if (!is.null(isolate(input[[tab_input_id]]))) isolate(input[[tab_input_id]]) else "Raster"
    cur_tab_val_clean <- if (tolower(as.character(cur_tab_val)) == "statistics") "Statistics" else "Raster"
    
    ds_label <- ds_label_base
    view_label <- cur_tab_val_clean
    
    layer_input_id <- make_layer_input_id(ds_label)
    layers_sel <- NULL
    if (!is.null(input[[layer_input_id]])) {
      layers_sel <- input[[layer_input_id]]
    } else {
      layers_sel <- character(0)
    }
    layers_sig <- if (length(layers_sel) == 0) "" else paste(sort(unique(layers_sel)), collapse = "|")
    
    ov <- overview()
    new_rows <- list()
    
    for (i in seq_len(nrow(sel))) {
      poly <- sel[i, , drop = FALSE]
      
      assigned_name <- NULL
      if ("source_name" %in% names(poly) && !is.na(poly$source_name) && nzchar(as.character(poly$source_name[1]))) {
        assigned_name <- as.character(poly$source_name[1])
      } else {
        base_name <- "Own polygon"
        if (!is.null(fixed_polys())) {
          fixed_match <- fixed_polys() %>% dplyr::filter(layer_id == poly$layer_id)
          if (nrow(fixed_match) > 0) base_name <- input$fixed_layer
        }
        if (!is.null(uploaded_polys())) {
          uploaded_match <- uploaded_polys() %>% dplyr::filter(layer_id == poly$layer_id)
          if (nrow(uploaded_match) > 0) {
            base_name <- as.character(uploaded_match$source_name[1])
          }
        }
        if ("source" %in% names(poly) && poly$source == "drawn") {
          base_name <- "Own polygon"
        }
        tmp <- poly
        if (!"geometry" %in% names(tmp)) tmp <- sf::st_as_sf(tmp)
        tmp <- assign_sequential_source_names(tmp, base_name = base_name, overview(), fixed_polys(), uploaded_polys(), drawn_features())
        assigned_name <- as.character(tmp$source_name[1])
      }
      
      duplicate_check <- ov %>% dplyr::filter(
        dataset == ds_label,
        view == view_label,
        (is.na(year) & is.na(year_val) | year == year_val),
        (is.na(date_from) & is.na(date_from_val) | date_from == date_from_val),
        (is.na(date_to) & is.na(date_to_val) | date_to == date_to_val),
        layers_sig == layers_sig
      )
      
      is_exact_dup <- FALSE
      if (nrow(duplicate_check) > 0) {
        if (!is.null(duplicate_check$wkt) && !is.null(poly$wkt)) {
          if (any(as.character(duplicate_check$wkt) == as.character(poly$wkt[1]))) is_exact_dup <- TRUE
        }
      }
      
      if (!is_exact_dup) {
        new_rows[[length(new_rows) + 1]] <- tibble::tibble(
          dataset = ds_label,
          view = view_label,
          year = year_val,
          polygon = assigned_name,
          wkt = poly$wkt[1],
          polygon_sf = list(poly),
          date_from = date_from_val,
          date_to = date_to_val,
          layers = list(as.character(layers_sel)),
          layers_sig = layers_sig
        )
      }
    } # end for selected polygons
    
    if (length(new_rows) > 0) {
      new_df <- dplyr::bind_rows(new_rows)
      overview(dplyr::bind_rows(ov, new_df))
    } else {
      showNotification("Selected dataset(s) already present for the selected polygon(s) with the same settings.", type = "message")
    }
    
  })
  
  output$overview_table <- renderUI({
    ov <- overview()
    if (is.null(ov) || nrow(ov) == 0) {
      return(p("No datasets added yet.", style = "color: grey; font-style: italic;"))
    }
    
    display_dates <- vapply(seq_len(nrow(ov)), function(i) {
      row <- ov[i, ]
      
      if (!is.null(row$dataset) && row$dataset == "NDVI") {
        df <- row$date_from; dt <- row$date_to
        df2 <- if (!is.na(df)) tryCatch(as.Date(df), error = function(e) NA) else NA
        dt2 <- if (!is.na(dt)) tryCatch(as.Date(dt), error = function(e) NA) else NA
        
        if (!is.na(df2) && !is.na(dt2)) {
          if (format(df2, "%Y-%m") == format(dt2, "%Y-%m")) {
            format(df2, "%Y-%m")
          } else {
            paste0(format(df2, "%Y-%m"), " - ", format(dt2, "%Y-%m"))
          }
        } else if (!is.na(df2)) {
          format(df2, "%Y-%m")
        } else if (!is.na(dt2)) {
          format(dt2, "%Y-%m")
        } else if (!is.na(row$year)) {
          as.character(row$year)
        } else ""
      } else {
        df <- row$date_from; dt <- row$date_to
        df2 <- if (!is.na(df)) tryCatch(as.Date(df), error = function(e) NA) else NA
        dt2 <- if (!is.na(dt)) tryCatch(as.Date(dt), error = function(e) NA) else NA
        
        if (!is.na(df2) && !is.na(dt2)) {
          if (df2 == dt2) format(df2, "%Y-%m-%d") else paste0(format(df2, "%Y-%m-%d"), " - ", format(dt2, "%Y-%m-%d"))
        } else if (!is.na(df2)) {
          format(df2, "%Y-%m-%d")
        } else if (!is.na(dt2)) {
          format(dt2, "%Y-%m-%d")
        } else if (!is.na(row$year)) {
          as.character(row$year)
        } else ""
      }
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    
    display_polygon <- as.character(ov$polygon)
    
    tags$table(
      class = "table table-striped table-bordered",
      tags$thead(tags$tr(
        tags$th("Dataset"),
        tags$th("Type"),
        tags$th("Date"),
        tags$th("Polygon"),
        tags$th("Layers"),
        tags$th("Delete")
      )),
      tags$tbody(
        lapply(seq_len(nrow(ov)), function(i) {
          row <- ov[i, ]
          layers_txt <- ""
          if (!is.null(row$layers[[1]]) && length(row$layers[[1]]) > 0) layers_txt <- paste(row$layers[[1]], collapse = ", ")
          tags$tr(
            tags$td(row$dataset),
            tags$td(row$view),
            tags$td(display_dates[i]),
            tags$td(display_polygon[i]),
            tags$td(layers_txt),
            tags$td(actionButton(
              paste0("delete_row_", i),
              "x",
              onclick = sprintf("Shiny.setInputValue('delete_row', %d, {priority: 'event'})", i),
              class = "btn-delete"))
          )
        })
      )
    )
  })
  
  empty_overview <- tibble::tibble(
    dataset = character(),
    view = character(),
    year = integer(),
    polygon = character(),
    wkt = character(),
    polygon_sf = list(),
    date_from = as.Date(character()),
    date_to = as.Date(character()),
    layers = list(),
    layers_sig = character()
  )
  
  observeEvent(input$delete_row, {
    idx <- as.integer(input$delete_row)
    if (is.na(idx)) return(NULL)
    
    ov <- overview()
    if (is.null(ov) || nrow(ov) == 0) {
      showNotification("Nothing to delete.", type = "warning")
      return(NULL)
    }
    
    if (idx < 1 || idx > nrow(ov)) {
      showNotification("That row no longer exists (index out of range).", type = "error")
      return(NULL)
    }
    
    new_ov <- ov[-idx, , drop = FALSE]
    if (nrow(new_ov) == 0) {
      overview(empty_overview)
    } else {
      overview(new_ov)
    }
    
    update_selected_highlights()
    showNotification("Row removed from overview.", type = "message")
  }, ignoreInit = TRUE)
  
  observeEvent(input$clear_overview, {
    cur_sel <- selected_polygons()
    overview(empty_overview)
    if (!is.null(cur_sel) && nrow(cur_sel) > 0) {
      selected_polygons(cur_sel)
    }
    update_selected_highlights()
    showNotification("Overview cleared", type = "message")
  }, ignoreInit = TRUE)
  
  # --------------------
  # Retrieval / Download UI & handlers
  # --------------------
  retrieve_and_save <- function(zipfile = NULL, workdir = NULL) {
    ov <- overview(); req(nrow(ov) > 0)
    if (is.null(workdir)) workdir <- file.path(tempdir(), "export")
    if (dir.exists(workdir)) unlink(workdir, recursive = TRUE)
    dir.create(workdir, recursive = TRUE)
    download_msgs(character(0)); local_msgs <- character(0); results <- list()
    
    withProgress(message = "Retrieving datasets...", value = 0, {
      for (i in seq_len(nrow(ov))) {
        ds <- ov$dataset[i]
        view_i <- ov$view[i]
        layers_i <- ov$layers[[i]]
        outfile <- file.path(workdir, paste0(ds, "_", view_i, "_", i, ".geojson"))
        tryCatch({
          local_msgs <- c(local_msgs, paste0("Retrieved: ", ds, " (", view_i, ") with layers: ", ifelse(length(layers_i)==0, "<all>", paste(layers_i, collapse = ", "))))
        }, error = function(e) {
          local_msgs <<- c(local_msgs, paste0("Failed: ", ds, " - ", e$message))
        })
        incProgress(1 / nrow(ov))
      }
    })
    
    if (!is.null(zipfile)) zip::zipr(zipfile, list.files(workdir, full.names = TRUE))
    download_msgs(c(download_msgs(), local_msgs))
    list(overview = ov, messages = download_msgs(), zipfile = zipfile, datasets = results)
  }
  
  output$download_ui <- renderUI({
    if (nrow(overview()) == 0) {
      div(style = "color: grey; font-style: italic;", "Download button will appear here once you add a dataset.")
    } else {
      div(style = "display:flex; gap:10px;",
          downloadButton("download_data", "Download dataset(s)", class = "btn-custom"),
          actionButton("return_to_r", "Return data to R (close app)", class = "btn-custom")
      )
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("naturedatacube_", Sys.Date(), ".zip"),
    content = function(zipfile) retrieve_and_save(zipfile)
  )
  
  output$download_messages <- renderUI({
    msgs <- download_msgs()
    if (length(msgs) == 0) p("Updates on dataset retrieval will appear here.", style = "color: grey; font-style: italic;") else HTML(paste(msgs, collapse = "<br>"))
  })
  
  observeEvent(input$return_to_r, {
    res <- retrieve_and_save()
    stopApp(res)
  })
  
  observe({
    disable_stats <- FALSE
    df <- drawn_features()
    up <- uploaded_polys()
    if (!is.null(df) && nrow(df) > 0) disable_stats <- TRUE
    if (!is.null(up) && nrow(up) > 0) disable_stats <- TRUE
    session$sendCustomMessage("ndc_toggle_statistics", disable_stats)
  })
  
  # Floating help button: open README modal
  observeEvent(input$open_guide, {
    read_readme <- function() {
      candidates <- c(
        here::here("NatureDataCube_README.txt"),
        "NatureDataCube_README.txt",
        "/mnt/data/NatureDataCube_README.txt"
      )
      for (p in candidates) {
        if (!is.null(p) && file.exists(p)) {
          txt <- tryCatch(readLines(p, warn = FALSE), error = function(e) NULL)
          if (!is.null(txt)) return(paste(txt, collapse = "<br>"))
        }
      }
      return("Help file not found. Please place 'NatureDataCube_README.txt' in the app directory.")
    }
    content <- read_readme()
    showModal(modalDialog(
      title = "How to use this app — Quick guide",
      HTML(content),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  }, ignoreInit = TRUE)
  
} # end server

# --------------------
# Zoom helper
# --------------------
zoom_to_sf <- function(sf_obj) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(invisible(NULL))
  bb <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
  leaflet::leafletProxy("map") %>%
    leaflet::fitBounds(
      as.numeric(bb["xmin"]),
      as.numeric(bb["ymin"]),
      as.numeric(bb["xmax"]),
      as.numeric(bb["ymax"])
    )
}

# --------------------
# Run the app
# --------------------
shinyApp(ui, server)
