# Nitrogen / Nature Data Cube configuration
# ----------------------------------------
# Keep the token separate from the rest of the app.
# Set this in your environment, e.g.:
# Sys.setenv(NDC_NATURE_TOKEN = "...")

nitrogen_endpoint <- "https://ndc-test.containers.wur.nl/api/"
nitrogen_collection <- "ndc-geoserver-rasters"
nitrogen_asset_name <- "wcs"

nitrogen_year_choices <- c("2024", "2025", "2040")
nitrogen_layer_choices <- c("ntot", "nox", "nh3")

nitrogen_default_year <- "2024"
nitrogen_default_layers <- nitrogen_layer_choices

nitrogen_clean_layer_name <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[nzchar(x)]
}

nitrogen_normalize_year <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  ifelse(nzchar(x), x, NA_character_)
}

nitrogen_controls_ui <- function() {
  shiny::tagList(
    shiny::tags$div(
      class = "dataset-controls",
      shiny::selectInput(
        "nitrogen_year",
        "Select year:",
        choices = nitrogen_year_choices,
        selected = nitrogen_default_year,
        multiple = FALSE
      ),
      shiny::tags$div(
        style = "margin-top: 6px; color: #5a6472;",
        "Retrieval will return all nitrogen rasters: ntot, nox, and nh3."
      )
    )
  )
}

nitrogen_controls_are_valid <- function(year, layers) {
  year_ok <- !is.null(year) && !is.na(year) && nzchar(as.character(year))
  layers_ok <- !is.null(layers) && length(layers) > 0 && any(nzchar(as.character(layers)))
  year_ok && layers_ok
}
