# ============================================================
# R Shiny NatureDataCube interface demo version: NAEM
# ============================================================
# Created on : 26 Jan 2026
# Last update : 30 Jan 2026
#
# Description:
# This script is based on the previous version created by Minke Mulder
# and this Shiny application allows users to:
#
# - Select a predefined study area or draw a custom polygon
# - Choose one or more NatureDataCube datasets
# - Configure dataset-specific parameters (for now only year)
# - Retrieve spatial data via the NatureDataCube API
# - Download results as GeoJSON files in a ZIP file or use them in a R variable
#
# how to run it:
# 1. first load / install the libraries
# 2. set the working directory to NatureDataCube-Shiny-NAEM
# 3. data_nc <- runApp("R/naturedatacube_app/app.R")
#
# Design rules for now:
# - ONE polygon (study area) per download / export
# - Multiple datasets are allowed for that polygon
# - Geometry is sent to the API as WKT (EPSG:4326)
# ============================================================


# ============================================================
# Libraries
# ============================================================

load_pkgs <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(to_install)) {
    install.packages(to_install)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

pkgs <- c(
  "shiny",
  "leaflet",
  "leaflet.extras",
  "sf",
  "dplyr",
  "purrr",
  "stringr",
  "httr",
  "geojsonsf",
  "jsonlite",
  "zip",
  "here",
  "terra",
  "lubridate"
)

load_pkgs(pkgs)


# ============================================================
# NatureDataCube API configuration
# ============================================================

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

mytoken <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3N1ZWR0byI6ImQubGljaHRlbmJlcmdAbmlvby5rbmF3Lm5sIiwicmVzb3VyY2UiOlsiKiJdLCJpYXQiOjE3NjgyMTM3OTZ9.nEmOwkBTzKBjlsZL8obY-kWvghNS4A1M1Vwv1B94SSU"

myheaders <- c(
  "Accept" = "application/json;charset=utf-8",
  "token"  = mytoken
)


# ============================================================
# Load polygon data
# ============================================================
gpkg <- here::here("data", "study_sites.gpkg")
layers <- sf::st_layers(gpkg)$name

all_layers <- set_names(
  map(layers, ~ st_read(gpkg, layer = .x, quiet = TRUE)),
  layers
)

nutnet <- all_layers[["nutnet_poly"]]
nestboxes <- all_layers[str_detect(names(all_layers), "^nestkasten_")] %>%
  reduce(rbind)
loobos <- all_layers[["loobos"]]
lights <- all_layers[["20251008_licht_op_natuur_lantaarnpalen"]]


# ============================================================
# add WKT column
# ============================================================
add_wkt_column <- function(sf_obj, col_name = "wkt") {
  sf_obj <- st_transform(sf_obj, 4326)
  sf_obj[[col_name]] <- st_as_text(st_geometry(sf_obj))
  sf_obj
}

nutnet_wkt    <- add_wkt_column(nutnet)
nestboxes_wkt <- add_wkt_column(nestboxes)
loobos_wkt    <- add_wkt_column(loobos)
lights_wkt    <- add_wkt_column(lights)

# ============================================================
# drawn polygon to sf
# ============================================================

convert_drawn_to_sf <- function(feat, start_layer_id = 1) {
  if (is.null(feat) || feat$geometry$type != "Polygon") return(NULL)
  
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(
    rbind,
    lapply(coords, function(x) as.numeric(unlist(x)))
  )
  
  st_as_sf(st_sfc(st_polygon(list(coords)), crs = 4326)) %>%
    mutate(
      wkt = st_as_text(st_geometry(.)),
      layer_id = start_layer_id
    )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  # title
  titlePanel("Data Nature Cube"),
  
  # custom style
  tags$head(
    tags$style(HTML("
      .btn-custom {
        color: white !important;
        background-color: #28a745 !important;
        border: none !important;
        transition: all 0.2s ease-in-out;
      }
      .btn-custom:hover {
        background-color: #218838 !important;
        color: white !important;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "fixed_layer",
        "Select project or draw own polygon :",
        choices = c("NutNet", "Nestboxes", "Loobos", "Light on Nature")
      ),
      leafletOutput("map", height = "400px"),
      br(),
      h4("Choose dataset for the selected area"),
      radioButtons(
        "selected_dataset",
        "Available datasets:",
        choices = c("Agricultural fields", "AHN", "Soil map", "Weather", "NDVI")
      )
    ),
    
    mainPanel(
      h4("Overview of selected datasets"),
      uiOutput("overview_table"),
      actionButton("clear_overview", "Clear overview",
                   style = "color: white; background-color: grey;"),
      br(),
      uiOutput("download_ui"),
      br(),
      uiOutput("download_messages"),
      hr(),
      h4("Dataset metadata and selection"),
      uiOutput("dataset_metadata"),
      uiOutput("year_ui"),
      actionButton("add_dataset", "Add to overview")
    )
  )
)


# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  
  drawn_polygon <- reactiveVal(NULL)
  fixed_polys   <- reactiveVal(NULL)
  
  overview <- reactiveVal(
    tibble::tibble(
      dataset = character(),
      year = integer(),
      polygon = character(),
      wkt = character(),
      polygon_sf = list(),
      date_from = as.Date(character()),
      date_to = as.Date(character())
    )
  )
  
  download_msgs <- reactiveVal(character(0))
  
  # --- Map ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 5.3, lat = 52.1, zoom = 7) %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polygonOptions = drawPolygonOptions(showArea = TRUE, repeatMode = FALSE),
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, remove = TRUE)
      )
  })
  
  # Fixed polygon selection
  observeEvent(input$fixed_layer, {
    leafletProxy("map") %>%
      clearGroup("fixed") %>%
      clearGroup("highlight_fixed") %>%
      clearPopups()
    
    poly <- switch(
      input$fixed_layer,
      "NutNet" = nutnet_wkt,
      "Nestboxes" = nestboxes_wkt,
      "Loobos" = loobos_wkt,
      "Light on Nature" = lights_wkt
    ) %>%
      mutate(layer_id = row_number())
    
    fixed_polys(poly)
    
    leafletProxy("map") %>% addPolygons(
      data = poly,
      group = "fixed",
      color = "black",
      fillOpacity = 0.3,
      weight = 2,
      layerId = ~layer_id
    )
    
    if (is.null(drawn_polygon())) {
      centroid <- sf::st_centroid(poly[1, ])
      coords <- sf::st_coordinates(centroid)
      
      leafletProxy("map") %>% addPopups(
        lng = coords[1],
        lat = coords[2],
        popup = "⚠ You have selected a project but still need to select or draw a project area.",
        options = popupOptions(closeButton = TRUE)
      )
    }
  })
  
  # Clicking a fixed polygon
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id)
    
    poly <- fixed_polys() %>% filter(layer_id == click$id)
    
    if (nrow(poly) > 0) {
      drawn_polygon(poly)
      leafletProxy("map") %>%
        clearGroup("highlight_fixed") %>%
        addPolygons(
          data = poly,
          group = "highlight_fixed",
          color = "#007bff",
          weight = 4,
          fillOpacity = 0.5
        )
    }
  })
  
  # Drawing a custom polygon
  observeEvent(input$map_draw_new_feature, {
    start_id <- if (is.null(fixed_polys())) 1 else max(fixed_polys()$layer_id, na.rm = TRUE) + 1
    poly_sf <- convert_drawn_to_sf(input$map_draw_new_feature, start_layer_id = start_id)
    req(poly_sf)
    
    drawn_polygon(poly_sf)
    
    leafletProxy("map") %>%
      clearGroup("highlight_drawn") %>%
      clearGroup("highlight_fixed") %>%
      addPolygons(
        data = poly_sf,
        group = "highlight_drawn",
        color = "#007bff",
        weight = 4,
        fillOpacity = 0.5,
        layerId = ~layer_id
      )
  })
  
  observeEvent(input$map_draw_deleted_features, {
    drawn_polygon(NULL)
    leafletProxy("map") %>% clearGroup("highlight_drawn")
  })
  
  # Metadata UI
  output$dataset_metadata <- renderUI({
    req(input$selected_dataset)
    switch(
      input$selected_dataset,
      "Agricultural fields" = HTML("Agricultural fields: Data about crops, yield, and fertilization."),
      "AHN" = HTML("AHN: Elevation data of the Netherlands. Provided as summary statistics only: minimum, maximum and mean (only AHN4 is currently available)."),
      "Soil map" = HTML("Soil map: Soil types / soil map intersections for the selected area."),
      "Weather" = HTML("weather from KNMI"),
      "NDVI" = HTML("Monthly average NDVI")
    )
  })
  
  # Dataset-specific options
  output$year_ui <- renderUI({
    req(input$selected_dataset)
    
    if (input$selected_dataset == "Agricultural fields") {
      numericInput("selected_year", "Select year:", value = 2025, min = 2020, max = 2025)
      
    } else if (input$selected_dataset == "Weather") {
      tagList(
        radioButtons(
          "weather_mode",
          "Weather query type:",
          choices = c("Single date" = "single", "Period" = "period"),
          selected = "single",
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.weather_mode == 'single'",
          dateInput(
            "weather_date",
            "Date (single):",
            value = as.Date("2025-01-01"),
            min = as.Date("2017-01-01"),
            max = as.Date("2025-01-01")
          )
        ),
        conditionalPanel(
          condition = "input.weather_mode == 'period'",
          dateRangeInput(
            "weather_period",
            "From - To:",
            start = as.Date("2024-12-01"),
            end = as.Date("2025-01-01"),
            min = as.Date("2017-01-01"),
            max = as.Date("2025-01-01")
          )
        )
      )
      
    } else if (input$selected_dataset == "NDVI") {
      tagList(
        radioButtons(
          "ndvi_mode", 
          "NDVI query type:",
          choices = c("Single month" = "single", "Range of months" = "range"),
          selected = "single",
          inline = TRUE
        ),
        
        # Single month
        conditionalPanel(
          condition = "input.ndvi_mode == 'single'",
          numericInput("ndvi_year",  "Year:", value = 2025, min = 2017, max = 2025),
          numericInput("ndvi_month", "Month (1-12):", value = 1,  min = 1, max = 12)
        ),
        
        # Range of months
        conditionalPanel(
          condition = "input.ndvi_mode == 'range'",
          fluidRow(
            column(6,
                   numericInput("ndvi_from_year", "From Year:", value = 2025, min = 2017, max = 2025),
                   numericInput("ndvi_from_month", "From Month (1-12):", value = 1, min = 1, max = 12)
            ),
            column(6,
                   numericInput("ndvi_to_year", "To Year:", value = 2025, min = 2017, max = 2025),
                   numericInput("ndvi_to_month", "To Month (1-12):", value = 12, min = 1, max = 12)
            )
          )
        )
      )
      
    } else {
      helpText("This dataset does not require a year or date selection.")
    }
  })
  
  

  # Add to overview
  observeEvent(input$add_dataset, {
    poly <- drawn_polygon()
    
    if (is.null(poly)) {
      showNotification("⚠ Please select or draw a polygon first.", type = "error", duration = 5)
      return(NULL)
    }
    
    req(input$selected_dataset)
    
    # default values
    year_val <- NA_integer_
    date_from_val <- as.Date(NA)
    date_to_val <- as.Date(NA)
    
    # dataset-specific handling
    if (input$selected_dataset == "Agricultural fields") {
      # agricultural fields needs a year
      req(input$selected_year)
      year_val <- as.integer(input$selected_year)
      
    } else if (input$selected_dataset == "NDVI") {
      # NDVI: can be single month or range
      if (input$ndvi_mode == "single") {
        # single month -> mark that month as a single-day range (or first-to-last day)
        year_val <- NA_integer_
        date_from_val <- as.Date(sprintf("%04d-%02d-01", input$ndvi_year, input$ndvi_month))
        # last day of that month
        date_to_val <- (as.Date(sprintf("%04d-%02d-01", input$ndvi_year, input$ndvi_month)) + months(1)) - 1
      } else {
        # range of months
        year_val <- NA_integer_
        date_from_val <- as.Date(sprintf("%04d-%02d-01", input$ndvi_from_year, input$ndvi_from_month))
        next_month <- as.Date(sprintf("%04d-%02d-01", input$ndvi_to_year, input$ndvi_to_month)) + months(1)
        date_to_val <- next_month - 1
      }
      
    } else if (input$selected_dataset == "Weather") {
      # Weather: single date or period
      if (is.null(input$weather_mode) || input$weather_mode == "single") {
        date_from_val <- as.Date(input$weather_date)
        date_to_val   <- as.Date(input$weather_date)
      } else {
        date_from_val <- as.Date(input$weather_period[1])
        date_to_val   <- as.Date(input$weather_period[2])
      }
      year_val <- NA_integer_
      
    } else if (input$selected_dataset %in% c("AHN", "Soil map")) {
      # these don't need year/date
      year_val <- NA_integer_
    } else {
      # fallback: if your UI adds other datasets in future
      year_val <- NA_integer_
    }
    
    # determine polygon name (fixed project name or "Own polygon")
    poly_name <- if (!is.null(poly$layer_id) && !is.null(fixed_polys())) {
      fixed_poly <- fixed_polys() %>% filter(layer_id == poly$layer_id)
      if (nrow(fixed_poly) > 0) input$fixed_layer else "Own polygon"
    } else {
      "Own polygon"
    }
    
    # Only allow one study area in the overview
    ov <- overview()
    if (nrow(ov) > 0) {
      if (!identical(ov$wkt[1], poly$wkt[1])) {
        showModal(modalDialog(
          title = "Only one study area allowed",
          paste0(
            "The overview already contains datasets for a different polygon.\n\n",
            "You can add multiple datasets but only for a single polygon at a time.\n\n",
            "To add datasets for this new polygon, please first clear the overview."
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
    }
    
    # duplicate check: treat NA comparisons carefully
    duplicate_check <- ov %>%
      filter(
        dataset == input$selected_dataset,
        (is.na(year) & is.na(year_val) | year == year_val),
        polygon == poly_name,
        (is.na(date_from) & is.na(date_from_val) | date_from == date_from_val),
        (is.na(date_to) & is.na(date_to_val) | date_to == date_to_val)
      )
    
    if (nrow(duplicate_check) == 0) {
      overview(bind_rows(
        ov,
        tibble::tibble(
          dataset = input$selected_dataset,
          year = year_val,
          polygon = poly_name,
          wkt = poly$wkt[1],
          polygon_sf = list(poly),
          date_from = date_from_val,
          date_to = date_to_val
        )
      ))
    } else {
      showNotification("This dataset + polygon + year/date is already in the overview.", type = "message")
    }
  })
  
  
  # Overview table
  output$overview_table <- renderUI({
    ov <- overview()
    if (nrow(ov) == 0)
      return(p("No datasets added yet.", style = "color: grey; font-style: italic;"))
    
    tags$table(
      class = "table table-striped table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Dataset"),
          tags$th("Year"),
          tags$th("Polygon"),
          tags$th("Delete")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(ov)), function(i) {
          row <- ov[i, ]
          tags$tr(
            tags$td(row$dataset),
            tags$td(ifelse(is.na(row$year), "", row$year)),
            tags$td(row$polygon),
            tags$td(
              # use Shiny.setInputValue to send a single delete event with the row index
              actionButton(
                paste0("delete_row_", i),
                "x",
                onclick = sprintf(
                  "Shiny.setInputValue('delete_row', %d, {priority: 'event'})",
                  i
                ),
                style = "color:white;background-color:#CD5C5C;padding:2px 5px;font-size:80%;"
              )
            )
          )
        })
      )
    )
  })
  
  
  # Delete buttons
  observeEvent(input$delete_row, {
    idx <- as.integer(input$delete_row)
    if (is.na(idx)) return(NULL)
    
    cur <- overview()
    if (idx >= 1 && idx <= nrow(cur)) {
      overview(cur[-idx, , drop = FALSE])
      download_msgs(character(0))
    }
  }, ignoreInit = TRUE)
  
  
  # Clear overview
  observeEvent(input$clear_overview, {
    overview(overview()[0, ])
    download_msgs(character(0))
  })
  
  # Retrieve & save
  retrieve_and_save <- function(zipfile = NULL, workdir = NULL) {
    ov <- overview()
    req(nrow(ov) > 0)
    
    if (is.null(workdir)) workdir <- file.path(tempdir(), "export")
    if (dir.exists(workdir)) unlink(workdir, recursive = TRUE)
    dir.create(workdir, recursive = TRUE)
    
    download_msgs(character(0))
    local_msgs <- character(0)
    results <- list()
    
    withProgress(message = "Retrieving datasets...", value = 0, {
      for (i in seq_len(nrow(ov))) {
        ds <- ov$dataset[i]
        year <- ov$year[i]
        outfile <- file.path(workdir, paste0(ds, "_", i, ".geojson"))
        
        tryCatch({
          poly_sf <- ov$polygon_sf[[i]]
          mypolygon <- poly_sf$wkt[1]
          
          # --- NDVI ---
          # --- NDVI ---
          if (ds == "NDVI") {
            if (input$ndvi_mode == "single") {
              year  <- as.integer(input$ndvi_year)
              month <- as.integer(input$ndvi_month)
              r <- download_avg_ndvi_month(poly_sf, year, month)
              
              if (!is.null(r)) {
                outfile <- file.path(workdir, paste0("NDVI_", year, "_", sprintf("%02d", month), ".tif"))
                terra::writeRaster(r, outfile, overwrite = TRUE)
                results[[paste0("NDVI_", i)]] <- r
                local_msgs <- c(local_msgs, paste0("Retrieved: NDVI ", year, "-", month))
              } else {
                local_msgs <- c(local_msgs, paste0("Failed: NDVI ", year, "-", month))
              }
              
            } else {  # NDVI range
              start_date <- ov$date_from[i]
              end_date   <- ov$date_to[i]
              
              if (is.na(start_date) || is.na(end_date)) {
                local_msgs <- c(local_msgs, paste0("Failed: NDVI - invalid date range for row ", i))
                incProgress(1 / nrow(ov))
              } else {
                start_year  <- as.integer(format(start_date, "%Y"))
                start_month <- as.integer(format(start_date, "%m"))
                end_year    <- as.integer(format(end_date, "%Y"))
                end_month   <- as.integer(format(end_date, "%m"))
                
                r_stack <- tryCatch({
                  download_avg_ndvi_stack(
                    poly = poly_sf,
                    start_year = start_year,
                    start_month = start_month,
                    end_year = end_year,
                    end_month = end_month
                  )
                }, error = function(e) {
                  message("Error in download_avg_ndvi_stack(): ", e$message)
                  NULL
                })
                
                if (!is.null(r_stack) && terra::nlyr(r_stack) > 0) {
                  # optionally name layers
                  layer_dates <- seq(from = start_date, to = end_date, by = "month")
                  names(r_stack) <- paste0("NDVI_", format(layer_dates, "%Y%m"))
                  
                  outfile <- file.path(workdir, paste0("NDVI_", format(start_date, "%Y%m"), "_to_", format(end_date, "%Y%m"), ".tif"))
                  terra::writeRaster(r_stack, outfile, overwrite = TRUE)
                  results[[paste0("NDVI_", i)]] <- r_stack
                  local_msgs <- c(local_msgs, paste0("Retrieved: NDVI stack ", format(start_date, "%Y-%m"), " to ", format(end_date, "%Y-%m")))
                } else {
                  local_msgs <- c(local_msgs, paste0("Failed: NDVI - no data in range ", start_date, " to ", end_date))
                }
                incProgress(1 / nrow(ov))
              }
            }  # end NDVI range else
            next  # move to next dataset
          }  # end ds == "NDVI"
          
          
          # --- Weather ---
          if (ds == "Weather") {
            cen_res <- get_closest_meteostation(mypolygon, token = mytoken)
            closest_id <- cen_res$closest_id
            if (is.null(closest_id)) {
              local_msgs <- c(local_msgs, paste0("Failed: Weather - no nearby station found"))
              incProgress(1 / nrow(ov))
              next
            }
            df <- ov$date_from[i]
            dt <- ov$date_to[i]
            if (is.na(df) || is.na(dt)) {
              local_msgs <- c(local_msgs, paste0("Failed: Weather - date_from or date_to is NA"))
              incProgress(1 / nrow(ov))
              next
            }
            meteo_sf <- if (df == dt) {
              get_meteo_for_date(closest_id, df, mytoken)
            } else {
              get_meteo_for_long_period(
                meteostation = closest_id,
                fromdate = df,
                todate = dt,
                token = mytoken,
                by_days = 200,
                sleep_sec = 0.5
              )
            }
            if (is.null(meteo_sf) || nrow(meteo_sf) == 0) {
              local_msgs <- c(local_msgs, "Failed: Weather - no data returned")
              incProgress(1 / nrow(ov))
              next
            }
            st_write(meteo_sf, outfile, delete_dsn = TRUE)
            results[[paste0(ds, "_", i)]] <- meteo_sf
            local_msgs <- c(local_msgs, paste0("Retrieved: ", ds))
            incProgress(1 / nrow(ov))
            next
          }  # end ds == "Weather"
          
          # --- Other datasets (GeoJSON) ---
          myres <- content(VERB("GET", url = myurl, add_headers(myheaders)))
          myres_sf <- geojson_sf(toJSON(myres, auto_unbox = TRUE))
          names(myres_sf) <- gsub("[:/\\?<>\\|*\"\\\\]", "_", names(myres_sf))
          st_write(myres_sf, outfile, delete_dsn = TRUE)
          results[[paste0(ds, "_", i)]] <- myres_sf
          local_msgs <- c(local_msgs, paste0("Retrieved: ", ds))
          
        }, error = function(e) {
          local_msgs <- c(local_msgs, paste0("Failed: ", ds, " - ", e$message))
        })
        
        incProgress(1 / nrow(ov))
      }  # end for loop
      
    })
    
    if (!is.null(zipfile)) {
      zip::zipr(zipfile, list.files(workdir, full.names = TRUE))
      local_msgs <- c(local_msgs, paste0("Zip written to: ", zipfile))
    }
    
    download_msgs(c(download_msgs(), local_msgs))
    
    list(
      overview = ov,
      messages = download_msgs(),
      zipfile = zipfile,
      datasets = results
    )
  }
  
  output$download_ui <- renderUI({
    if (nrow(overview()) == 0) {
      div(style = "color: grey; font-style: italic;",
          "Download button will appear here once you add a dataset.")
    } else {
      div(style = "display:flex; gap:10px;",
          downloadButton("download_data", "Download dataset(s)", class = "btn-custom"),
          actionButton("return_to_r", "Return data to R (close app)", class = "btn-custom"))
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("naturedatacube_", Sys.Date(), ".zip"),
    content = function(zipfile) retrieve_and_save(zipfile)
  )
  
  output$download_messages <- renderUI({
    msgs <- download_msgs()
    if (length(msgs) == 0)
      p("Updates on dataset retrieval will appear here.", style = "color: grey; font-style: italic;")
    else
      HTML(paste(msgs, collapse = "<br>"))
  })
  
  observeEvent(input$return_to_r, {
    res <- retrieve_and_save()
    stopApp(res)
  })
  
}


# ============================================================
# Run app
# ============================================================

shinyApp(ui, server)
