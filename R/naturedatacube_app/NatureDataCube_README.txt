
Nature Data Cube — Quick Start Guide
===================================

Overview
--------
This app helps you define an area of interest and request raster or statistics datasets (e.g. Weather, NDVI, Soil) for that area.
You can select an existing project area, draw your own polygon on the map, or upload polygon files (GeoPackage, Shapefile, GeoJSON, KML).

1) Define area of interest (left sidebar)
   - Click a project under "Projects" to show that project's polygons on the map.
   - Draw your own polygon using the draw toolbar on the map (use the polygon tool).
   - Upload polygon files using the Upload panel. Supported formats: .zip (containing shapefile or gpkg/geojson), .gpkg, .geojson, .kml, .shp.
   - Click polygons on the map to select / deselect them. Selected polygons are highlighted blue and will be used for dataset queries.

2) Choosing a dataset (left sidebar)
   - Use the "Choose dataset for the selected area" menu to pick a dataset (e.g. Weather, NDVI).
   - Click the small (i) icon next to a dataset to see dataset metadata/description (information modal).
   - After choosing a dataset, pick the finer controls shown (date, year, month, or sub-layers) in the right-hand panel that appears under "Dataset metadata and selection".

3) Adding datasets to the overview
   - After selecting polygons and choosing dataset settings, click "Add to overview".
   - Each added combination appears in the "Overview of selected datasets" table, showing dataset, type, date, polygon name, and layers.
   - The app avoids exact duplicates (same polygon + same dataset + same date/settings).

4) Downloading data
   - When you've added at least one dataset to the overview, the "Download dataset(s)" button will appear.
   - Click "Download dataset(s)" to start retrieving datasets and packaging them into a ZIP file. Progress messages appear in the "Updates on dataset retrieval" area.
   - The "Return data to R (close app)" button will run the retrieval and then exit the app returning the results to the calling R session (useful when launching app from RStudio).

5) Uploads & GeoPackage layer handling
   - If a GeoPackage contains multiple layers it will be listed in the Upload panel and you can select which layer(s) to import.
   - Imported layers receive unique source names and are shown on the map. You can remove uploaded layers via the Upload panel table.

6) Polygons & names
   - Polygons shown on the map have human-readable labels (hover/label) and unique source names (e.g. 'Own polygon', 'Nestboxes', or 'my_upload.gpkg_2').
   - The Overview uses the same polygon names as the map hover labels for a 1:1 match.

7) Tips & troubleshooting
   - If the "Statistics" tab is disabled, it means you have active drawn or uploaded polygons; switch to Raster, or clear uploaded/drawn polygons to enable Statistics.
   - If an uploaded GeoPackage does not appear, check that it isn't corrupted and that it contains polygon layers.
   - For shapefiles inside a ZIP: ensure all shapefile components (.shp, .shx, .dbf, .prj) are present in the ZIP.
   - Coordinate reference systems: the app transforms polygons to EPSG:4326 (WGS84) where possible.
   - If dataset retrieval fails, check the messages area for details shown after retrieval attempts.

8) Contact / development notes
   - This app is a demo interface — the actual data retrieval is driven by backend retrieval functions (ndc_get, gm_get, etc.). Make sure these helper scripts are present in the R/retrieval_functions/ folder for full functionality.
   - If you want this guide to be included as a static file in the app bundle, save this README as 'NatureDataCube_README.txt' in the app directory or a `www/` folder so it can be served directly.
   - For help or to report bugs, add details about your input files and application logs.

Enjoy exploring — and thanks for using the Nature Data Cube app!
