# NatureDataCube Shiny interface (demo version)

This repository contains the R code for a Shiny interface of the NatureDataCube.
The idea of the NatureDataCube is that it offers an accessible way for researchers/ecologists to retrieve relevant data. 
It is a demo version and it was shown in December during three presentations.
Since it is still a demo version it can only retrieve data from KNMI, the other options
are there as examples. The Shiny interface was made as an example of what the interface
of the NatureDataCube could look like and to gather feedback from researchers.

For the LTER-LIFE team there is additional information on Teams in WP4 - datacube.

This interface was made by Minke Mulder (NIOO-KNAW) in October - December 2025.

### Opening the Shiny app

To open and use this code, go to [R/naturedatacube_app](https://github.com/LTER-LIFE/NatureDataCube/tree/main/R/naturedatacube_app) 
and open `app.R`. Click "Run app" in the top right (or Ctrl+Shift+Enter). And if you would like to retrieve data from KNMI you
have to fill in the API tokens as well.

### Generate API tokens for KNMI

To generate your free personal API tokens to retrieve KNMI data you can go to [Register API tokens](https://developer.dataplatform.knmi.nl/apis).
The Open Data API can be filled in for the "api_key" in the `app.R` and the EDR API key as "edr_api_key".
The Open Data API key is for in situ data from weather stations and the EDR API key is for grid/raster data.

### Description

The user can open a Shiny interface which lets them select an area (management area, research site or they can upload their own shapefile)
to retrieve data for. Below is a description of all the files that are in this repository.

#### Shiny app

Folder: [R/naturedatacube_app](https://github.com/LTER-LIFE/NatureDataCube/tree/main/R/naturedatacube_app)

This folder contains the R code for the Shiny interface.

Files:

- `app.R`\
  The code for the app itself. Open the R file, fill in the KNMI API keys if you want to retrieve KNMI data and click
  "Run app" in the top right (or Ctrl+Shift+Enter).

#### Data retrieval functions

Folder: [R/retrieval_functions/KNMI](https://github.com/LTER-LIFE/NatureDataCube/tree/main/R/retrieval_functions/KNMI)

This folder contains retrieval functions for data from the KNMI.

Files:

- `get_daily_data_knmi.R`\
  retrieves daily in situ data from weather stations in the selected area or the closest station to the selected area based on the centroid *(requires KNMI Open Data API token)* 
- `get_daily_grid_data_knmi.R`\
  retrieves daily grid data for the selected area *(requires KNMI EDR API token)* 
- `get_hourly_data_knmi.R`\
  retrieves hourly in situ data from weather stations in the selected area or the closest station to the selected area based on the centroid *(requires KNMI Open Data API token)*
- `get_stations_knmi.R`\
  retrieves weather station info that is needed for retrieving daily or hourly in situ data *(requires KNMI Open Data API token)*
- `rename_columns_knmi.R`\
  renames the columns of the retrieved KNMI data for increased readability, for the daily and hourly in situ variables (in Dutch) 

#### Data

Folder: [data](https://github.com/LTER-LIFE/NatureDataCube/tree/main/data)

This folder contains the data that is needed for offering the options of the available datasets and the polygons the user can select.

Files: 

- `dataset_info_demo.csv`\
  this CSV file contains mock info (except KNMI info) for the app to create all the options
  for the user. **Note:** this is mock data to make an interface to show in the demo presentations
  and get feedback from researchers. Only the KNMI data can actually be used to retrieve data for the selected area.
- `knmi_variable_names.csv`\
  this CSV file contains names of the KNMI variables used to rename the columns for the daily and hourly in situ data. 
  It contains short names, long names and a description of the variables in Dutch and in English.

Folder: [data/polygons](https://github.com/LTER-LIFE/NatureDataCube/tree/main/data/polygons)

This folder contains the two types of polygons the user can select, which are the management areas and the research sites.

Files:

- `SNL_beheertypen_Veluwe_subset.shp`\
  subset of management areas on the Veluwe (only southwest): [SNL](https://atlas.bij12.nl/WebViewer/index.html?viewer=SNL_applicatie)
- `merged_shapefiles_research_sites.shp`\
  a couple research sites on the Veluwe that have been provided by the researchers themselves
- *(and the other associated files with the same names)*


#### Archive

Folder: [archive/AgroDataCube](https://github.com/LTER-LIFE/NatureDataCube/tree/main/archive/AgroDataCube)

This folder contains a Shiny interface for the AgroDataCube, which was made as a test case before 
working on the interface of the NatureDataCube and therefor is in the "archive" of the NatureDataCube repository and has its own README.
