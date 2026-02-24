# NatureDataCube Shiny interface (demo version for the NAEM)

This repository contains the R code for a Shiny interface of the NatureDataCube.
The idea of the NatureDataCube is that it offers an accessible way for researchers/ecologists to retrieve relevant data. 
It is a demo version and it will be used for the NAEM.
The Shiny interface was made as an example of what the interface
of the NatureDataCube could look like and to gather feedback from researchers.



This interface is built upon the work from Minke Mulder (NIOO-KNAW) in October - December 2025.

### Opening the Shiny app
To use the Shiny app and continue working with the retrieved data in R, the app must be launched in a specific way so that the output is stored in an R object.
Steps: 
- Navigate to the Shiny app [directory](https://github.com/LTER-LIFE/NatureDataCube-Shiny-NAEM/tree/main/R/naturedatacube_app)
- Open app.R in RStudio (or another R environment).
- Install and load all required R packages.
- Set your working directory to the root folder of the repository: setwd("path/to/NatureDataCube-Shiny-NAEM")
- Launch the Shiny app from the R console: data_nc <- runApp("R/naturedatacube_app/app.R")

Launching the app in this way ensures that the output generated through the Shiny interface is returned and stored in the R variable data_nc.
This allows you to continue working with the retrieved data in R after closing the app. To retrieve data from the Nature Data Cube, an API token is required. 
Make sure your token is available in your R session before requesting data. 

### Generate an API token

To generate your free personal API token to retrieve data you can go to [Register API tokens](https://agrodatacube.wur.nl/api/register.jsp).


### Description

The user can open a Shiny interface which lets them select an area (management area, research site or they cand draw their own area of interest)
to retrieve data for. Below is a description of all the files that are in this repository.

#### Shiny app

Folder: [R/naturedatacube_app](https://github.com/LTER-LIFE/NatureDataCube-Shiny-NAEM/tree/main/R/naturedatacube_app)

This folder contains the R code for the Shiny interface. 

Folder: [retrieval_functions](https://github.com/LTER-LIFE/NatureDataCube-Shiny-NAEM/tree/main/R/retrieval_functions)
taken from [NatureDataCube-R](https://github.com/LTER-LIFE/NatureDataCube-R)

This folder contains the retrieval code for constructing the urls.

#### Data

Folder: [data](https://github.com/LTER-LIFE/NatureDataCube-Shiny-NAEM/tree/main/data)

This folder contains the data of the available study areas. 


