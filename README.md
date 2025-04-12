
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nfitoosm

Package build to prepare NFI ground plot data into OSM input data using
`data.table`.

To explore what fields exists in NFI ground plot data go to their
[documentation page](https://nfi.nfis.org/en/ground_plot).  
To request NFI data you can go to their [form
webpage](https://nfi.nfis.org/en/datarequestform).  
Details about Open Stand Model (OSM) are found at [FORUS Research
webpage](https://forusresearch.com/downloads/osm/index.html).

# Use

This package contains only one main function to produce the OSM input
that from NFI. Make sure to explore its documentation `?nfi_to_osm()` to
learn about about how you can modify some of the data translation
parameters.

``` nfi

devtools::install_github("vmanvailer/nfittoosm")
library(nfitoosm)
osm_input_data <- nfi_to_osm(nfi_folder,
                             scenario_name = "Acadian_ALL",
                             remeasurement_number = NULL,
                             calculate_stockable = TRUE,
                             provinces = c("New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador"),
                             model_variant = "Acadian",
                             include_small_trees = TRUE,
                             dbh_filter = 1,
                             output_path = tempdir()
)

file.exists(file.path(tempdir(), "nfitoosm_Acadian_ALL.sqlite"))
```

The function creates the two required data tables Stand List Table and
Tree List Table. [OSM Input Data
Details](https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm).

``` table
head(osm_input_data)
```

Contact the developer if any issues found.

This project is currently unlicensed. Please contact for permissions.
