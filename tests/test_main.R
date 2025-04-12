# devtools::install_github("vmanvailer/nfitoosm")
# library(nfitoosm)
osm_input_data <- nfi_to_osm(nfi_folder = "development/nfi_data",
                             scenario_name = "nfi_simulation",
                             remeasurement_number = NULL,
                             calculate_stockable = TRUE,
                             provinces = c("Newfoundland and Labrador"),
                             model_variant = "Newfoundland and Labrador",
                             include_small_trees = TRUE,
                             dbh_filter = 1,
                             output_path = tempdir()
)

file.exists(file.path(tempdir(), "nfi_to_osm_input_data.sqlite"))

osm_input_data
