devtools::install_github("vmanvailer/nfittoosm")
library(nfitoosm)
osm_input_data <- nfi_to_osm(nfi_folder,
                             scenario_name = "nfi_simulation",
                             remeasurement_number = NULL,
                             calculate_stockable = TRUE,
                             provinces = c("New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador"),
                             model_variant = "Acadian",
                             include_small_trees = TRUE,
                             dbh_filter = 0,
                             use_projected_height = TRUE,
                             calculate_ef_for_dbh_bin = TRUE,
                             ef_dbh_increment_bin = 4,
                             output_path = tempdir()
)

file.exists(file.path(tempdir(), "nfi_to_osm_input_data.sqlite"))

osm_input_data