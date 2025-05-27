# devtools::install_github("vmanvailer/nfitoosm")
library(nfitoosm)
osm_input_data <- nfi_to_osm(nfi_folder = "development/nfi_data",
                             scenario_name = "Acadian_ALL",
                             remeasurement_number = NULL,
                             calculate_stockable = TRUE,
                             provinces = c("New Brunswick", "Prince Edward Island", "Nova Scotia"),
                             model_variant = "Acadian",
                             only_remeasured_trees = TRUE,
                             include_small_trees = FALSE,
                             dbh_filter = 1,
                             # output_path = tempdir()
                             output_path = file.path(getwd(), "development", "to_chris", "2025-05-27_iteration3")
)

file.exists(file.path(tempdir(), "nfi_to_osm_input_data.sqlite"))

stand_list <- tidyr::separate(osm_input_data$OSM_StandList, col = "SurveyID", into = c("nfi_plot", "loc_id", "meas_num"), sep = "L|M", remove = FALSE)
setDT(stand_list)
dcast(stand_list, nfi_plot + loc_id ~ meas_num, value.var = "SurveyAge") |> View()
stand_list$nfi_plot <- gsub("ID", "", stand_list$nfi_plot)
readr::write_csv(stand_list, "tests/all_acadian_stand_list.csv")

# tree table
tree_list <- tidyr::separate(osm_input_data$OSM_TreeList, col = "SurveyID", into = c("nfi_plot", "loc_id", "meas_num"), sep = "L|M", remove = FALSE)
setDT(tree_list)
dcast(tree_list, nfi_plot + loc_id ~ meas_num, value.var = "Born") |> View()




nfi_photo_plot <- fread("development/from_ben/pp_nfi_plot_list_nb.csv")
nfi_intersect <- intersect(as.character(nfi_photo_plot$nfi_plot), stand_list$nfi_plot)
nfi_diff <- setdiff(stand_list$nfi_plot, as.character(nfi_photo_plot$nfi_plot))
gp_nb_nfi_plot_list <- stand_list[nfi_plot %in% nfi_intersect][,.(nfi_plot, loc_id, meas_num)]

nfi_intersect <- data.table(nfi_plot = nfi_intersect)
nfi_diff <- data.table(nfi_plot = nfi_diff)
fwrite(nfi_intersect, "development/to_ben/nfi_gp_pp_intersect.csv")
fwrite(nfi_diff, "development/to_ben/nfi_gp_pp_diff.csv")
