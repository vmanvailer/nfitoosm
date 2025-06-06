# NFI to OSM data
#' 
#' NFI to OSM data
#' 
#' This function extracts and properly formats Canada's National Forest Inventory (NFI) data to Open Stand Mode (OSM) input format.
#' It attempts to maximize the use of data but there are still some improvements that can be made to use the full extent of NFI data. Check 'Value' section for more details.
#' 
#' It uses the current NFI data structure in csv files and is based on file naming patterns for ground plot data e.g. "all_gp_trees".
#' It maximizes the use of data by making several assumptions on which data should be included and how. Several of those assumptions can be changed by the user by modifying the function parameters e.g. calculate_ef_for_dbh_bin = TRUE.
#' For full details on how each OSM input variable is constructed and which assumptions are made consult the details.
#' 
#' @param nfi_folder String. The path to the folder containing the NFI data. 
#' @param scenario_name String. The name of the OSM growth scenario to be populated into the 'Scenario' column of Stand List Table.
#' @param remeasurement_number Integer. NFI numbers to individualize each field campaign. Starts at '0' (plot establishment) and the following numbers in the sequence are remeasurements.
#' @param calculate_stockable Logical. Refer to the optional OSM column "Stockable" defaults to `TRUE`.
#' OSM uses this information to adjust calculation when projecting a stand growth. Use if it is known that there are portions of the site that are portions of the plot that do not support tree growth (e.g. roads or other). 
#' 
#' On NFI, this information may be contained in two variables: `meas_plot_size` and `nom_plot_size`. The actual measured area (in ha) of plots (meas_plot_size) may be different than the designed size of the plot (nom_plot_size). nominal plot size is standardized to 400 square meters (or 0.04 ha).
#' This may happen because sites are assigned to the center of randomly established photo plots and may fall partially on non-forested areas e.g. road, or other non-productive areas. 
#' 
#' However, it is important to note that there may be other reasons for not measuring the entire plot (fallen tree, inaccessible private property or other safety reason).
#' 
#' If `TRUE` will calculate ratio of measured size (ha) to nominal size (ha) and assume unmeasured portions are unproductive, otherwise will use nominal size (ha) and assume the entire plot is productive.  
#' Also, if using `meas_num = 0` there is a higher chance fully productive plots being incompletely measured (due to cost issues). See page 12 of \link[https://nfi.nfis.org/resources/groundplot/GP_compilation_procedures_2.4.pdf]{Compilation Procedures}
#' @param fill_survey_age Logical. Fill SurveyAge for t-1 when SurveyAge is available for time t. 
#' 
#' Important: SurveyAge is extracted from site_age variable in NFI table all_gp_ltp_header.csv. site_age variable is calculated as the average age of all trees in the plot and may not align with time lapsed between re measurements.
#' For example, nfi_plot 916316 has site_age of 52 at meas_num == 0 and 59 at meas_num == 1 (7 years of difference) even though they were measured 12 years apart.
#' @param provinces Vector of strings. Provinces for which you want data for. Will only retrieve data for one of the four provinces in the argument default.
#' @param model_variant Vector of string. OSM has two models. The "Acadian" model variant has been calibrated for the provinces of New Brunswick, Prince Edward Island and Nova Scotia. The "Newfoundland and Labrador" model has been calibrated for Newfoundland and Labrador only.
#' @param only_remeasured_trees Logical. Defaults to FALSE to maximize data use. Ignored if querying a single re measurement number. Will include ingrowth trees and remeasured trees. Ingrowth trees are small trees that crossed the threshold of DBH >= 9cm between remeasurements. May cause large increase in biomass.
#' @param include_small_trees Logical. Defaults to true to maximize data use and ignored if only_remeasured_trees = TRUE. Will include all individual trees measured on the small plot data. See page 32 of \link[https://nfi.nfis.org/resources/groundplot/Gp_guidelines_v5.0.pdf]{Ground Plot Guidelines}.
#' @param dbh_filter Numeric. A field to filter out trees based on DBH. Will pick DBH higher than or equal to this value.
#' @param output_path String. Use to output the SQLite database that can be passed to OSM. 
#' @return Returns a list with two data frames (data.tables) and a writes a database on specified output_path. The data frames should match the input data exactly as in \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{Open Stand Model}.
#' Multiple assumptions are made on the creation of each variable and they are described below.
#' \itemize{
#'    \item \strong{STAND LIST TABLE}
#'          \describe{
#'            \item{Scenario}{User specified}
#'            \item{SurveyID}{Same as `nfi_plot` in NFI data.}
#'            \item{SurveyYear}{The year from `meas_date` in \emph{all_gp_site_info.csv} NFI data.}
#'            \item{SurveyAge}{The stand age from `site_age` in \emph{all_gp_ltp_header.csv} NFI data. Uses arithmetic average age of trees deemed suitable for compiling site age. For more details see page 129 of \link[https://nfi.nfis.org/resources/groundplot/Gp_guidelines_v5.0.pdf]{Ground Plot Guidelines}.}
#'            \item{Plots}{Number of plots for a given stand. Defaults to 1 where each NFI plot will assumed to represent a stand.}
#'            \item{Stockable}{Ratio of total sample area (all plots) that supports productive tree growth. Calculated as described in `calculate_stockable`}
#'            \item{X}{NFI plots uses UTM coordinates systems which are converted to Latitude/Longitude system using package \pkg{sf}}
#'            \item{Y}{Same as above.}
#'            }
#'    \item \strong{TREE LIST TABLE}
#'          \describe{
#'            \item{SurveyID}{Same as `nfi_plot` in NFI data.}
#'            \item{TreeID}{Same as `tree_num` in \emph{all_gp_lgtree_plot.csv} and `smtree_num` in \emph{all_gp_smtree_plot.csv}}
#'            \item{Species}{NFI Species are mapped to OSM species based on the `Acadian_SpeciesList` table contained in the \link[https://forusresearch.com/downloads/osm/OSMv2.25.1/OSMv2.25.1_Demo.zip]{OSM demo package}.
#'                           NFI codes are mapped to the `GENUS` and `SPECIES` column of the `Acadian_SpeciesList` and the variable `OSM_AD_CmdKey` from that table is used.}
#'            \item{DBH}{DBH of individual trees or average DBH for each DBH Bin classes when `calculate_ef_for_dbh_bin = TRUE`.}
#'            \item{HT}{Height of individual trees or average height for each DBH Bin classes when `calculate_ef_for_dbh_bin = TRUE`.}
#'            \item{Stems}{Trees per hectare this record represents. i.e., tree expansion factor. Calculated as described in the argument `calculate_ef_for_dbh_bin`.}
#'            \item{ToCut}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{Weight}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{HTK}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}. Could potentially be extracted from `height` of `stem_cond = "B"` while using `height_prj` as OSM `HT`.}
#'            \item{CR}{Crown Ratio. Calculated for both large and small trees when, at minimum, `crown_cond` and `crown_base` data are avaialble but may also use `crow_top` if avaialble. Calculated as `(crown_top-crown_base)/HT` or `(crown_top-HT)/HT`.}
#'            \item{DBHI}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{HTI}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{Origin}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}. May be partially inferred from \emph{plot_origin.csv} variables `veg_origin` and `regen_type`}
#'            \item{Born}{Derived from `age_total` variable of \emph{all_gp_ltp_tree_age.csv}. For more details see psgr 27 of \link[https://nfi.nfis.org/resources/groundplot/GP_compilation_procedures_2.4.pdf]{Compilation proceedures}.}
#'            \item{Died}{Derived from `lgtree_status` (Large trees) or `smtree_status` (Small trees). If classified as DS = Dead Standing then it is assigned 9999 to be initialized in the snag pool. LS (Live Stading), LF (Live fallen) or M (Missing) are assumed to as live (set to 0).
#'                        Setting to 9999 assumes tree died 7 years ago as per OSM default. Could potentially be improved to check whether tree died between remeasurement and set to value to be year of measurement 2 minus year of measurement 1 divided 2.)}
#'            \item{Risk}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{Grade}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            }
#' }
#'   
#' @import data.table
#' @import RSQLite
#' @importFrom tidyr separate
#' @import sf
#' @export
nfi_to_osm <- function(nfi_folder,
                       scenario_name = "nfi_simulation",
                       remeasurement_number = NULL,
                       calculate_stockable = TRUE,
                       fill_age_gaps = TRUE,
                       provinces = c("New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador"),
                       model_variant = c("Acadian", "Newfoundland and Labrador"),
                       only_remeasured_trees = FALSE,
                       include_small_trees = TRUE,
                       dbh_filter = 1,
                       output_path = getwd()
) {
  
  if(!dir.exists(output_path)){
    stop("Specified directory below does not exist\n\n\t", output_path)
  }
  # nfi_folder <- file.path(getwd(), "development", "nfi_data")
  file_list <- list.files(nfi_folder,  "all_gp_.*\\.csv", recursive = TRUE, full.names = TRUE)
  
  # --- Stand Table ------------------------------------------------------------
  # Read site_info
  site_info_path <- grep(x = file_list, pattern =  file.path(nfi_folder, "all_gp_site_info.csv"), value = TRUE)
  site_info <- fread(site_info_path)
  
  ## Scenario ------------------------------------------------------------------
  site_info[, Scenario := scenario_name]
  
  # Select and rename columns for plot table
  stand_table <- site_info[, .(
    Scenario = Scenario,
    nfi_plot = nfi_plot,
    loc_id = loc_id, # filtering column
    meas_num = meas_num,# Filtering column
    meas_date = meas_date, # intermediate variable to calculate YEAR (SurveyYear)
    utm_n = utm_n, # intermediate variable to calculate LAT
    utm_e = utm_e, # intermediate variable to calculate LONG
    utm_zone = utm_zone, # intermediate variable to calculate LAT/LONG
    province = province
  )]
  
  # Filter to a given field campaign
  if(is.null(remeasurement_number)){
    message("No remeasurement number chosen. Will include all remeasurements. \n\nSurveyID will use the name pattern ID {nfi_plot} L {loc_id} M {meas_num}\n nfi_plot = NFI plot id\n loc_id = Location ID (used when plot change locations)\n meas_num = Remeasurement number\n\n")
  } else {
    stand_table <- stand_table[meas_num == remeasurement_number]
    if(nrow(stand_table) == 0){
      stop(paste0("There is no data for remeasurement number ", remeasurement_number, "\n\n"))
    }
  }
  
  # Location change (loc_id) may not impact model setup.
  # stand_table[loc_id == 0,]
  
  ## Survey Year ---------------------------------------------------------------
  # Create year variable and remove intermediate variables used to calculate LAT and LONG
  stand_table[, SurveyYear := tidyr::separate(.SD, col = meas_date, into = "meas_yr", sep = "-", extra = "drop"), .SDcols = "meas_date"]
  stand_table[, SurveyYear := as.integer(SurveyYear)]
  
  ## Survey Age ----------------------------------------------------------------
  # Read ltp_tree_header
  ltp_header_path <- grep(x = file_list, pattern =  "ltp_header.csv", value = TRUE)
  ltp_header <- fread(ltp_header_path)
  
  stand_table <- merge(stand_table, ltp_header[,.(
    nfi_plot, loc_id, meas_date, meas_num, # Id fields
    site_age,                              # Age field
    meas_plot_size, 
    nom_plot_size # size field (For "Stockable" variable)
    )], by = c("nfi_plot", "loc_id", "meas_date", "meas_num"), all.x = TRUE)
  
  data.table::setnames(stand_table, old = "site_age", new = "SurveyAge")
  
  
  if(fill_age_gaps){
  message("fill_age_gaps = TRUE | filling SurveyAge for time 't-1' where SurveyAge for time 't' is available\n\n")  
    # Filling SurveyAge gaps
    # Order data to make logic easier
    setorder(stand_table, nfi_plot, loc_id, meas_num)
    
    # Step 2: Fill lower SurveyAges when higher ones exist
    stand_table <- stand_table[, SurveyAge := {
      # check which entries are missing SurveyAge
      filled <- SurveyAge
      
      if (any(is.na(SurveyAge)) && any(!is.na(SurveyYear))) {
        # Get last known SurveyAge and year (from the max meas_num)
        known_idx <- which(!is.na(SurveyAge))
        last_idx <- suppressWarnings(max(known_idx))
        
        # Fill earlier NAs only if they come before known value
        if (last_idx > 1) {
          for (i in seq_len(last_idx - 1)) {
            if (is.na(filled[i])) {
              years_diff <- SurveyYear[last_idx] - SurveyYear[i]
              filled[i] <- SurveyAge[last_idx] - years_diff
            }
          }
        }
      }
      filled
    }, by = .(nfi_plot, loc_id)]
  }
  
  
  ## Plots ---------------------------------------------------------------------
  stand_table[,Plots := 1]
  
  ## Stockable -----------------------------------------------------------------
  # If measured is chosen 
  if(calculate_stockable){
    message("Calculating 'Stockable' as measured plot size / nominal plot size (where both data exists).\n\n")
    } else {
    message("Setting 'Stockable' to 1.\n\n")
    }
  
  stand_table[, Stockable := fifelse(calculate_stockable & !is.na(meas_plot_size) & !is.na(nom_plot_size) & nom_plot_size > 0, 
                                     meas_plot_size/nom_plot_size,
                                     1)
  ]
  
  ## Convert UTM to Lat Long ---------------------------------------------------
  # First, create a function to convert UTM to lat/lon
  utm_to_longlat <- function(easting, northing, zone) {
    # Create an SF object
    points <- data.frame(x = easting, y = northing)
    sp <- st_as_sf(points, coords = c("x", "y"), crs = paste0("+proj=utm +zone=", zone, " +datum=WGS84"))
    
    # Transform to WGS84 (latitude/longitude)
    longlat <- st_transform(sp, crs = "+proj=longlat +datum=WGS84")
    
    # Extract coordinates
    coords <- st_coordinates(longlat)
    return(data.frame(lat = coords[, "Y"], lon = coords[, "X"]))
  }
  
  # Apply the UTM to lat/lon conversion *by zone*
  stand_table[, `:=`(Y = NA_real_, X = NA_real_)] # Initialize columns
  zones <- unique(stand_table$utm_zone) # Get unique UTM zones
  
  for (zone in zones) {
    rows <- stand_table$utm_zone == zone
    coords <- utm_to_longlat(stand_table[rows, utm_e], stand_table[rows, utm_n], zone)
    stand_table[rows, `:=`(Y = coords$lat, X = coords$lon)]
  }
  
  # --- Model variant ----------------------------------------------------------
  error_model_variant = "Please choose either 'Acadian' or 'Newfoundland and Labrador' as a model_variant."
  if(length(model_variant) != 1){
    stop(error_model_variant)
  }
  
  pt <- setNames(
    c("New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador"),
    c("NB", "PE", "NS", "NL")
  )
  wrong_pt <- setdiff(provinces, pt)
  if(length(wrong_pt) > 0){
    stop(paste0("The following provinces are not part allowed or are mispelled:\n\t", paste(wrong_pt, collapse = "\n\t")))
  }
  
  pt_filter <- pt[pt %in% provinces] |> names()
  stand_table <- stand_table[province %in% pt_filter,]
  
  if(model_variant == "Acadian"){
    not_allowed_pt <- setdiff(provinces, c("New Brunswick", "Prince Edward Island", "Nova Scotia"))
    if(length(not_allowed_pt) > 0){
      message(paste0("Warning | '", not_allowed_pt, "' is not calibrated for the Acadian model.\n"))
    }
    
    if(nrow(stand_table) == 0){
      stop(paste0("There is no data for remeasurement number ", remeasurement_number, "for any of:\n", paste0(paste0("\t", provinces), collapse = "\n")))
    }
    
    # Zone    
    stand_table[, Zone := province]
    
    column_order <- c("Scenario", "nfi_plot", "loc_id", "meas_num", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Zone", "Management", "BGI")
    
  } else if(model_variant == "Newfoundland and Labrador"){
    
    if(!"NL" %in% pt_filter){
      message("Warning | You removed the Newfoundland and Labrador from the 'provinces' argument but specified model_variant = 'Newfoundland and Labrador'.\nPerhaps you meant to use 'model_variannt = Acadian'?\n\nModel will default the 'District' column to 4 'NL'.\n")
      # District will default to 4 as per:
      # https://forusresearch.com/downloads/osm/help/OSM.Variants/OSM.Variants.NL/OSM.Variants.NL.HelpFiles/OSM.Variants.NL.InputTables.htm
    }
    column_order <- c("Scenario", "nfi_plot", "loc_id", "meas_num", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Management")
    
  } else {
    
    stop(error_model_variant)
    
  }
  
  # --- Management (Extra column) ----------------------------------------------
  treatment_path <- grep(x = file_list, pattern =  "all_gp_treatment.csv", value = TRUE)
  treatment <- fread(treatment_path, select = c("nfi_plot", "loc_id", "meas_date", "meas_num", "treat_type", "treat_yr"))
  
  stand_table <- merge(stand_table, treatment, all.x = TRUE)
  stand_table[, meas_yr := as.integer(substr(meas_date, 1, 4))]
  stand_table[, treat_last25_yt := fifelse(!is.na(meas_yr) &  meas_yr > 0 & !is.na(treat_yr) & treat_yr > 0, meas_yr-treat_yr <= 25, FALSE)]
  stand_table[, Management :=  fifelse(treat_type == "PC" & treat_last25_yt, "PartialCut",
                                      fifelse(treat_type == "CC" & treat_last25_yt, "Clearcut",
                                              fifelse(treat_type == "PT" & treat_last25_yt, "PCT", "None")))]
  
  
  # --- BGI --------------------------------------------------------------------
  if (model_variant == "Acadian"){
  stand_table <- merge(stand_table, bgi_values, by = c("nfi_plot", "loc_id", "meas_date", "meas_num", "province"), all.x = TRUE)
  }
  stand_table <- stand_table[,..column_order]
  
  # --- Tree Table -------------------------------------------------------------
  ## --- Large Tree Table ------------------------------------------------------
  # Read ltp_tree 
  ltp_tree_path <- grep(x = file_list, pattern =  "ltp_tree.csv", value = TRUE)
  ltp_tree <- fread(ltp_tree_path)
  
  # Merge the two tables
  large_tree_table <- merge(ltp_tree, ltp_header, by = c("nfi_plot", "loc_id", "meas_date", "meas_num"), all.x = TRUE)
  
  if(!is.null(remeasurement_number)){
    large_tree_table <- large_tree_table[meas_num == remeasurement_number]
  }
  
  # Filter only the nfi plots to be used.
  large_tree_table <- large_tree_table[nfi_plot %in% unique(stand_table$nfi_plot)]
  
  
  # Location change (loc_id) may not impact model setup.
  # large_tree_table[loc_id == 0,]
  
  # Select and rename columns for tree table
  large_tree_table <- large_tree_table[, .(
    nfi_plot = nfi_plot,
    loc_id = loc_id,
    meas_num = meas_num,
    meas_date = meas_date,
    meas_plot_size = meas_plot_size,
    nom_plot_size = nom_plot_size,
    tree_num = tree_num,
    tree_genus = lgtree_genus,
    tree_species = lgtree_species, 
    DBH = dbh,
    stem_cond = stem_cond,
    HT = fifelse(stem_cond != "B" & height > 0, height, fifelse(stem_cond != "B" & height < 0 & !is.na(height_prj), height_prj, NA)),
    HTK = fifelse(stem_cond == "B" & !is.na(height_prj) & height_prj > 0, height_prj, NA),
    crown_cond = crown_cond,
    crown_base = crown_base,
    crown_top = crown_top,
    tree_status = fifelse(lgtree_status == "DS", "Dead", "Live"),
    tree_size = "Large"
  )]
  
  if(dbh_filter > 0) message("Excluding trees with DBH < ", dbh_filter, "cm\n\n")
  large_tree_table <- large_tree_table[DBH >= dbh_filter]
  
  # If more than one remeasurement period was select and user does not want to include small trees, 
  # I assume user only wants trees that have been
  if((is.null(remeasurement_number) | length(remeasurement_number) > 1) & only_remeasured_trees){
    message("Excluding any large trees that have not been remeasured (at least once).")
    # Ensure data.table and ordered by measurement
    setorder(large_tree_table, nfi_plot, loc_id, tree_num, meas_num)
    
    # Flag whether this tree appears in a *future* meas_num
    # This includes all multiple measurements except the last one. Not usefull when only two measurements are available.
    # tree_table[, is_remeasured := .N > 1, by = .(nfi_plot, loc_id, tree_num)]    
    large_tree_table[, is_remeasured := .N > 1, by = .(nfi_plot, loc_id, tree_num)]    
    large_tree_table <- large_tree_table[is_remeasured == TRUE]
  }
  
  tree_table <- map_tree_species(tree_table = large_tree_table)
  
  ## --- Small Tree Table ------------------------------------------------------
  if(only_remeasured_trees){
    
    message("only_remeasured_trees = TRUE. Skipping small tree processing")
    
  } else if(include_small_trees) {
    
      message("Including small trees.\n\n")
      
      # Read stp_tree and stp_header
      stp_header_path <- grep(x = file_list, pattern =  "stp_header.csv", value = TRUE)
      stp_tree_path <- grep(x = file_list, pattern =  "stp_tree.csv", value = TRUE)
      
      stp_header <- fread(stp_header_path)
      stp_tree <- fread(stp_tree_path)
      
      # Merge the two tables
      small_tree_table <- merge(stp_tree, stp_header, by = c("nfi_plot", "loc_id", "meas_date", "meas_num"), all.x = TRUE)
      
      # Filter for selected measurement number
      if(!is.null(remeasurement_number)){
        small_tree_table <- small_tree_table[meas_num == remeasurement_number]
      }
      
      # Filter only the nfi plots to be used.
      small_tree_table <- small_tree_table[nfi_plot %in% unique(stand_table$nfi_plot)]
      
      # Find max tree_num per plot and meas_num in large trees
      max_large_ids <- tree_table[, .(max_large_id = max(tree_num, na.rm = TRUE)),
                                  by = .(nfi_plot, meas_num)]
      
      # Join with small tree table
      small_tree_table <- merge(small_tree_table, max_large_ids, by = c("nfi_plot", "meas_num"), all.x = TRUE)
      
      # Replace NAs (if no large trees in plot/meas_num) with 0
      small_tree_table[is.na(max_large_id), max_large_id := 0]
      
      # Renumber small tree IDs to continue from large tree IDs
      small_tree_table[, tree_num := max_large_id + .I, by = .(nfi_plot, meas_num)]
      
      # Drop helper column
      small_tree_table[, max_large_id := NULL]
      
      # Select and rename columns for small tree table
      small_tree_table <- small_tree_table[, .(
        nfi_plot = nfi_plot,
        loc_id = loc_id,
        meas_num = meas_num,
        meas_date = meas_date,
        meas_plot_size = meas_plot_size,
        nom_plot_size = nom_plot_size,
        tree_num = tree_num,
        tree_genus = smtree_genus,
        tree_species = smtree_species,
        DBH = smtree_dbh,
        HT = fifelse(stem_cond != "B" & smtree_ht > 0, smtree_ht, fifelse(stem_cond != "B" & smtree_ht < 0 & !is.na(smtree_ht_prj), smtree_ht_prj, NA)),
        HTK = fifelse(stem_cond == "B" & !is.na(smtree_ht_prj) & smtree_ht_prj > 0, smtree_ht_prj, NA),
        tree_status = fifelse(smtree_status == "DS", "Dead", "Live"),
        tree_size = "Small"
      )]
      
      small_tree_table <- map_tree_species(tree_table = small_tree_table)
      order_col <- intersect(names(tree_table), names(small_tree_table))
      
      small_tree_table <- small_tree_table[,..order_col]
      tree_table <- rbindlist(list(tree_table, small_tree_table), fill = TRUE)
    } # End of include_small_tree
  
  # Correct tree status
  message("Correcting previous tree status | where trees were 'Dead' in previous but 'Live' in current, change previous status to 'Live'.")
  # Set order to be able to use shift correctly.
  setorder(tree_table, nfi_plot, loc_id, meas_num)
  
  tree_table[, `:=`(
    next_status   = shift(tree_status, type = "lead")
  ), by = .(nfi_plot, loc_id, tree_num)]

  tree_table[, tree_status_corr := fifelse(next_status == "Live" & tree_status == "Dead" & !is.na(next_status),
                                           "Live", tree_status)]
  # Print some reports
  status_corrected <- tree_table[next_status == "Live" & tree_status == "Dead" & !is.na(next_status)]
  status_corrected <- status_corrected[,.(status_corr_message = paste0("nfi_plot: ", nfi_plot, "\tloc_id: ", loc_id, "\tmeas_num: ", meas_num, "\ttree_num:", paste(tree_num, collapse = ", "))), by = .(nfi_plot, loc_id, meas_num)]
  n_status_corrected <- status_corrected |> nrow()
  
  message(paste0(n_status_corrected, " tree records corrected in:\n\t", 
                 paste(status_corrected$status_corr_message, collapse = "\n\t"), "\n\n"))
  
  ## --- Expansion Factor (Stems) ----------------------------------------------
  
  message("Calculating expansion factor as 1/nominal plot size\n\n")
  # Nominal plot size is used but may vary from one measurement to the next for large trees.
  # Or they may have not been submitted on establishment and are only present on re measurement.
  # For OSM those need be uniform across remeasruements
  setorder(tree_table, nfi_plot, loc_id, meas_num, tree_size)
  
  # Check the size of the next re-measurement.
  tree_table[, `:=`(
    next_nom_plot_size = shift(nom_plot_size, type = "lead")
  ), by = .(nfi_plot, loc_id, tree_num, tree_size)]
  
  
  tree_table[,Stems := fifelse(
    !is.na(nom_plot_size) & nom_plot_size > 0, (1/nom_plot_size),   # If nom_plot_size present, use that
    fifelse(tree_size == "Small", 1/0.0050,                         # If not, check if it is a small tree and use standard for small trees (0.0050).
    fifelse(next_nom_plot_size == 0.0405, 1/0.0405, 1/0.04)))]      # If not small, check whether the next measurement has some measurement and use that. Otherwise, default to 0.04.
  
  # Backfill this data for previous measurements where missing.
  tree_table[, `:=`(
    next_Stems = shift(Stems, type = "lead")
  ), by = .(nfi_plot, loc_id, tree_num, tree_size)]
  
  tree_table[ ,Stems := fifelse(is.na(Stems) & !is.na(next_Stems), next_Stems, Stems)]
  
  # }
  
  ## ToCut = NaN --------------
  ## Weight = NaN -------------
  ## HTK ----------------
  # Dealt with when preparing tree tables above.
  
  ## --- Crown Ratio (CR) ------------------------------------------------------
  # Crown condition 1 all foliage, branch and twigs present. 2 is some or small foliage lost but branches and twigs present, 3+ major loss.
  # Crown base and top can have missing data.
  # Stem condition (I = Intact, B = Broken, M = Missing) Not considering stem condition.
  message("Calculating Crown Ratio ('CR') where data is available.\n\n")
  tree_table[,CR := fifelse(crown_cond > 0 & crown_cond <= 2.5 & !is.na(crown_base) & is.na(crown_top), (crown_top-crown_base)/HT,
                            fifelse(crown_cond > 0 & crown_cond <= 2.5 & !is.na(crown_base) & !is.na(HT), (HT-crown_base)/HT,
                                    NA))]
  ## --- DBHI ------------------------------------------------------------------
  # Extract numeric year from meas_date (format "YYYY-MMM-DD")
  tree_table[, year := year(as.IDate(meas_date, format = "%Y-%b-%d"))]
  
  # Order for lead/lag operations
  setorder(tree_table, nfi_plot, loc_id, tree_num, meas_num)
  
  # Calculate DBH and HT increment
  tree_table[, `:=`(
    next_DBH      = shift(DBH, type = "lead"),
    next_HT       = shift(HT, type = "lead"),
    next_year     = shift(year, type = "lead"),
    next_status   = shift(tree_status, type = "lead")
  ), by = .(nfi_plot, loc_id, tree_num)]
  
  # Apply increment calculations only if both measurements are live and non-NA
  tree_table[, DBHI := fifelse(
    tree_status == "Live" & next_status == "Live" & !is.na(DBH) & !is.na(next_DBH),
    (next_DBH - DBH) / (next_year - year),
    NA_real_
  )]
  
  ## --- HTI -------------------------------------------------------------------
  
  tree_table[, HTI := fifelse(
    tree_status == "Live" & next_status == "Live" & !is.na(HT) & !is.na(next_HT),
    (next_HT - HT) / (next_year - year),
    NA_real_
  )]
  
  # Clean up helper columns
  tree_table[, c("next_DBH", "next_HT", "next_year", "next_status") := NULL]
  
  ## --- Origin  ---------------------------------------------------------------
  # Some data possible from plot_origin.csv variables 'veg_origin' and 'regen_type'
  
  ## --- Born ------------------------------------------------------------------
  age_path <- grep(x = file_list, pattern = "tree_age.csv", value = TRUE)
  message("Getting 'Born' variable from tree age table.\n")
  age_table <- fread(age_path, select = c("nfi_plot", "loc_id", "meas_date", "meas_num", "tree_num", "age_total"))
  tree_table <- merge(tree_table, age_table, by = c("nfi_plot", "loc_id", "meas_date", "meas_num", "tree_num"), all.x = TRUE)
  
  message("Filling tree age when age is available for time 't' but is missing at time 't-1'.\n\t - Only applies to trees that were large trees (DBH > 9) in both 't' and 't-1'.\n\t - Calculated as age at time 't' minus time lapsed (years) between time 't' and time 't-1'.\n\n")
  # Filling age_total gaps
  # Order data to make logic easier
  setorder(tree_table, nfi_plot, loc_id, meas_num, tree_num)
  
  # Step 2: Fill lower SurveyAges when higher ones exist
  tree_table <- tree_table[, age_total := {
    # check which entries are missing SurveyAge
    filled <- age_total
    
    if (any(is.na(age_total)) && any(!is.na(age_total))) {
      # Get last known age_total and year (from the max meas_num)
      known_idx <- which(!is.na(age_total))
      last_idx <- suppressWarnings(max(known_idx))
      
      # Fill earlier NAs only if they come before known value
      if (last_idx > 1) {
        for (i in seq_len(last_idx - 1)) {
          if (is.na(filled[i])) {
            years_diff <- year[last_idx] - year[i]
            filled[i] <- age_total[last_idx] - years_diff
          }
        }
      }
    }
    filled
  }, by = .(nfi_plot, loc_id, tree_num)]
  
  ## --- Died ------------------------------------------------------------------
  # NFI possible status for awareness 
  # DS = Dead fallen, LF = Live Fallen, LS = Live Stading, M = Missing.
  #  = estimate between measurements or 9999
  message("Identifying dead trees. They will be assumed to have died at the mid-year between previous and current remeasurement year. Tree that were 'Dead' at establishment will be assumed to have died 7 years ago as per OSM defaults.\n\n")
  
  # Order for detection
  setorder(tree_table, nfi_plot, loc_id, tree_num, meas_num)
  
  # Shift previous status and year forward
  tree_table[, `:=`(
    prev_status = shift(tree_status, type = "lag"),
    prev_year   = shift(year, type = "lag")
  ), by = .(nfi_plot, loc_id, tree_num)]
  
  # Calculate year of death as midpoint
  tree_table[, Died := fifelse(
    tree_status == "Dead" & prev_status == "Live" & !is.na(prev_year) & !is.na(year),
    floor((prev_year + year) / 2), fifelse(tree_status == "Dead" & meas_num == 0,
                                           9999, NA_integer_)
  )]
  
  tree_table[, c("prev_year", "prev_status") := NULL]
  

  ## Risk = 0 ------------------------------------------------------------------
  ## Grade = 0 -----------------------------------------------------------------
  ## Wrap-up -------------------------------------------------------------------
  
  data.table::setnames(tree_table, old = "tree_num", new = "TreeID")
  
  # Create ids like in standplot
  if(is.null(remeasurement_number)){
    stand_table[ ,SurveyID := paste0("ID", nfi_plot, "L", loc_id, "M", meas_num)]
    stand_table[, `:=` (
      nfi_plot = NULL,
      loc_id = NULL,
      meas_num = NULL
      )]

    tree_table[ ,SurveyID := paste0("ID", nfi_plot, "L", loc_id, "M", meas_num)]
    tree_table[, nfi_plot := NULL]
    data.table::setnames(tree_table, old = c("OSM_AD_CmdKey", "age_total"), new = c("Species", "Born"))
    
  } else {
    data.table::setnames(stand_table, old = c("nfi_plot") , new = c("SurveyID"))
    data.table::setnames(tree_table, old = c("nfi_plot", "OSM_AD_CmdKey", "age_total"), new = c("SurveyID", "Species", "Born"))
  }

  # Select only final columns
  if(model_variant == "Acadian"){
    stand_table_column_order <- c("Scenario", "SurveyID", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Zone", "Management", "BGI")
  } else {
    stand_table_column_order <- c("Scenario", "SurveyID", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Management")
  }
  stand_table <- stand_table[,..stand_table_column_order]
  tree_table_column_order <- c("SurveyID", "TreeID", "Species", "DBH", "HT", "DBHI", "HTI", "Stems", "CR", "Born", "Died")
  tree_table <- tree_table[,..tree_table_column_order]
  
  # Checking for errors or missed merges.
  in_tree_not_in_stand <- setdiff(unique(tree_table$SurveyID), unique(stand_table$SurveyID))
  in_stand_not_in_tree <- setdiff(unique(stand_table$SurveyID), unique(tree_table$SurveyID))
  
  unmatched_list <- paste(paste0("\t", in_stand_not_in_tree),  collapse = "\n")
  
  # Warnings and errors
  if(length(in_stand_not_in_tree)>0 & (!include_small_trees | only_remeasured_trees)){
    message(paste0("Warning | The NFI plots below have only small trees. Those records will be removed from the Stand Table since you have set 'include_small_trees = FALSE' or 'only_remeasured_trees = TRUE'\n", unmatched_list, "\n\n"))
    stand_table <- stand_table[!SurveyID %in% in_stand_not_in_tree]
    
  } else if (length(in_stand_not_in_tree)>0 & include_small_trees){
    message(paste0("Warning | NFI plots below could not be matched to tree data. Those will be removed from the Stand List Table\n",
                   "You can double check the source file at\n\t", ltp_tree_path, "\n\n", "for the nfi_plot ids\n", unmatched_list, "\n\n",
                   "If there is an issue, contact the developer.\n"))
    stand_table <- stand_table[!SurveyID %in% in_stand_not_in_tree]
  }
  
  if(length(in_tree_not_in_stand)>0){
    stop("Something is wrong with the code. There are tree data that does not match any stand data records. Contact developer.\n")
  }
  
  # Final data
  osm_input_data <- list(OSM_StandList = stand_table,
                         OSM_TreeList = tree_table)
  
  db_output_path <- file.path(output_path, paste0("nfitoosm_", scenario_name, ".sqlite"))
  
  # Write out SQLite
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_output_path)
  lapply(names(osm_input_data[1:2]), function(tbl_name){
    RSQLite::dbWriteTable(con, tbl_name, osm_input_data[[tbl_name]], overwrite = TRUE)
  }
  )
  RSQLite::dbDisconnect(con)
  
  message("OSM input table successfully created at:\n\n\t", db_output_path)
  
  #Return data
  return(osm_input_data)
}
