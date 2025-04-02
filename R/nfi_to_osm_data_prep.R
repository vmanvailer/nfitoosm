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
#' @param provinces Vector of strings. Provinces for which you want data for. Will only retrieve data for one of the four provinces in the argument default.
#' @param model_variant Vector of string. OSM has two models. The "Acadian" model variant has been calibrated for the provinces of New Brunswick, Prince Edward Island and Nova Scotia. The "Newfoundland and Labrador" model has been calibrated for Newfoundland and Labrador only.
#' @param include_small_trees Logical. Defaults to true to maximize data use. Will include all individual trees measured on the small plot data. See page 32 of \link[https://nfi.nfis.org/resources/groundplot/Gp_guidelines_v5.0.pdf](Ground Plot Guidelines).
#' @param dbh_filter Numeric. A field to filter out trees based on DBH. Will pick DBH higher than or equal to this value.
#' @param use_projected_height Logical. When both (measured, calculated or estimated) height and projected height are available. Prefer projected height. Projected height are calculated for broken stems (i.e. stem_cond == "B").
#' @param calculate_ef_for_dbh_bin Logical. Refer to the required OSM column "Stems" and defaults to `TRUE`.
#' Calculates expansion factors for DBH bins specified in `ef_dbh_increment_bin`, instead of for each individual tree. The calculation will first group trees into DBH class sizes (e.g. (0-4],(4-8],(8-12], and etc.).
#' It then aggregates data by counting number of trees and averaging DBH and height for each bin. 
#' Finally the 'Stems' variable is calculate as the count of stem/measured_plot_size (generally 0.04 but sometimes less).
#' 
#' Aggregated Stems, DBH and Height will represent individual trees to be used as input in OSM.
#' If `FALSE` it will assume each individual tree has an expansion factor of 1/measured_plot_size. This may be less prefered as it gives equal weights to all individual trees in the plot.
#' @param ef_dbh_increment_bin Numeric. In centimeters. Only used if `calculate_ef_for_dbh_bin = TRUE`. 
#' @param output_path String. Use to output the SQLite database that can be passed to OSM. 
#' @return Returns a list with two data frames (data.tables) and a writes a database on specified output_path. The data frames should match the input data exactly as in \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{Open Stand Model}.
#' Multiple assumptions are made on the creation of each variable and they are described below.
#' \itemize{
#'    \item \strong{STAND LIST TABLE}
#'          \describe{
#'            \item{Scenario}{User specified}
#'            \item{SurveyID}{Same as `nfi_plot` in NFI data.}
#'            \item{SurveyYear}{The year from `meas_date` in \emph{all_gp_site_info.csv} NFI data.}
#'            \item{SurveyAge}{The stand age from `site_age` in \emph{all_gp_ltp_header.csv} NFI data. Uses arithmetic average age of trees deemed suitable for compiling site age. For more details see page 129 of \link[https://nfi.nfis.org/resources/groundplot/Gp_guidelines_v5.0.pdf](Ground Plot Guidelines).}
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
                       provinces = c("New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador"),
                       model_variant = c("Acadian", "Newfoundland and Labrador"),
                       include_small_trees = TRUE,
                       dbh_filter = 0,
                       use_projected_height = TRUE,
                       calculate_ef_for_dbh_bin = TRUE,
                       ef_dbh_increment_bin = 4,
                       output_path = getwd()
) {
  
  file_list <- list.files(nfi_folder,  "all_gp_.*\\.csv", recursive = TRUE, full.names = TRUE)
  
  # --- Plot Table ------------------------------------------------------------
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
    # elev = elevation,
    utm_n = utm_n, # intermediate variable to calculate LAT
    utm_e = utm_e, # intermediate variable to calculate LONG
    utm_zone = utm_zone, # intermediate variable to calculate LAT/LONG
    province = province
  )]
  
  # Filter to a given field campaign
  if(is.null(remeasurement_number)){
    message("No remeasurement_number defined. Defaulting to first remeasurement campaign (meas_num = 1) which has measured most plots and most variables.\n")
    stand_table <- stand_table[meas_num == 1]
  } else {
    stand_table <- stand_table[meas_num == remeasurement_number]
    if(nrow(stand_table) == 0){
      stop(paste0("There is no data for remeasurement number ", remeasurement_number))
    }
  }
  
  # Location change (loc_id) may not impact model setup.
  # stand_table[loc_id == 0,]
  
  ## Survey Year --------------------------------------------------------------
  # Create year variable and remove intermediate variables used to calculate LAT and LONG
  # stand_table[, meas_yr := year(as.Date(meas_date, "%Y-%b-$d"))]
  stand_table[, SurveyYear := tidyr::separate(.SD, col = meas_date, into = "meas_yr", sep = "-", extra = "drop"), .SDcols = "meas_date"]
  stand_table[, SurveyYear := as.integer(SurveyYear)]
  
  ## Survey Age ---------------------------------------------------------------
  # Read ltp_tree_header
  ltp_header_path <- grep(x = file_list, pattern =  "ltp_header.csv", value = TRUE)
  ltp_header <- fread(ltp_header_path)
  
  stand_table <- merge(stand_table, ltp_header[,.(
    nfi_plot, loc_id, meas_date, meas_num, # Id fields
    site_age,                              # Age field
    meas_plot_size, 
    nom_plot_size # size field (For "Stockable" variable)
    )], by = c("nfi_plot", "loc_id", "meas_date", "meas_num"))
  
  data.table::setnames(stand_table, old = "site_age", new = "SurveyAge")
  
  ## Plots --------------------------------------------------------------------
  stand_table[,Plots := 1]
  
  ## Stockable ----------------------------------------------------------------
  # If measured is chosen 
  if(calculate_stockable){
    message("Calculating 'Stockable' as measured plot size / nominal plot size (where both data exists).\n\n")
    } else {
    message("Setting 'Stockable' to 1.\n\n")
  }
  stand_table[, Stockable := fifelse(calculate_stockable & nom_plot_size > 0, 
                                    meas_plot_size/nom_plot_size,
                                    1)
             ]
  
  ## Convert UTM to Lat Long --------------------------------------------------
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
  
  
  if(model_variant == "Acadian"){
    not_allowed_pt <- pt[!provinces %in% c("New Brunswick", "Prince Edward Island", "Nova Scotia")]
    if(length(not_allowed_pt) > 0){
      message(paste0("Warning | '",not_allowed_pt, "' is not calibrated for the Acadian model.\n"))
    }
    
    pt_filter <- pt[pt %in% provinces] |> names()
    stand_table <- stand_table[province %in% pt_filter,]
    
    if(nrow(stand_table) == 0){
      stop(paste0("There is no data for remeasurement number ", remeasurement_number, "for any of:\n", paste0(paste0("\t", provinces), collapse = "\n")))
    }
    
    # Zone    
    stand_table[, Zone := province]
    
    # PLACEHOLDER to add Management (possible to add BGI with a lot o work..)
    column_order <- c("Scenario", "nfi_plot", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Zone", "Management")
    
  } else if(model_variant == "Newfoundland and Labrador"){
    
    if(!"NL" %in% pt_filter){
      message("Warning | You removed the Newfoundland and Labrador from the 'provinces' argument but specified model_variant = 'Newfoundland and Labrador'.\nPerhaps you meant to use 'model_variannt = Acadian'?\n\nModel will default the 'District' column to 4 'NL'.\n")
      # District will default to 4 as per:
      # https://forusresearch.com/downloads/osm/help/OSM.Variants/OSM.Variants.NL/OSM.Variants.NL.HelpFiles/OSM.Variants.NL.InputTables.htm
      column_order <- c("Scenario", "nfi_plot", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Management")
      
    }
    
  } else {
    
    stop(error_model_variant)
    
  }
  
  # Management (Extra column)
  treatment_path <- grep(x = file_list, pattern =  "all_gp_treatment.csv", value = TRUE)
  treatment <- fread(treatment_path, select = c("nfi_plot", "loc_id", "meas_date", "meas_num", "treat_type", "treat_yr"))
  
  stand_table <- merge(stand_table, treatment)
  stand_table[, meas_yr := as.integer(substr(meas_date, 1, 4))]
  stand_table[, treat_last25_yt := meas_yr-treat_yr <= 25]
  stand_table[, Management :=  fifelse(treat_type == "PC" & treat_last25_yt, "PartialCut",
                                      fifelse(treat_type == "CC" & treat_last25_yt, "Clearcut",
                                              fifelse(treat_type == "PT" & treat_last25_yt, "PCT", "None")))]
  
  stand_table <- stand_table[,..column_order]
  
  data.table::setnames(stand_table, old = "nfi_plot", new = "SurveyID")
  
  
  # --- Tree Table -------------------------------------------------------------
  
  # Prepare species list found on the Acadian model to be merged with nfi data. We want the proper species code to use on OSM.
  setDT(acadian_species_list)
  acadian_species_list[, .(GENUS, SPECIES)]
  acadian_species_list[, `:=` (NFI_GENUS = toupper(substr(GENUS, 1, 4)),
                               NFI_SPECIES = toupper(substr(SPECIES, 1, 3))
  )]
  
  ## --- Large Tree Table ------------------------------------------------------
  # Read ltp_tree 
  ltp_tree_path <- grep(x = file_list, pattern =  "ltp_tree.csv", value = TRUE)
  ltp_tree <- fread(ltp_tree_path)
  
  # Merge the two tables
  large_tree_table <- merge(ltp_tree, ltp_header, by = c("nfi_plot", "loc_id", "meas_date", "meas_num"))
  
  if(is.null(remeasurement_number)){
    large_tree_table <- large_tree_table[meas_num == 1]
  } else {
    large_tree_table <- large_tree_table[meas_num == remeasurement_number]
  }
  
  # Location change (loc_id) may not impact model setup.
  # large_tree_table[loc_id == 0,]
  
  # Select and rename columns for tree table
  large_tree_table <- large_tree_table[, .(
    nfi_plot = nfi_plot,
    loc_id = loc_id,
    meas_num = meas_num,
    meas_date = meas_date,
    meas_plot_size = meas_plot_size,
    tree_num = tree_num,
    tree_genus = lgtree_genus,
    tree_species = lgtree_species, 
    DBH = dbh,
    HT = fifelse(use_projected_height & !is.na(height_prj) & height_prj > 0, height_prj, height),
    crown_cond = crown_cond,
    crown_base = crown_base,
    crown_top = crown_top,
    tree_status = fifelse(lgtree_status == "DS", "Dead", "Live"),
    tree_size = "Large"
  )]
  
  map_tree_species <- function(tree_table){
    species_columns <- c("tree_genus", "tree_species")
    
    # Add Softwood/Hardwood classification for species
    tree_table <- merge(tree_table, 
                        unique(nfi_species[,.(GROUP, CODE_GENU, CODE_SPEC)]),
                        by.x = species_columns,
                        by.y = c("CODE_GENU", "CODE_SPEC"),
    )
    
    # Based on the Acadian Demo files I'll use the OSM_AD_CmdKey column.
    tree_table <- merge(tree_table,
                        acadian_species_list[,.(NFI_GENUS, NFI_SPECIES, OSM_AD_CmdKey)],
                        by.x = species_columns,
                        by.y = c("NFI_GENUS", "NFI_SPECIES"),
                        all.x = TRUE,
                        allow.cartesian = TRUE)
    
    # If NFI species don't match OSM Acadian Species List, check NFI species list for Softwood (OS)/Hardwood (OH) and use those codes.
    # If OS/OH not found, assign "XX" to represent an unknown species on OSM. 
    tree_table[, OSM_AD_CmdKey := fifelse(is.na(OSM_AD_CmdKey) & GROUP == "Softwood", "OS",  
                                          fifelse(is.na(OSM_AD_CmdKey) & GROUP == "Hardwood", "OH", "XX"))]
    return(tree_table)
  }
  
  tree_table <- map_tree_species(tree_table = large_tree_table)
  
  ## --- Small Tree Table ------------------------------------------------------
  if(include_small_trees){
    message("Including small trees.\n\n")
    
    # Read stp_tree and stp_header
    stp_header_path <- grep(x = file_list, pattern =  "stp_header.csv", value = TRUE)
    stp_tree_path <- grep(x = file_list, pattern =  "stp_tree.csv", value = TRUE)
    
    stp_header <- fread(stp_header_path)
    stp_tree <- fread(stp_tree_path)
    
    # Merge the two tables
    small_tree_table <- merge(stp_tree, stp_header, by = c("nfi_plot", "loc_id", "meas_date", "meas_num"))
    
    if(is.null(remeasurement_number)){
      small_tree_table <- small_tree_table[meas_num == 1]
    } else {
      small_tree_table <- small_tree_table[meas_num == remeasurement_number]
    }
    
    # Select and rename columns for small tree table
    small_tree_table <- small_tree_table[, .(
      nfi_plot = nfi_plot,
      loc_id = loc_id,
      meas_num = meas_num,
      meas_date = meas_date,
      meas_plot_size = meas_plot_size,
      tree_num = smtree_num,
      tree_genus = smtree_genus,
      tree_species = smtree_species,
      DBH = smtree_dbh,
      HT = fifelse(use_projected_height & !is.na(smtree_ht_prj) & smtree_ht_prj > 0, smtree_ht_prj, smtree_ht),
      tree_status = fifelse(smtree_status == "DS", "Dead", "Live"),
      tree_size = "Small"
    )]
    
    small_tree_table <- map_tree_species(tree_table = small_tree_table)
    
    tree_table <- rbindlist(list(large_tree_table, small_tree_table), fill = TRUE)
  }
  
  # Filter files to include only those filtered in stand list.
  tree_table <- tree_table[nfi_plot %in% unique(stand_table$SurveyID),]
  
  ## --- Expansion Factor (Stems) ----------------------------------------------
  if(dbh_filter > 0) messaging("Including only trees with DBH >=", dbh_filter, "\n\n")
  tree_table <- tree_table[DBH >= dbh_filter]
  
  if(calculate_ef_for_dbh_bin){
    message("Calculating expansion factor (OSM variable 'Stems') using DBH bins of size : ", ef_dbh_increment_bin, "\n",
            "Will average 'DBH', 'Height', and 'Crown' variables for each DBH bin to produce Tree List Table.\n\n")
    tree_table[,DBHBin := cut(DBH, seq(0, max(DBH), by = ef_dbh_increment_bin))]
    tree_table_unaggregated <- copy(tree_table)
    tree_table <- tree_table[, .(n_stems = .N,
                                 meas_plot_size = mean(meas_plot_size),
                                 DBH = mean(DBH, na.rm = T),
                                 HT = mean(ifelse(HT < 0, NA, HT), na.rm = TRUE),
                                 crown_cond = mean(ifelse(crown_cond < 0, NA, crown_cond), na.rm = TRUE),
                                 crown_base = mean(ifelse(crown_base < 0, NA, crown_base), na.rm = TRUE),
                                 crown_top = mean(ifelse(crown_top < 0, NA, crown_top), na.rm = TRUE)
                                 ),
                             by = .(nfi_plot, loc_id, meas_date, meas_num, tree_genus, tree_species, OSM_AD_CmdKey, tree_status, DBHBin)]
    tree_table[ ,Stems := (n_stems/meas_plot_size)]
    
    tree_table_unaggregated[,Stems := (1/meas_plot_size)]
    
  } else {
    
    message("Calculating expansion factor as 1/measured plot size\n\n")
    tree_table[,Stems := (1/meas_plot_size)]
    
  }
  
  ## ToCut = NaN --------------
  ## Weight = NaN -------------
  ## HTK = NaN ----------------
  
  ## --- Crown Ratio (CR) -------------------------------------------------------
  # Crown condition 1 all foliage, branch and twigs present. 2 is some or small foliage lost but branches and twigs present, 3+ major loss.
  # Crown base and top can have missing data.
  # Stem condition (I = Intact, B = Broken, M = Missing) Not considering stem condition.
  message("Calculating Crown Ratio ('CR') where data is available.\n\n")
  tree_table[,CR := fifelse(crown_cond > 0 & crown_cond <= 2.5 & !is.na(crown_base) & is.na(crown_top), (crown_top-crown_base)/HT,
                            fifelse(crown_cond > 0 & crown_cond <= 2.5 & !is.na(crown_base) & !is.na(HT), (HT-crown_base)/HT,
                                    NA))]
  if(calculate_ef_for_dbh_bin){
    tree_table_unaggregated[,CR := fifelse(crown_cond > 0 & crown_cond <= 2.5 & !is.na(crown_base) & is.na(crown_top), (crown_top-crown_base)/HT,
                                           fifelse(crown_cond > 0 & crown_cond <= 2.5 & !is.na(crown_base) & !is.na(HT), (HT-crown_base)/HT,
                                                   NA))]
  }
  ## DBHI = NaN --------
  # = possible but skip
  ## HTI = NaN ---------
  # = possible but skip
  
  ## --- Origin  ----------------------------------------------------------------
  # Some data possible from plot_origin.csv variables 'veg_origin' and 'regen_type'
  
  ## --- Born -------------------------------------------------------------------
  age_path <- grep(x = file_list, pattern = "tree_age.csv", value = TRUE)
  message("Getting 'Born' variable from tree age table.")
  age_table <- fread(age_path, select = c("nfi_plot", "loc_id", "meas_date", "meas_num", "tree_num", "age_total"))
  
  if(calculate_ef_for_dbh_bin){
    message("'calculate_ef_for_dbh_bin = TRUE'. Averaging tree age based on expansion factor DBH bin\n\n")
    age_table_aggregated <- merge(age_table, ltp_tree[,.(nfi_plot, loc_id, meas_date, meas_num, tree_num,
                                  tree_genus = lgtree_genus, 
                                  tree_species = lgtree_species,
                                  tree_status = fifelse(lgtree_status == "DS", "Dead", "Live"),
                                  DBH = dbh)])
    age_table_aggregated[, DBHBin := cut(DBH, seq(0, max(DBH), by = ef_dbh_increment_bin))]
    age_table_aggregated <- age_table_aggregated[, .(age_total = mean(age_total)),
                                                 by = .(nfi_plot, loc_id, meas_date, meas_num, tree_genus, tree_species, tree_status, DBHBin)] # Same as when calculating expansion factor.
    tree_table <- merge(tree_table, age_table_aggregated, all.x = TRUE)
    
    tree_table_column_order <- c("SurveyID", "Species", "DBH", "HT", "Stems", "CR", "Born", "Died")
    
    tree_table_unaggregated <- merge(tree_table_unaggregated, age_table, by = c("nfi_plot", "loc_id", "meas_date", "meas_num", "tree_num"), all.x = TRUE)
    data.table::setnames(tree_table_unaggregated, old = "tree_num", new = "TreeID")
    tree_table_unaggregated_column_order <- c("SurveyID", "TreeID", "Species", "DBH", "HT", "Stems", "CR", "Born", "Died")
    
  } else{
    tree_table <- merge(tree_table, age_table, by = c("nfi_plot", "loc_id", "meas_date", "meas_num", "tree_num"), all.x = TRUE)
    data.table::setnames(tree_table, old = "tree_num", new = "TreeID")
    tree_table_column_order <- c("SurveyID", "TreeID", "Species", "DBH", "HT", "Stems", "CR", "Born", "Died")
  }
  
  
  ## --- Died ------------------------------------------------------------------
  #  = estimate between measurements or 9999
  
  # DS = Dead fallen, LF = Live Fallen, LS = Live Stading, M = Missing.
  # Assumes all which are not dead to be live standing.
  message("Identifying dead trees. They will be assumed to have died 7 years ago as per OSM defaults.\n\n")
  tree_table[,Died := fifelse(tree_status == "DS", 9999, 0)]
  tree_table_unaggregated[,Died := fifelse(tree_status == "DS", 9999, 0)]
  ## Risk = 0 ------------------------------------------------------------------
  ## Grade = 0 -----------------------------------------------------------------
  ## Wrap-up -------------------------------------------------------------------
  data.table::setnames(tree_table, old = c("nfi_plot", "OSM_AD_CmdKey", "age_total"), new = c("SurveyID", "Species", "Born"))

  tree_table <- tree_table[,..tree_table_column_order]
  
  if(calculate_ef_for_dbh_bin){
    data.table::setnames(tree_table_unaggregated, old = c("nfi_plot", "OSM_AD_CmdKey", "age_total"), new = c("SurveyID", "Species", "Born"))
    tree_table_unaggregated <- tree_table_unaggregated[,..tree_table_unaggregated_column_order]
  }
  
  in_tree_not_in_stand <- setdiff(unique(tree_table$SurveyID), unique(stand_table$SurveyID))
  in_stand_not_in_tree <- setdiff(unique(stand_table$SurveyID), unique(tree_table$SurveyID))
  
  unmatched_list <- paste(paste0("\t", in_stand_not_in_tree),  collapse = "\n")
  
  if(length(in_stand_not_in_tree)>0 & !include_small_trees){
    message(paste0("Warning | The NFI plots below have only small trees. Those will be removed since you have set 'include_small_trees = FALSE'\n", unmatched_list, "\n\n"))
  } else if (length(in_stand_not_in_tree)>0 & include_small_trees){
    message(paste0("Warning | NFI plots below could not be matched to tree data. Those will be removed from the Stand List Table\n",
                   "You can double check the source file at\n\t", ltp_tree_path, "\n\n", "for the nfi_plot ids\n", unmatched_list, "\n\n",
                   "If there is an issue, contact the developer.\n"))
  }
  
  if(length(in_tree_not_in_stand)>0){
    stop("Something is wrong with the code. There are tree data that does not match any stand data records. Contact developer.\n")
  }
  
  osm_input_data <- list(OSM_StandList = stand_table,
                         OSM_TreeList = tree_table,
                         OSM_TreeList_unaggregated = if (calculate_ef_for_dbh_bin) tree_table_unaggregated else NULL
                         )
  if(is.null(osm_input_data$tree_table_unaggregated)){
    osm_input_data <- osm_input_data[1:2]
  }
  
  if(!dir.exists(output_path)){
    stop("Specified directory below does not exist\n\n\t", output_path)
  }
  
  db_output_path <- file.path(output_path, "nfi_to_osm_input_data.sqlite")
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