# Install and load required packages
if(!require(data.table)){install.packages("data.table")}
if(!require(RSQLite)){install.packages("RSQLite")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(sf)){install.packages("sf")}

library(data.table)
library(RSQLite)
library(dplyr)
library(sf)

nfi_folder <- "development/nfi_data"


RSQLite::dbquery
#' @param nfi_folder description
#' @param scenario_name description
#' @param measurement_number description
#' @param include_small_trees description
#' @param provinces description
#' @param model_variant description
#' @param calculate_expansion_factor Refer to the required OSM column "Stems" and defaults to `TRUE`.
#' Calculates expansion factors for DBH bins specified in `expansion_factor_dbh_bin`. The calculation will first group trees into DBH class sizes (e.g. (0-4],(4-8],(8-12], and etc.).
#' It then counts number of trees and average DBH and height in each bin. 
#' Finally the 'Stems' variable is calculate as the count of stem/measured_plot_size (generally 0.04 but sometimes less).
#' Averaged dbh and height will be the final values to be inputed into OSM.
#' If `FALSE` (less accurate) it will assume each individual tree has an expansion factor of 1/measured_plot_size (generally 0.04 but sometimes less).
#' @param expansion_factor_dbh_bin Only used if `calculate_expansion_factor = TRUE`. 
#' @param calculate_stockable (Optional) Refer to the optional OSM column "Stockable" defaults to `TRUE`.
#' OSM uses this information to adjust calculation there are portions of the site that are unproductive forest (e.g. roads or other). 
#' On NFI, the actual measured area (in ha) of plots (meas_plot_size) may be different than the designed size of the plot (nom_plot_size). 
#' This may happen because sites are assigned to the center of randomly established photo plots and may fall partially on non forested e.g. road, or other non-productive areas. 
#' However, it is important to note that there may be other reasons for not measuring the entire plot (fallen tree, inaccessible property or other safety reason).
#' If `TRUE` will calculate ratio of measured size (ha) to nominal size (ha) and assume unmeasured portions are unproductive, otherwise will use nominal size (ha) and assume the entire plot is productive.  
#' Also, if using `meas_num = 0` there is a higher chance fully productive plots being incompletely measured (due to cost issues). See page 12 of \link[https://nfi.nfis.org/resources/groundplot/GP_compilation_procedures_2.4.pdf]{Compilation Procedures }
#' @return list with two dataframes and a creates a database on specified output_path. The dataframes should match the input data exactly of \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{Open Stand Model}.
#' Multiple assumptions are made on the creation of each variable and they are described below.
#' \describe{
#'   \item{Scenario}{First item}
#'   \item{SrveyID}{Second item}
#'   \item{SurveyYear}{Second item}
#'   \item{SurveyAge}{Second item}
#'   \item{Plots}{Second item}
#'   \item{Stockable}{Second item}
#'   \item{X}{Second item}
#'   \item{Y}{Second item}
#'   }
#'   
#' \describe{
#'   \item{SurveyID}{First item}
#'   \item{TreeID}{Second item}
#'   \item{Species}{Second item}
#'   \item{DBH}{Second item}
#'   \item{HT}{Second item}
#'   \item{Stems}{Second item}
#'   \item{ToCut}{Second item}
#'   \item{Weight}{Second item}
#'   \item{HTK}{Second item}
#'   \item{CR}{Second item}
#'   \item{DBHI}{Second item}
#'   \item{HTI}{Second item}
#'   \item{Origin}{Second item}
#'   \item{Born}{Second item}
#'   \item{Died}{Second item}
#'   \item{Risk}{Second item}
#'   \item{Grade}{Second item}
#'   }

nfi_to_osm <- function(nfi_folder,
                       scenario_name = "nfi_simulation",
                       measurement_number,
                       include_small_trees = TRUE,
                       provinces = c("New Brunswick", "Prince Edward Island", "Nova Scotia"),
                       model_variant = c("Acadian", "Newfoundland and Labrador"),
                       
                       calculate_stockable = TRUE,
                       calculate_expansion_factor = TRUE,
                       expansion_factor_dbh_bin = 4,
                       output_path = get_wd()
) {
  
#
  #
  # DIED AND SP MISSING ONLY
  #
  #
  #
  #
  #
  #
  #
  file_list <- list.files(nfi_folder,  "all_gp_.*\\.csv", recursive = TRUE, full.names = TRUE)
  
  # --- Plot Table ------------------------------------------------------------
  # Read site_info
  site_info_path <- grep(x = file_list, pattern =  file.path(nfi_folder, "all_gp_site_info.csv"), value = TRUE)
  site_info <- fread(site_info_path)
  
  ## Scenario ------------------------------------------------------------------
  site_info[, Scenario := scenario_name]
  
  # Select and rename columns for plot table
  plot_table <- site_info[, .(
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
   
  ## Survey Year --------------------------------------------------------------
  # Create year variable and remove intermediate variables used to calculate LAT and LONG
  # plot_table[, meas_yr := year(as.Date(meas_date, "%Y-%b-$d"))]
  plot_table[, SurveyYear := tidyr::separate(.SD, col = meas_date, into = "meas_yr", sep = "-", extra = "drop"), .SDcols = "meas_date"]
  plot_table[, SurveyYear := as.integer(SurveyYear)]
  
  ## Survey Age ---------------------------------------------------------------
  # Read ltp_tree_header
  ltp_header_path <- grep(x = file_list, pattern =  "ltp_header.csv", value = TRUE)
  ltp_header <- fread(ltp_header_path)
  
  plot_table <- merge(plot_table, ltp_header[,.(
    nfi_plot, loc_id, meas_date, meas_num, # Id fields
    site_age,                              # Age field
    meas_plot_size, 
    nom_plot_size # size field (For "Stockable" variable)
    )], by = c("nfi_plot", "loc_id", "meas_date", "meas_num"))
  
  data.table::setnames(plot_table, old = "site_age", new = "SurveyAge")
  
  ## Plots --------------------------------------------------------------------
  plot_table[,Plots := 1]
  
  ## Stockable ----------------------------------------------------------------
  # If measured is chosen 
  plot_table[, Stockable := fifelse(calculate_stockable & nom_plot_size > 0, 
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
  plot_table[, `:=`(Y = NA_real_, X = NA_real_)] # Initialize columns
  zones <- unique(plot_table$utm_zone) # Get unique UTM zones
  
  for (zone in zones) {
    rows <- plot_table$utm_zone == zone
    coords <- utm_to_longlat(plot_table[rows, utm_e], plot_table[rows, utm_n], zone)
    plot_table[rows, `:=`(Y = coords$lat, X = coords$lon)]
  }
  
  
  #
  #
  #
  #
  # PLACEHOLDER FILTER meas_num
  #
  #
  #
  #
  #
  
  
  # Location change (loc_id) may not impact model setup.
  # plot_table[loc_id == 0,]
  
  
  error_model_variant = "Please choose either 'Acadian' or 'Newfoundland and Labrador' as a model_variant."
  if(length(model_variant) != 1){
    stop(error_model_variant)
  }
  
  pt <- setNames(
    c("New Brunswick", "Prince Edward Island", "Nova Scotia"),
    c("NB", "PE", "NS")
  )
  
  pt_filter <- pt[pt %in% provinces] |> names()
  plot_table <- plot_table[province %in% pt_filter,]
  
  if(model_variant == "Acadian"){
    
    # Zone    
    plot_table[, Zone := province]
    
    # PLACEHOLDER to add Management (possible to add BGI with a lot o work..)
    column_order <- c("Scenario", "nfi_plot", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Zone", "Management")
    
  } else if(model_variant == "Newfoundland and Labrador"){
    
    if(!"NL" %in% pt_filter){
      warning("You removed the Newfoundland and Labrador from the 'provinces' argument but specified model_variant = 'Newfoundland and Labrador'.\nPerhaps you meant to use 'model_variannt = Acadian'?\n\nModel will default the 'District' column to 4 'NL'.")
      # District will default to 4 as per:
      # https://forusresearch.com/downloads/osm/help/OSM.Variants/OSM.Variants.NL/OSM.Variants.NL.HelpFiles/OSM.Variants.NL.InputTables.htm
      column_order <- c("Scenario", "nfi_plot", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Management")
      
    }
    
    # PLACEHOLDER to add District, Management, MAT
    
  } else {
    
    stop(error_model_variant)
    
  }
  
  # Management (Extra column)
  treatment_path <- grep(x = file_list, pattern =  "all_gp_treatment.csv", value = TRUE)
  treatment <- fread(treatment_path, select = c("nfi_plot", "loc_id", "meas_date", "meas_num", "treat_type", "treat_yr"))
  
  plot_table <- merge(plot_table, treatment)
  plot_table[, meas_yr := as.integer(substr(meas_date, 1, 4))]
  plot_table[, treat_last25_yt := meas_yr-treat_yr <= 25]
  plot_table[, Management :=  fifelse(treat_type == "PC" & treat_last25_yt, "PartialCut",
                                      fifelse(treat_type == "CC" & treat_last25_yt, "Clearcut",
                                              fifelse(treat_type == "PT" & treat_last25_yt, "PCT", "None")))]
  # data.table::setnames(plot_table, "nfi_plot" = "SurveyID")
  
  
  plot_table <- plot_table[,..column_order]
  
  # --- Tree Table -------------------------------------------------------------
  
  ## --- Large Tree Table ------------------------------------------------------
  # Read ltp_tree 
  ltp_tree_path <- grep(x = file_list, pattern =  "ltp_tree.csv", value = TRUE)
  ltp_tree <- fread(ltp_tree_path)
  
  
  # Merge the two tables
  tree_table <- merge(ltp_tree, ltp_header, by = c("nfi_plot", "loc_id", "meas_date", "meas_num"))
  
  # Select and rename columns for tree table
  tree_table <- tree_table[, .(
    nfi_plot = nfi_plot,
    TreeID = tree_num,
    lgtree_species = lgtree_species, # intermediate to get proper names
    DBH = dbh,
    HT = height,
    tree_status = lgtree_status # Intermediate to filter
  )]
  
  #
  #
  #
  #
  # PLACEHOLDER SP NAME
  #
  #
  #
  #
  #
  
  ## --- Small Tree Table ------------------------------------------------------
  if(include_small_trees){
    
    # Read stp_tree and stp_header
    ltp_tree_path <- grep(x = nfi_folder, pattern =  "stp_tree.csv", value = TRUE)
    ltp_header_path <- grep(x = nfi_folder, pattern =  "stp_header.csv", value = TRUE)
    
    stp_tree <- fread(file_list[49])
    stp_header <- fread(file_list[48])
    
    # Merge the two tables
    small_tree_table <- merge(stp_tree, stp_header, by = c("nfi_plot", "loc_id", "meas_date", "meas_num"))
    
    # Select and rename columns for small tree table
    small_tree_table <- small_tree_table[, .(
      nfi_plot = nfi_plot,
      tree = smtree_num,
      spp = smtree_species,
      DBH = smtree_dbh,
      HT = smtree_ht,
      tree_status = smtree_status
    )]
    
    tree_table <- rbindlist(list(tree_table, small_tree_table))
  }
  
  ## --- Expansion Factor (Stems) ----------------------------------------------
  if(calculate_expansion_factor){
    
  tst <- copy(tree_table)
  tst_rm <- tst[dbh > 0.1]
  
  tst_rm[,DBHBin := cut(dbh, seq(0, max(dbh), by = expansion_factor_dbh_bin))]
  
  dbhbin_summary <- tst_rm[, .(n_stems = .N,
                             nfi_plot = unique(nfi_plot),
                             meas_plot_size = mean(meas_plot_size),
                             dbh_mean = mean(dbh, na.rm = T),
                             height = mean(ifelse(height < 0, NA, height), na.rm = TRUE)),
                             by = .(loc_id, meas_date, meas_num, lgtree_genus, lgtree_species, DBHBin)]
  dbhbin_summary[,Stems := (n_stems/meas_plot_size)]
  merge(plot_table, dbhbin_summary)
  
  } else {

    tst <- copy(tree_table)
    tst_rm <- tst[dbh > 0.1]
    tst_rm[,Stems := (1/meas_plot_size)]
    
  }
  
  ## ToCut = NaN --------------
  ## Weight = NaN -------------
  ## HTK = NaN ----------------
  
  ## --- Crown Ratio (CR) -------------------------------------------------------
  # Crown condition 1 all foliage, branch and twigs preent. 2 is some or small foliage lost but branches and twigs present, 3+ major loss.
  # Crown base and top can have missing data.
  # Stem condition (I = Instact, B = Broken, M = Missing) Not considering stem condition.
  tst_rm[,CR := fifelse(crown_cond %in% c(1, 2) & crown_base > 0 & crown_top > 0, (crown_top-crown_base)/height,
                        fifelse(crown_cond %in% c(1, 2) & crown_base > 0 & crown_top < 0, (height-crown_base)/height,
                                NA))]
  ## DBHI = NaN --------
  # = possible but skip
  ## HTI = NaN ---------
  # = possible but skip
  
  ## --- Origin  ----------------------------------------------------------------
  # Some data possible form plot_origin.csv variables 'veg_origin' and 'regen_type'
  
  ## --- Born -------------------------------------------------------------------
  age_path <- grep(x = file_list, pattern = "tree_age.csv", value = TRUE)
  age <- fread(age_path, select = c("nfi_plot", "loc_id", "meas_date", "meas_num", "tree_num", "age_total"))
  tst_rm_age <- merge(tst_rm, age, by = c("nfi_plot", "loc_id", "meas_date", "meas_num", "tree_num"), all.x = TRUE)
  
  ## --- Died -------------------------------------------------------------------
  #  = estimate between measurements or 9999
  
  ## Risk = 0 -------------------------------------------------------------------
  ## Grade = 0 ------------------------------------------------------------------

  
  # Combine large and small tree tables
  
  #Return data
  list(plot_table = plot_table, tree_table = tree_table)
  
  # rename nfi_plot to SurveyID
  data.table::setnames(plot_table, "nfi_plot" = "SurveyID")
}


# Process the data
nfi_data <- process_nfi_data(data_dir, nfi_folder)

# --- Create SQLite Database ---

# 1. Establish Connection
db_file <- "osm_input.sqlite"  # Name of your SQLite database file
conn <- dbConnect(SQLite(), dbname = db_file)


# 2. Write Data to Tables

dbWriteTable(conn, "Plot", nfi_data$plot_table, overwrite = TRUE)
dbWriteTable(conn, "Tree", nfi_data$tree_table, overwrite = TRUE)

# 3. Disconnect
dbDisconnect(conn)

cat("Database created successfully at:", db_file, "\n")