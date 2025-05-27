#' Map Tree Species to OSM Command Keys
#'
#' Matches tree species from NFI-derived data to the standardized `OSM_AD_CmdKey`
#' used by the Open Stand Model (OSM), using both genus-species and genus-only lookup tables.
#'
#' This function performs a multi-step matching process to assign the correct OSM command key:
#' \enumerate{
#'   \item Performs an exact match on genus and species using the \code{acadian_gnsp_only} reference.
#'   \item If no match is found, attempts a fallback match using only genus (rows with species code \code{"SPP"}).
#'   \item If still unmatched, assigns fallback codes based on species group:
#'     \itemize{
#'       \item \code{"OS"} for Softwoods
#'       \item \code{"OH"} for Hardwoods
#'       \item \code{"XX"} for unknown or unmatched species
#'     }
#' }
#'
#' It also merges softwood/hardwood group classification from the \code{nfi_species} reference table.
#'
#' @param tree_table A \code{data.table} containing at least the columns \code{tree_genus} and \code{tree_species}.
#'
#' @return A modified \code{data.table} with additional columns:
#' \describe{
#'   \item{\code{GROUP}}{The species group (e.g., "Softwood", "Hardwood").}
#'   \item{\code{OSM_AD_CmdKey}}{The OSM command key for each tree, used to identify species in the model.}
#' }
#'
#' @details
#' This function relies on two reference objects that are available as part of the package:
#' \itemize{
#'   \item \code{nfi_species}: Lookup table of genus/species and species group.
#'   \item \code{acadian_gnsp_only}: OSM species lookup table with genus/species and fallback genus-only entries.
#' }
#'
#' @examples
#' \dontrun{
#' tree_table <- data.table(tree_genus = "Picea", tree_species = "mariana")
#' tree_table <- map_tree_species(tree_table)
#' }
#'
#' @export
map_tree_species <- function(tree_table) {
  
  # Prepare species list found on the Acadian model to be merged with nfi data. We want the proper species code to use on OSM.
  acadian_splist_dt <- copy(acadian_species_list)
  setDT(acadian_splist_dt)
  acadian_gnsp_only <- acadian_splist_dt[, .(GENUS, SPECIES, OSM_AD_CmdKey)]
  acadian_gnsp_only[, `:=` (NFI_GENUS = toupper(substr(GENUS, 1, 4)),
                            NFI_SPECIES = toupper(substr(SPECIES, 1, 3))
  )]
  acadian_gnsp_only[, NFI_SPECIES := fifelse(SPECIES == "saccharum", "SAH", NFI_SPECIES)]
  
  species_columns <- c("tree_genus", "tree_species")
  
  # Step 1: Add Softwood/Hardwood GROUP info
  tree_table <- merge(tree_table,
                      unique(nfi_species[, .(GROUP, CODE_GENU, CODE_SPEC)]),
                      by.x = species_columns,
                      by.y = c("CODE_GENU", "CODE_SPEC"),
                      all.x = TRUE)
  
  # Step 2: Full genus + species match to OSM species list
  tree_table <- merge(tree_table,
                      acadian_gnsp_only[NFI_SPECIES != "SPP", .(NFI_GENUS, NFI_SPECIES, OSM_AD_CmdKey)],
                      by.x = species_columns,
                      by.y = c("NFI_GENUS", "NFI_SPECIES"),
                      all.x = TRUE)
  
  # Step 3: Fallback match on genus only where species match failed
  unmatched <- is.na(tree_table$OSM_AD_CmdKey)
  
  if (any(unmatched)) {
    genus_only_lookup <- acadian_gnsp_only[NFI_SPECIES == "SPP", .(NFI_GENUS, OSM_AD_CmdKey)]
    
    tree_table[unmatched, 
               OSM_AD_CmdKey := genus_only_lookup[.SD, 
                                                  on = .(NFI_GENUS = tree_genus),
                                                  x.OSM_AD_CmdKey]]
  }
  
  # Step 4: Final fallback if no genus match either → OS (Softwood), OH (Hardwood), or XX
  tree_table[, OSM_AD_CmdKey := fifelse(is.na(OSM_AD_CmdKey) & GROUP == "Softwood", "OS",
                                        fifelse(is.na(OSM_AD_CmdKey) & GROUP == "Hardwood", "OH",
                                                fifelse(is.na(OSM_AD_CmdKey), "XX", OSM_AD_CmdKey)))]
  
  return(tree_table)
}