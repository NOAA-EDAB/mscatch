#' Get species object
#'
#' Lists set of rules required for catch at length aggregation
#'
#'@param species_itis Numeric. Itis code for species
#'
#'@export

get_species_object <- function(species_itis) {


  if (species_itis == 172909) {
    speciesRules <- list()
    speciesRules$species_itis <- 172909
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$marketCodes <- data.frame(use = c("LG","S","S","S"),combine=c("LG","SQ","MD","PW"))
    speciesRules$gearCodes <- data.frame(use = c("050"),combine=c("all"))
  }


  return(speciesRules)

}

#
# Age Length Data:      Comland
# Footprint:            GB: 522,525,551,552,561,562
# Market Code Grouping: LG, S(SQ,MD,PW) , UN(X1,X2,X3,MX)
# Gear Types(codes):    None
# Aggregate to:         SEMESTER
# Borrowing Lengths:    None
# Length Weight data:   Survey
# Length Weight fit:    SEMESTER
# Catch at age:         YEAR, SEMESTER?
#
