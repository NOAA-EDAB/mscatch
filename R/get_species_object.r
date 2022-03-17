#' Get species object
#'
#' Lists set of rules required for catch at length aggregation
#'
#'@param species_itis Numeric. Itis code for species
#'
#'@export

get_species_object <- function(species_itis) {


  if (species_itis == 172909) { # Yellowtail
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
    speciesRules$marketCodes <- data.frame(use = c("LG","SM","SM","SM","UN"),
                                           combine=c("LG","SQ","MD","PW","all"))
    speciesRules$gearCodes <- data.frame(use = c("050"),
                                         combine=c("all"))
  }

  if (species_itis == 172414 ) { #Mackerel
    speciesRules <- list()
    speciesRules$species_itis <- 172414
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$marketCodes <- data.frame(use = c("LG","LG","LG","MD","SM","SM","SM","UN"),
                                           combine=c("LG","XG","JB","MD","SQ","ES","SV","all"))
    speciesRules$gearCodes <- data.frame(use = c("050"),
                                         combine=c("all"))
  }

  if (species_itis == 161722 ) { #Herring
    speciesRules <- list()
    speciesRules$species_itis <- 161722
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$marketCodes <- data.frame(use = c("UN"),
                                           combine=c("all"))
    speciesRules$gearCodes <- data.frame(use = c("170","170","170","170","170","170","050"),
                                         combine=c("170","120","121","122","124","370","all"))
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
