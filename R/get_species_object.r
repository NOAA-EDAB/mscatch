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
    speciesRules$area <- c("000","464","465","466","467","468","469","500","510","511","512","513","514","515","520","521","522","523","524","525","526","530","533","534","537","538",
                           "539","540","541","542","543","550","551","552","560","561","562","600","610","611","612","613","614","615","616","620","621","622","623","624","625","626",
                           "627","628","629","630","631","632","633","634","635","636","637","638","639","640","650","660","670","680","700","701","702","703","704","705","706","707",
                           "708","709","710","711","712","713","714","715","716","718","722","723","730","732","736","739","745","746","799")
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
