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
    speciesRules$temporalAggregation <- "semester"
    speciesRules$marketCodes <- data.frame(use = c("LG","SM","SM","SM","UN"),
                                           combine=c("LG","SQ","MD","PW","all"))
    speciesRules$gearCodes <- data.frame(use = c("050"),
                                         combine=c("all"))
    speciesRules$area <- c(520, 522:525, 541:543, 550:552, 560:562)
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightDataTime <- "all"
    speciesRules$LengthWeightRelationships <- c("semester")
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                  end = c())
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1973

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
    speciesRules$temporalAggregation <- "semester"
    speciesRules$marketCodes <- data.frame(use = c("LG","LG","LG","MD","SM","SM","SM","UN"),
                                           combine=c("LG","XG","JB","MD","SQ","ES","SV","all"))
    speciesRules$gearCodes <- data.frame(use = c("050"),
                                         combine=c("all"))
    speciesRules$area <- c(464:469, 500,510:515,520:526,530,533,534,537:543,550:552,
                           560:562,600,610:616,620:640,650,660,670,680,700:716,718,722,723,
                           730,732,736,739,745,746,798,799) # "000"?
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                  end = c())
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1992

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
    speciesRules$area <- c()
  }

  if (species_itis == 164744 ) { #Haddock
    speciesRules <- list()
    speciesRules$species_itis <- 164744
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$marketCodes <- data.frame(use = c("LG","LG","SR","SK","UN"),
                                           combine=c("LG","XG","SR","SK","all"))
    speciesRules$gearCodes <- data.frame(use = c("050"),
                                         combine=c("all"))
    speciesRules$area <- c(520:526, 530, 533, 534, 537:543, 550:552, 560:562,
                           600, 610:616, 620:640, 650, 660, 670, 680, 700)
  }

  if (species_itis == 172905 ) { #Winter flounder GB
    speciesRules <- list()
    speciesRules$species_itis <- 172905
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$temporalAggregation <- "quarter"
    speciesRules$marketCodes <- data.frame(use = c("LS","LS","LG","LG","SM","SM","SM","UN"),
                                           combine=c("LS","XG","LG","LM","SQ","MD","PW","all"))
    speciesRules$gearCodes <- data.frame(use="050",
                                         combine="all")
    speciesRules$area <- paste(c(522:525,542,453,551,552,561,562))
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1964
  }

  if (species_itis == 172905 ) { #Winter flounder SNEMA
    speciesRules <- list()
    speciesRules$species_itis <- 172905
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$marketCodes <- data.frame()
    speciesRules$gearCodes <- data.frame()
    speciesRules$area <- c()
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
