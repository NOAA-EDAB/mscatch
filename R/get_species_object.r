#' Get species object
#'
#' Lists set of rules required for catch at length aggregation
#'
#'@param species_itis Numeric. Itis code for species (Default = NA, Returns a NA object for user to define)
#'@param stock Character string. Define stock region for species_itis
#'
#'@export

get_species_object <- function(species_itis = NULL, stock = NULL) {

  if (is.null(species_itis)) {
    speciesRules <- list()
    speciesRules$species_itis <- NA
    speciesRules$speciesName <- NA
    speciesRules$SVSPP <- NA
    speciesRules$temporalAggregation <- NA
    speciesRules$marketCodes <- NA
    speciesRules$gearCodes <- NA
    speciesRules$area <- NA
    speciesRules$LengthWeightData <- NA
    speciesRules$LengthWeightRelationships <- NA
    speciesRules$LengthWeightTimeBlocks <- NA
    speciesRules$AgeData <- NA
    speciesRules$AgeLengthKey <- NA
    speciesRules$startDate <- NA

  } else if (species_itis == 172909) { # Yellowtail
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

  } else if (species_itis == 172414 ) { #Mackerel
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

  } else if (species_itis == 161722 ) { #Herring
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
    speciesRules$temporalAggregation <- "quarter"
    speciesRules$marketCodes <- data.frame(use = c("UN"),
                                           combine=c("all"))
    speciesRules$gearCodes <- data.frame(use = c("050","050","050","050","050","050","050","050","998"),
                                         combine=c("050","051","056","058","121","132","170","370","all"))
    speciesRules$area <- "all"
    speciesRules$LengthWeightData <- "commercial"
    speciesRules$LengthWeightRelationships <- "same" # as aggregation
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester","gear")
    speciesRules$startDate <- 1965

  } else if (species_itis == 164744 ) { #Haddock
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
    speciesRules$temporalAggregation <- "semester"
    speciesRules$marketCodes <- data.frame(use = c("LG","LG","SR","SK","UN"),
                                           combine=c("LG","XG","SR","SK","all"))
    speciesRules$gearCodes <- data.frame(use = c("050"),
                                         combine=c("all"))
    speciesRules$area <- c(520:526, 530, 533, 534, 537:543, 550:552, 560:562,
                           600, 610:616, 620:640, 650, 660, 670, 680, 700)
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(2005),
                                                      end = c(2005))
    speciesRules$AgeData <- c()
    speciesRules$AgeLengthKey <- c()
    speciesRules$startDate <- 1964



  } else if (species_itis == 172905 & stock == "GB") { #Winter flounder GB
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
  } else if (species_itis == 172905 & stock == "SNEMA" ) { #Winter flounder SNEMA
    speciesRules <- list()
    speciesRules$species_itis <- 172905
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$temporalAggregation <- "semester"
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$marketCodes <- data.frame(use=c("LG","LG","LG","LG","MD","SM","SM","UN"),
                                           combine = c("LS","LG","XG","LM","MD","SQ","PW","all"))
    speciesRules$gearCodes <- data.frame(use="050",
                                         combine="all")
    speciesRules$area <- c(521,526,533:539,611:613)
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "single"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(1960),
                                                      end = c(2010))
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1982
  } else if (species_itis == 164712) { # Cod (GB).  East + West(561-562) regions
    speciesRules <- list()
    speciesRules$species_itis <- 164712
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$temporalAggregation <- "quarter"
    speciesRules$marketCodes <- data.frame(use=c("LG","LG","LG","MK","SK","SK","UN"),
                                           combine = c("LG","ST","WH","MK","SK","ST","all"))
    speciesRules$gearCodes <- data.frame(use = c("050"),
                                         combine= c("all"))
    speciesRules$area <- c(521,522,525,526,537:539,561,562)
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("semester")
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(1992),
                                                      end = c(2007))
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1981

  } else if (species_itis == 160617) { # spiny dog
    speciesRules <- list()
    speciesRules$species_itis <- 160617
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$temporalAggregation <- "annual"
    speciesRules$marketCodes <- data.frame(use= "UN",
                                           combine="all")
    speciesRules$gearCodes <- data.frame(use = c("050","998"),
                                         combine = c("050","all"))
    speciesRules$area <- "all" #???
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("sex")
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- NA
    speciesRules$AgeLengthKey <- NA
    speciesRules$startDate <- NA

  } else if (species_itis == 564145) { # winter skate
    speciesRules <- list()
    speciesRules$species_itis <- 564145
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$temporalAggregation <- NA
    speciesRules$marketCodes <- data.frame(use=c(),
                                           combine=c())
    speciesRules$gearCodes <- data.frame(use = c(),
                                         combine = c())
    speciesRules$area <- "all" #???
    speciesRules$LengthWeightData <- NA
    speciesRules$LengthWeightRelationships <- NA
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- NA
    speciesRules$AgeLengthKey <- NA
    speciesRules$startDate <- NA

  } else if (species_itis == 164791) { # silver hake
    speciesRules <- list()
    speciesRules$species_itis <- 164791
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$temporalAggregation <- "semester"
    speciesRules$marketCodes <- data.frame(use=c(),
                                           combine=c())
    speciesRules$gearCodes <- data.frame(use = c(),
                                         combine = c())
    speciesRules$area <- "all" #???
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("year","semester")
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(1973),
                                                      end = c(1979))
    speciesRules$AgeData <- "survey"
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1955

  } else if (species_itis == 164499) { # monkfish
    speciesRules <- list()
    speciesRules$species_itis <- 164499
    speciesRules$speciesName <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(COMMON_NAME.y) %>%
      dplyr::pull()
    speciesRules$SVSPP <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == speciesRules$species_itis) %>%
      dplyr::distinct(SVSPP) %>%
      dplyr::pull()
    speciesRules$temporalAggregation <- "semester"
    speciesRules$marketCodes <- data.frame(use=c(),
                                           combine=c())
    speciesRules$gearCodes <- data.frame(use = c(),
                                         combine = c())
    speciesRules$area <- "all" #???
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("year","semester")
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(1973),
                                                      end = c(1979))
    speciesRules$AgeData <- "survey"
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1955

  } else if (species_itis == 167687) { # black sea bass
    speciesRules <- list()
    speciesRules$species_itis <- 167687
    speciesRules$speciesName <- "BLACK SEA BASS"
    speciesRules$SVSPP <- 141
    speciesRules$temporalAggregation <- "semester"
    speciesRules$marketCodes <- data.frame(use = c("SM","SM","MD","LG","JB","JB","UN"),
                                           combine=c("SQ","ES","MD","LG","JB","XL","all"))
    speciesRules$gearCodes <- data.frame(use=c(rep("Trawl",4),
                                               rep("PotsTraps",7),
                                               rep("Gillnet",5),
                                               "Handline","Other"),
                                         combine=c("05","17","35","37",
                                                   "08","18","19","20","21","27","30",
                                                   "10","11","50","51","52",
                                                   "02","all"))
    if(is.null(stock)) {
      stop(paste0("Please enter a valid stock definition for ",speciesRules$speciesName,". Either North or South"))
    }
    if (stock == "North") {
      speciesRules$area <- paste(c(340,465,467,468,510:515,520:526,530,533,534,
                                   537:539,541:543,551,552,560:562,611:613, 616))
    } else if (stock == "South") {
      speciesRules$area <- paste(c(614:615,621:629,631:640))
    } else {
      stop(paste0("Please enter a valid stock definition for ",speciesRules$speciesName,". Either North or South"))
    }


    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- NA
  } else {
    stop(paste0("Not coded for species itis code =  ",species_itis, " with stock = ",stock))
  }


  return(speciesRules)

}

