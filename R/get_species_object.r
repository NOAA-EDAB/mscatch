#' Get species object
#'
#' Lists set of rules required for catch at length aggregation
#'
#'@param species_itis Numeric. Itis code for species (Default = NA, Returns a NA object for user to define)
#'@param stock Character string. Define stock region for species_itis (Default = NA)
#'
#'
#'@return A List
#'\item{species_itis}{6 digit species ITIS code}
#'\item{speciesName}{Species Name}
#'\item{SVSPP}{Species survey SVSPP code}
#'\item{temporalAggregation}{Level of temporal aggregation: "QTR","SEMESTER","YEAR"}
#'\item{marketCodes}{Data frame: mapping Market codes to use with market codes to combine}
#'\item{gearCodes}{Data frame: mapping gear codes to use with gear codes to combine}
#'\item{statStockArea}{Character array of statistical areas (3 characters)}
#'\item{howAggregate}{How to aggregate the landings data in absense of length samples: "borrow","combine"}
#'\item{LengthWeightData}{Where length-weight data comes from: "survey","commercial","both"}
#'\item{LengthWeightRelationships}{How to fit length-weight relationships. By: "semester","quarter"}
#'\item{LengthWeightTimeBlocks}{Time block over which to use for length weight data: "all", c(years)}
#'\item{AgeData}{Where Age data from from: "survey","commercial"}
#'\item{AgeLengthKey}{Variables over which to calculate age length key: "semester","qtr","gear"}
#'\item{startDate}{First year of assessment}
#'\item{stock}{Defines the stock, if more than one stock for a species:"GB","GOM","North","South}
#'
#'
#'@export

get_species_object <- function(species_itis = NULL, stock = NA) {

  if (is.null(species_itis)) {
    speciesRules <- list()
    speciesRules$species_itis <- NA
    speciesRules$speciesName <- NA
    speciesRules$SVSPP <- NA
    speciesRules$temporalAggregation <- NA
    speciesRules$marketCodes <- NA
    speciesRules$gearCodes <- NA
    speciesRules$statStockArea <- NA
    speciesRules$howAggregate <- NA
    speciesRules$LengthWeightData <- NA
    speciesRules$LengthWeightRelationships <- NA
    speciesRules$LengthWeightTimeBlocks <- NA
    speciesRules$AgeData <- NA
    speciesRules$AgeLengthKey <- NA
    speciesRules$startDate <- NA
    speciesRules$stock <- NA

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
    speciesRules$gearCodes <- data.frame(use = c("Other"),
                                         combine=c("all"))
    speciesRules$statStockArea <- c(520, 522:525, 541:543, 550:552, 560:562)
    speciesRules$howAggregate <- "combine"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightDataTime <- "all"
    speciesRules$LengthWeightRelationships <- c("semester")
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                  end = c())
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1973
    speciesRules$stock <- NULL


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
    speciesRules$gearCodes <- data.frame(use = c("pelagic"),
                                         combine=c("all"))
    speciesRules$statStockArea <- c(464:469, 500,510:515,520:526,530,533,534,537:543,550:552,
                           560:562,600,610:616,620:640,650,660,670,680,700:716,718,722,723,
                           730,732,736,739,745,746,798,799) # "000"?
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                  end = c())
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1992
    speciesRules$stock <- NULL


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
    speciesRules$gearCodes <- data.frame(use = c("pelagic","pelagic","pelagic","pelagic","pelagic","pelagic","pelagic","pelagic","fixedGear"),
                                         combine=c("050","051","056","058","121","132","170","370","all"))
    speciesRules$statStockArea <- c(430, 456, 464, 465, 467, 511:515, 521, 522,
                                    525, 526, 533, 534, 537:539, 542, 551, 552,
                                    561, 562, 611:616, 621:623, 625:628, 631,
                                    632, 635, 636, 640, 641, 642, 701,999)
    speciesRules$howAggregate <- "combine"
    speciesRules$LengthWeightData <- "commercial"
    speciesRules$LengthWeightRelationships <- "quarter"#c("year") #,"quarter","gear") # as aggregation
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester","gear")
    speciesRules$startDate <- 1965
    speciesRules$stock <- NULL


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
    speciesRules$statStockArea <- c(520:526, 530, 533, 534, 537:543, 550:552, 560:562,
                           600, 610:616, 620:640, 650, 660, 670, 680, 700)
    speciesRules$howAggregate <- "borrow" # assumed
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(2005),
                                                      end = c(2005))
    speciesRules$AgeData <- c()
    speciesRules$AgeLengthKey <- c()
    speciesRules$startDate <- 1964
    speciesRules$stock <- NULL




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
    speciesRules$statStockArea <- paste(c(522:525,542,543,551,552,561,562))
    speciesRules$howAggregate <- "combine" # assumed
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1964
    speciesRules$stock <- "GB"

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
    speciesRules$statStockArea <- c(521,526,533:539,611:613)
    speciesRules$howAggregate <- "combine"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "single"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(1960),
                                                      end = c(2010))
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1982
    speciesRules$stock <- "SNEMA"

  } else if (species_itis == 172905 & stock == "" ) { #Winter flounder dummy stock
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
    speciesRules$marketCodes <- data.frame(use = c("LS","LS","LG","LG","SM","SM","SM","UN"),
                                           combine=c("LS","XG","LG","LM","SQ","MD","PW","all"))
    speciesRules$gearCodes <- data.frame(use="demersal",
                                         combine="all")
    speciesRules$statStockArea <- c(521,522,525,526,537,538,551,552,561,562)
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "single"
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(1960),
                                                      end = c(2010))
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1982
    speciesRules$stock <- "SNEMA"


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
    speciesRules$statStockArea <- c(521,522,525,526,537:539,561,562)
    speciesRules$howAggregate <- "borrow" # assumed
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("semester")
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(1992),
                                                      end = c(2007))
    speciesRules$AgeData <- c("commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$startDate <- 1981
    speciesRules$stock <- NULL


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
    speciesRules$temporalAggregation <- "year"
    speciesRules$marketCodes <- data.frame(use= "UN",
                                           combine="all")
    speciesRules$gearCodes <- data.frame(use = c("fixedGear"),
                                         combine = c("all"))
    speciesRules$statStockArea <- c(200:699)
    speciesRules$howAggregate <- "borrow" #???
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "single"#c("sex")
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- NA
    speciesRules$AgeLengthKey <- NA
    speciesRules$startDate <- NA
    speciesRules$stock <- NULL


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
    speciesRules$temporalAggregation <- "quarter" # ???
    speciesRules$gearCodes <- data.frame(use=c("fixedGear"),
                                           combine=c("all"))
    speciesRules$marketCodes <- data.frame(use = "UN",
                                         combine = "all")
    speciesRules$statStockArea <- c(500:699) #???
    speciesRules$howAggregate <- NA
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- NA
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- NA
    speciesRules$AgeLengthKey <- NA
    speciesRules$startDate <- NA
    speciesRules$stock <- NULL


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

    if(is.null(stock)) {
      stop(paste0("Please enter a valid stock definition for ",speciesRules$speciesName,". Either North or South"))
    }

    if(stock == "North") {
      speciesRules$statStockArea <- c(511:515,521,522,551, 561)
      speciesRules$howAggregate <- "borrow"
      speciesRules$marketCodes <- data.frame(use=c("UN"),
                                             combine=c("all"))
      speciesRules$gearCodes <- data.frame(use = c("Trawl"),
                                           combine = c("all"))
      speciesRules$stock <- "North"
    } else if (stock == "South") {
      speciesRules$statStockArea <- c(525,526,533:539,541:543,
                                      552,562,611:639)
      speciesRules$howAggregate <- "borrow"
      speciesRules$marketCodes <- data.frame(use=c("UN"),
                                             combine=c("all"))
      speciesRules$gearCodes <- data.frame(use = c("Trawl"),
                                           combine = c("all"))
      speciesRules$stock <- "South"
    } else if (stock == "") {
      speciesRules$statStockArea <- c(521:526,538,551,552,561,562)

      speciesRules$howAggregate <- "borrow"
      speciesRules$marketCodes <- data.frame(use=c("UN"),
                                             combine=c("all"))
      speciesRules$gearCodes <- data.frame(use = c("Trawl"),
                                           combine = c("all"))
      speciesRules$stock <- "all"
    }  else {
      stop(paste0("Please enter a valid stock definition for ",speciesRules$speciesName,". Either North or South"))
    }

    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("semester")
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
    speciesRules$marketCodes <- data.frame(use = c("LG","SQ","SQ","UN"),
                                           combine = c("LG","SQ","PW","all"))
    speciesRules$gearCodes <- data.frame(use=c("Bottom","Bottom","Fixed"),
                                         combine=c("13","05","all"))
    speciesRules$statStockArea <- "all" #???
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester" # c("year","semester")
    speciesRules$LengthWeightTimeBlocks <- data.frame(start = c(),
                                                      end = c())
    speciesRules$AgeData <- "survey"
    speciesRules$AgeLengthKey <- c("semester")
    speciesRules$startDate <- NA
    speciesRules$stock <- NULL


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
      speciesRules$statStockArea <- paste(c(340,465,467,468,510:515,520:526,530,533,534,
                                   537:539,541:543,551,552,560:562,611:613, 616))
      speciesRules$howAggregate <- NA
      speciesRules$stock <- "North"

    } else if (stock == "South") {
      speciesRules$statStockArea <- paste(c(614:615,621:629,631:640))
      speciesRules$howAggregate <- NA
      speciesRules$stock <- "South"

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

