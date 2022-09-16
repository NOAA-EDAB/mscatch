#' Get species object
#'
#' Lists set of rules required for catch at length aggregation
#'
#'@param species_itis Numeric. Itis code for species (Default = NA, Returns a NA object for user to define)
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

get_species_object_mskeyrun <- function(species_itis = NULL) {

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
    speciesRules$gearCodes <- data.frame(use = c("demersal"),
                                         combine=c("all"))
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537)
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("semester")
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$maxAge <- 8
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
    speciesRules$gearCodes <- data.frame(use = c("demersal","pelagic","pelagic","pelagic","fixedGear"),
                                         combine=c("05","17","37","12","all"))
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537) # "000"?
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester"
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$maxAge <- 10
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
    speciesRules$gearCodes <- data.frame(use = c("pelagic"),
                                         combine=c("all"))
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537)
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "quarter"#c("year") #,"quarter","gear") # as aggregation
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$maxAge <- 8
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
    speciesRules$gearCodes <- data.frame(use = c("demersal","demersal","fixedGear"),
                                         combine=c("05","13","all"))
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537)
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("semester")
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$maxAge <- 15
    speciesRules$stock <- NULL


  } else if (species_itis == 172905) { #Winter flounder dummy stock
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
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537)
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "single"
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$maxAge <- 7
    speciesRules$stock <- NULL


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
    speciesRules$gearCodes <- data.frame(use = c("demersal","demersal","demersal","demersal","fixedGear"),
                                         combine= c("05","16","36","13","all"))
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537)
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("semester")
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$maxAge <- 15
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
    speciesRules$gearCodes <- data.frame(use = c("fixedGear","fixedGear","fixedGear","fixedGear","fixedGear","demersal"),
                                         combine = c("10","01","02","50","52","all"))
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537)
    speciesRules$howAggregate <- "borrow" #???
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "single"#c("sex")
    speciesRules$AgeData <- NA
    speciesRules$AgeLengthKey <- NA
    speciesRules$maxAge <- NA
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
    speciesRules$temporalAggregation <- "semester" # ???
    speciesRules$gearCodes <- data.frame(use=c("demersal"),
                                           combine=c("all"))
    speciesRules$marketCodes <- data.frame(use = "UN",
                                         combine = "all")
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537)#???
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "single"
    speciesRules$AgeData <- NA
    speciesRules$AgeLengthKey <- NA
    speciesRules$maxAge <- NA
    speciesRules$stock <- NULL


  } else if (species_itis == 164791) { # silver hake dummy stock
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
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537)
    speciesRules$howAggregate <- "borrow"
    speciesRules$marketCodes <- data.frame(use=c("UN"),
                                           combine=c("all"))
    speciesRules$gearCodes <- data.frame(use = c("demersal"),
                                         combine = c("all"))

    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- c("semester")
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$maxAge <- 8
    speciesRules$stock <- NULL

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
    speciesRules$gearCodes <- data.frame(use=c("demersal","demersal","fixedGear"),
                                         combine=c("13","05","all"))
    speciesRules$statStockArea <- c(521,522,523,524,525,526,538,551,552,561,562,537)
    speciesRules$howAggregate <- "borrow"
    speciesRules$LengthWeightData <- "survey"
    speciesRules$LengthWeightRelationships <- "semester" # c("year","semester")
    speciesRules$AgeData <- c("survey","commerical")
    speciesRules$AgeLengthKey <- c("year","semester")
    speciesRules$maxAge <- 10
    speciesRules$stock <- NULL



  } else {
    stop(paste0("Not coded for species itis code =  ",species_itis, " with stock = ",stock))
  }


  return(speciesRules)

}

