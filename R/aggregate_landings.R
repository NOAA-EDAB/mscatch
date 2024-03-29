#' Landings data aggregated based on length sample availability
#'
#'Landings data analysed to determine NEGEAR aggregation, MARKET_CODE descriptions analysed over time to determine aggregation all subject to
#'length sample availability. Output document created and a logfile to inform the user what steps were taken during aggregation
#'
#'All plotting functions will need to be finined/generalized
#'rmd file needs to be created to report all decisions
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}.
#' This object is used to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#'@param landingsData Tidy data frame. Landings by YEAR,QTR,NEGEAR,MARKET_CODE,landings_land,landings_nn,len_totalNumLen,len_numLengthSampls
#'@param lengthData Tidy data frame. Length data by YEAR,QTR,NEGEAR,MARKET_CODE, LENGTH, NUMLEN
#'@param speciesName Character string. speciesName for data used. (This is used in plotting only)
#'@param landingsThresholdGear Numeric scalar (proportion). Minimum proportion of cumulative landings to avoid aggregation of gear. Default = .9
#'@param nLengthSamples Numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Default = 1
#'@param pValue Numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#'@param aggregate_to Character string. Level of aggregation for all MARKET_CODES and gears ("QTR", "YEAR", "SEMESTER", MIX").
#'Default = YEAR.
#'@param borrowLengths Boolean. Return data as is or use algorithm to borrow lengths from neighboring cells (Time and/or space)
#'@param proportionMissing Numeric scalar. Proportion of missing samples allowed per YEAR for each MARKET_CODE/GEAR combination). Default = 0.2
#'@param otherGear Character string. Code to indicate the class for "other Gear".
#'This is the group of gear types that land the species of interest but in small numbers Default = "998"
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F (Default = F)
#'@param logfile Character string. Specify the name for the log file generated describing all decisions made.
#'@param speciesRules List. Containing species specific rules. Default = NULL (Fully automated).
#'Note:  Predefined \code{speciesRules} will be bundled with the package for select species
#'
#'@importFrom dplyr "summarize" "summarise" "group_by" "filter" "select" "arrange" "mutate"
#'@importFrom magrittr "%>%"
#'
#' @return List of landings and associated length samples
#' \item{landings}{Tibble (n x 8). Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples) }
#' \item{lengthData}{Tibble (m x 8 ). Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)}
#'
#'@export

aggregate_landings <- function(channel,
                               landingsData,
                               lengthData,
                               speciesName = NULL,
                               landingsThresholdGear = .90,
                               nLengthSamples = 1,
                               pValue = 0.05,
                               aggregate_to = "YEAR",
                               borrowLengths = T,
                               proportionMissing = .2,
                               otherGear = "998",
                               outputDir=here::here("output"),
                               outputPlots=F,
                               logfile="logFile.txt",
                               speciesRules = NULL) {

  aggregate_to <- toupper(aggregate_to)
  if(aggregate_to == "QUARTER"){
    aggregate_to <- "QTR"
  }

  # write function call to log file
  write_to_logfile(outputDir,logfile,data=deparse(dbutils::capture_function_call()),label="Arguments passed to aggregate_landings:",append=F)


  if (is.null(speciesName) & is.null(speciesRules)){
    stop("Missing species Name: The species name must be present in EITHER the argument `speciesName` or `speciesRules$speciesName` ")
  } else if (!is.null(speciesName) & !is.null(speciesRules)) {
    stop("Multiple species Names: The species name must be present in EITHER the argument `speciesName` or `speciesRules$speciesName` ")
  } else if (is.null(speciesName)){
    speciesName <- speciesRules$speciesName
  }

  ## CHECKS QA/QC #################################################
  # Perform data checks, qa/qc and return summarized data as a list
  data <- checks_qa_qc(landingsData,lengthData,speciesName,aggregate_to,outputDir,logfile)

  speciesRules <- expand_species_rules(speciesRules,outputDir,logfile)

  # plot histogram of landings (and lengths) over time by QTR
  plot_landings_by_type(speciesRules$speciesName,data$landings,1,outputPlots,outputDir,"3a",type="QTR")
  plot_lengths_by_type(speciesRules$speciesName,data$landings,1,outputPlots,outputDir,"3c",type="QTR")


  # First year in which length samples were taken
  sampleStartYear <- min(as.numeric(unique(data$lengthData$YEAR)))
  numYears <- length(unique(data$landings$YEAR))


  ## GEARs #####################################################
  message("Aggregating by gear type ...")
  # 1. aggregate the gears based on landings

  if(is.null(speciesRules)) {
    # Fully automated. Gears are selected based on data.

    data <- aggregate_gear(data,otherGear,landingsThresholdGear,speciesName,logfile,outputDir,outputPlots)
    # compare length distributions across gear types
    data <- compare_gear_lengths(data,pValue,outputDir,logfile)
    # list of gears in ordered by landings
    gearList <- data$landings %>%
      dplyr::group_by(NEGEAR) %>%
      dplyr::summarise(totLand=sum(landings_land),.groups="drop") %>%
      dplyr::filter(NEGEAR != otherGear) %>%
      dplyr::arrange(desc(totLand)) %>%
      dplyr::select(NEGEAR) %>%
      dplyr::pull()
    gearList <- c(gearList,otherGear)
    mainGearType <- gearList[1]
  } else {
    # use user defined gear aggregation
    data <- aggregate_gear_rules(data,speciesRules,logfile,outputDir,outputPlots)
    gearList <- c(unique(data$landings$NEGEAR),otherGear)
    mainGearType <- gearList[1]
  }


  # obtain names of gear types and write to log file.
  # Requires access to Databases
  # gearNames <- comlandr::get_gears(channel,substr(head(gearList,-1),start=1,stop=2))$data %>%
  #   dplyr::select(NEGEAR,GEARNM) %>%
  #   dplyr::filter(NEGEAR %in% gearList) %>%
  #   dplyr::distinct()
  #
  # write_to_logfile(outputDir,logfile,data= gearNames,label="Gear Codes and description:",append=T)
  #
  #

  # take a look at length distribution of size categories
  plot_length_histogram(data$lengthData,outputDir,outputPlots)

  ## MARKET CODES ##################################

  # Combine market category
  # Use user defined aggregation or length distributions to aggregate instead of Market category
  # Need to keep Unclassified and Unknown categories separate from known market categories
  # if market category has landings but no length data at all. Then the landings need to be lumped into a
  # neighboring size class. Very subjective but don't lump into unclassified/ unknown

  # obtain market code descriptions and write to log file
  # Requires database access
  # marketCodeNames <- comlandr::get_species_itis(channel,speciesName)$data %>%
  #   dplyr::select(MARKET_CODE,MARKET_DESC) %>%
  #   dplyr::distinct()
  #
  # write_to_logfile(outputDir,logfile,data= marketCodeNames,label="Market Codes and description:",append=T)

  message("Aggregating by market code ...")

  if(is.null(speciesRules)) {
    # Fully automated. Market Codes selected based on data
    data <- aggregate_market_codes(data,speciesName,pValue,outputDir,outputPlots,logfile)
  } else {
    # use user defined gear aggregation
    data <- aggregate_market_codes_rules(data,speciesRules,outputDir,outputPlots,logfile)
  }

  marketCodeList <- unique(data$landings$MARKET_CODE)

  # aggregate over QTRs based on semester designation
  if (aggregate_to == "SEMESTER") {
    data$landings <- data$landings %>%
      dplyr::mutate(SEMESTER = dplyr::case_when(QTR %in% c(1,2) ~ 1, TRUE ~ 2)) %>%
      dplyr::select(-QTR) %>%
      dplyr::group_by(YEAR,NEGEAR,MARKET_CODE,SEMESTER) %>%
      dplyr::summarise(landings_land = sum(landings_land),
                    landings_nn = sum(landings_nn),
                    len_totalNumLen = sum(len_totalNumLen),
                    len_numLengthSamples  = sum(len_numLengthSamples),
                    .groups= "drop")
    data$lengthData <- data$lengthData %>%
      dplyr::mutate(SEMESTER = dplyr::case_when(QTR %in% c(1,2) ~ 1, TRUE ~ 2)) %>%
      dplyr::select(-QTR) %>%
      dplyr::group_by(YEAR,NEGEAR,MARKET_CODE,SEMESTER,LENGTH) %>%
      dplyr::summarise(NUMLEN = sum(NUMLEN),
                    .groups="drop")


  }
  if (aggregate_to != "QTR") {
    plot_landings_by_type(speciesRules$speciesName,data$landings,1,outputPlots,outputDir,"3b",type=aggregate_to)
    plot_lengths_by_type(speciesRules$speciesName,data$landings,1,outputPlots,outputDir,"3d",type=aggregate_to)
  }

  plot_market_code_by_time(data,9,outputDir,outputPlots,aggregate_to = aggregate_to)

  plot_length_sample_table(data,11,outputDir,outputPlots,aggregate_to = aggregate_to,speciesName)


  # Return data without any length borrowing
  #  This terminates the algorithm without any length borrowing.
  if (!(borrowLengths)) {
    data <- output_data(data,aggregate_to)
    write_to_logfile(outputDir,logfile,"",label="landings and length data returned without imputation",append=T)
    return(data)
  }

  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ## Interpolation/imputation starts here
  # QTR, SEMESTER, ANNUAL #############################################

  # 3. look at QTR to see if need to lump quarters or borrow from other years
  # plot all diagnostics again with current aggregated data

  write_to_logfile(outputDir,logfile,paste0("Length samples started in ",as.character(sampleStartYear),". All landings prior to this year will use this years data"),label=NULL,append = T)

  # find set of years where samples were not taken but landings were. Early years
  landYrs <- unique(data$landings$YEAR)
  missingEarlyYears <- seq(min(landYrs),sampleStartYear-1)

  # Need to start in latest year and work backward, filling in for each gear type
  write_to_logfile(outputDir,logfile,data="",label=paste0("Length samples by ",aggregate_to,"  YEAR-",aggregate_to," missing: YEAR-",aggregate_to," used"),append=T)
  yrsList <- unique(data$landings$YEAR) # full list of years in landings data


  # Write out table of length Samples by gear type similar to A13, p 78 of mackerel 2017 assessment

  # loop through NEGEAR / MARKET_CODE combinations to determine where to borrow length samples from
  # rules
  message("Borrowing/Imputation")
  print(gearList)
  print(marketCodeList)
  for (gearType in gearList) { # loop over gear types
    # Check to see if any gear types have zero length samples
    # If they do then aggregate with otherGear (May need an option to select gear type)
    zeroSamples <- data$landings %>% dplyr::filter(NEGEAR == gearType,len_numLengthSamples > 0)
    if ((nrow(zeroSamples) == 0) & (is.null(speciesRules))) {
      print(gearType)
      ## add to other gear
      landingsData <- aggregate_data_by_class(data$landings,variable="NEGEAR",classes=c(gearType,otherGear),conditionalOn=NULL,dataset="landings",aggregate_to)
      data$landings <- landingsData
      lengthData <- aggregate_data_by_class(data$lengthData,variable="NEGEAR",classes=c(gearType,otherGear),conditionalOn=NULL,dataset="lengths",aggregate_to)
      data$lengthData <- lengthData
      write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",aggregate_to," coded to: ",otherGear),label=NULL,append=T)
      next
    }

    for (marketCode in marketCodeList) { # loop over market category
      if (marketCode == "UN") next # deal with Unclassifieds differently

      # filter data by gear, market code and years where samples were taken
      samplesData <- data$landings %>%
        dplyr::filter(YEAR >= sampleStartYear & NEGEAR == gearType & MARKET_CODE == marketCode)
      # find number of times no samples seen by YEAR
      aggYEARData <- samplesData %>%
        dplyr::group_by(YEAR) %>%
        dplyr::summarize(totLand=sum(landings_land),numSamples=all(len_numLengthSamples < nLengthSamples))

      numYearsLengthsStarted <-  length(unique(samplesData$YEAR))

      # Do this always except when other gear and not null
      if (!((gearType == otherGear) & (!is.null(speciesRules)))) {
        print(c(marketCode,gearType))
        write_to_logfile(outputDir,logfile,data=paste0("NEGEAR: ",gearType," - MARKET_CODE: ",marketCode," - There are ",sum(aggYEARData$numSamples)," years out of ",length(aggYEARData$numSamples)," (in which there are landings) where no length samples exist - based on # YEARS"),label=NULL,append=T)

        if (nrow(samplesData) == 0) {
          message(paste0("No landings for NEGEAR = ",gearType,", MARKET_CODE = ",marketCode))
          next
        }
      }

      # deal with other gear differently since by definition may be sparse. aggregate to year may be preferable
      if (gearType == otherGear) {
        if (is.null(speciesRules)) {
        # if (mean(aggQTRData$numSamples) < proportionMissing*numYearsLengthsStarted) {
        #   data <- aggregate_to_qtr(data,gearType,marketCode,QTRData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile)
        # } else {
          data <- aggregate_to_year(data,gearType,gearList,marketCode,aggYEARData,sampleStartYear,missingEarlyYears,proportionMissing,nLengthSamples,pValue,outputDir,logfile,aggregate_to)
        # }
          next
        }
        next
      }


      ## Main aggregation

      # several choices from user.
      # 1. aggregate all to QTRs or SEMESTER and borrow where necessary
      # 2. aggregate all to years and borrow where necessary
      # 3. a mix of both (need to work out how to expand unclassifieds)


      if (aggregate_to == "QTR") {
        # find number of times no samples seen by QTR
        aggQTRData <- samplesData %>%
          dplyr::group_by(QTR) %>%
          dplyr::summarise(numSamples=sum(len_numLengthSamples < nLengthSamples))
        # determines if number of years missing data is small enough to borrow data from other years
        write_to_logfile(outputDir,logfile,data=paste0("NEGEAR: ",gearType," - MARKET_CODE: ",marketCode," - Approx : ",mean(aggQTRData$numSamples)," years have NO length samples - based on mean(QTRs)"),label=NULL,append=T)
        QTRData <- samplesData

        #data <- aggregate_to_qtr(data,gearType,marketCode,QTRData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile)
        data <- aggregate_to_qtr2(data,speciesRules$howAggregate,gearType,marketCode,QTRData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile)

      } else if (aggregate_to == "SEMESTER") {

        SEMESTERData <- data$landings %>%
          dplyr::filter(YEAR >= sampleStartYear & NEGEAR == gearType & MARKET_CODE == marketCode)

        # check if any length samples
        lengthSamplesPresent <- SEMESTERData %>% dplyr::filter(len_numLengthSamples>0)
print(nrow(lengthSamplesPresent))
        if (nrow(lengthSamplesPresent) == 0) {
          #recode to "UN"
          ll <- data$landings %>%
            dplyr::mutate(MARKET_CODE = dplyr::case_when(((NEGEAR == gearType) & (MARKET_CODE == marketCode)) ~ "UN", TRUE ~ MARKET_CODE))

          ## update landings (no need to update )
          data$landings <- ll %>%
            group_by(YEAR,SEMESTER,NEGEAR,MARKET_CODE) %>%
            summarize(landings_land=sum(landings_land,na.rm=T),
                      landings_nn=sum(landings_nn,na.rm=T),
                      len_totalNumLen= sum(len_totalNumLen,na.rm=T),
                      len_numLengthSamples=sum(len_numLengthSamples,na.rm=T),
                      .groups = "drop")

          write_to_logfile(outputDir,logfile,data=paste0("No length samples for market code = ",marketCode,". Renamed to UN"),label=NULL,append=T)

          next

        }

        #data <- aggregate_to_semester(data,gearType,marketCode,SEMESTERData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile)
        data <- aggregate_to_semester2(data,speciesRules$howAggregate,gearType,marketCode,SEMESTERData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile)

      } else if (aggregate_to == "YEAR") {

        data <- aggregate_to_year(data,gearType,gearList,marketCode,aggYEARData,sampleStartYear,missingEarlyYears,proportionMissing,nLengthSamples,pValue,outputDir,logfile,aggregate_to)

      }


    } # marketCode
  } # gearType


  # update gear list in case any gears were collapsed into otherGear
  gearList <- data$landings %>%
    dplyr::filter(NEGEAR != otherGear) %>%
    dplyr::distinct(NEGEAR) %>%
    dplyr::pull()
  gearList <- c(gearList,otherGear)


  # ############################################################################
  # ############################################################################
  # ############################################################################
  ## ############################### UNCLASSIFIEDS ##############################
  # ############################################################################
  # ############################################################################
  # ############################################################################

  ## Aggregate UNclassified category for each gear type
  # We aggregate to the same level that the MARKET_CODE was aggregated. Either QTR or annual
  # for otherGear we always aggregate to annual

  print("Unclassifieds")

  ## This sections needs to change
  if (aggregate_to %in% c("QTR","YEAR")) {
    variable <- "QTR"
  } else if(aggregate_to ==  "SEMESTER") {
    variable <- "SEMESTER"
  }


  marketCode <- "UN"
  for (gearType in gearList) {
    if (gearType == otherGear) {
      next
    }
    conditionalOn <- rbind(c("NEGEAR",gearType),c("MARKET_CODE",marketCode))

    if (aggregate_to == "QTR") {
      nTimeIntervals <- 4
      # dont need to do anything already at QTR level
    } else if (aggregate_to == "YEAR") {
      nTimeIntervals <- 4
      for (iq in 1:nTimeIntervals) {
        landingsData <- aggregate_data_by_class(data$landings,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="landings")
        data$landings <- landingsData
        lengthData <- aggregate_data_by_class(data$lengthData,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="lengths")
        data$lengthData <- lengthData
        write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - QTR ",iq," coded to 0 - MARKET_CODE:",marketCode),label=NULL,append=T)
      }
    } else if (aggregate_to == "SEMESTER"){
      nTimeIntervals <- 2
      # dont need to do anything already at SEMESTER level

      # for (iq in 1:nTimeIntervals) {
      #   landingsData <- aggregate_data_by_class(data$landings,variable="SEMESTER",classes=c(iq,0),conditionalOn=conditionalOn,dataset="landings",aggregate_to)
      #   data$landings <- landingsData
      #   lengthData <- aggregate_data_by_class(data$lengthData,variable="SEMESTER",classes=c(iq,0),conditionalOn=conditionalOn,dataset="lengths",aggregate_to)
      #   data$lengthData <- lengthData
      #   write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - SEMESTER ",iq," coded to 0 - MARKET_CODE:",marketCode),label=NULL,append=T)
      # }

    } else {
      stop(paste0("ERROR: Not coded for this yet - aggregate_to = ",aggregate_to," in unclassifieds"))
    }

  }


  # now deal with UNCLASSIFIEDS in the "other gear" category which should have
  # market categories aggregated annually rather than by QTR or SEMESTER.
  # By definition other gear category will have few landings and therefore aggregated to annual
  # we need to aggregate UN category to annual so we can expand
  # aggregate QTR/SEMESTER to annual. Code QTR = 0

  if(is.null(speciesRules)) {
    conditionalOn <- rbind(c("NEGEAR",otherGear),c("MARKET_CODE",marketCode))
    for (iq in 1:nTimeIntervals) {
      landingsData <- aggregate_data_by_class(data$landings,variable=variable,classes=c(iq,0),conditionalOn=conditionalOn,dataset="landings",aggregate_to)
      data$landings <- landingsData
      lengthData <- aggregate_data_by_class(data$lengthData,variable=variable,classes=c(iq,0),conditionalOn=conditionalOn,dataset="lengths",aggregate_to)
      data$lengthData <- lengthData
      write_to_logfile(outputDir,logfile,data=paste0("Gear: ",otherGear," - ",aggregate_to," ",iq," coded to 0 - MARKET_CODE:",marketCode),label=NULL,append=T)
    }
  }


  # When we go to expand unclasified. We have a problem if there are landings for "UN" but we
  # a. dont have any length samples. Cant expand
  #.b. dont have any landings for other market categories. Cant obtain a scaling factor
  # so we need to check for this prior to expanding

  data <- aggregate_unclassifieds(data,variable,nLengthSamples,aggregate_to,outputDir,logfile)

  # produce report on decisions made and include figures


  #  write_to_logfile(outputDir,logfile,"Other gear (code 998) will be aggregated similarly to other gears\n",label=NULL,append = T)
  message(paste0("Check the logfile = ",outputDir,"/",logfile," for details regarding aggregation choices") )


  if (aggregate_to == "SEMESTER") {

    data$landings <- data$landings %>%
      dplyr::rename(TIME=SEMESTER)
    data$lengthData <- data$lengthData %>%
      dplyr::rename(TIME=SEMESTER)

  } else if ((aggregate_to == "QTR") | (aggregate_to == "YEAR")) {

    data$landings <- data$landings %>%
      dplyr::rename(TIME=QTR)
    data$lengthData <- data$lengthData %>%
      dplyr::rename(TIME=QTR)
  } else {
    stop(paste0("Not coded for ",aggregate_to))
  }

  return(data)


}
