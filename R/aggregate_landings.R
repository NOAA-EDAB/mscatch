#' Landings data aggregated based on length sample availability
#'
#'Landings data analysed to determine NEGEAR aggregation, MARKET_CODE descriptions analysed over time to determine aggregation all subject to
#'length sample availability. Output document created and a logfile to inform the user what steps were taken during aggregation
#'
#'All plotting functions will need to be finined/generalized
#'rmd file needs to be created to report all decisions
#'
#'@param landingsData Tidy data frame. Landings by YEAR,QTR,NEGEAR,MARKET_CODE,landings_land,landings_nn,len_totalNumLen,len_numLengthSampls
#'@param lengthData Tidy data frame. Length data by YEAR,QTR,NEGEAR,MARKET_CODE, LENGTH, NUMLEN
#'@param species_itis Numeric scalar. species_itis code relating to the landings and length data
#'@param landingsThresholdGear Numeric scalar (proportion). Minimum proportion of cumulative landings to avoid aggregation of gear. Default = .9
#'@param nLengthSamples Numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Default = 1
#'@param pValue Numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#'@param aggregate_to Character string. Level of aggregation for all MARKET_CODES and gears
#'@param proportionMissing Numeric scalar. Proportion of missing samples allowed per YEAR for each MARKET_CODE/GEAR combination). Default = 0.2
#'@param otherGear Character string. Code to indicate the class for "other Gear". This is the group of gear types that land the species of interest but in small numbers
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F (Default = F)
#'@param logFile Character string. Specify the name for the log file generated describing all decisions made.
#'
#'@importFrom dplyr "summarize" "summarise" "group_by" "filter" "select" "arrange" "mutate"
#'@importFrom magrittr "%>%"
#'
#' @return List of landings and associated length samples
#' \item{landings}{Tibble (n x 8). Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples) }
#' \item{lengthData}{Tibble (m x 8 ). Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)}
#'
#'@export

aggregate_landings <- function(landingsData,lengthData,species_itis,
                              landingsThresholdGear = .90, nLengthSamples = 1, pValue = 0.05, aggregate_to = "QTR",
                              proportionMissing = .2, otherGear,
                              outputDir=here::here("output"), outputPlots=F, logfile="logFile.txt") {


  write_to_logfile(outputDir,logfile,"",label="DECISIONS MADE DURING AGGREGATION OF DATA")
  write_to_logfile(outputDir,logfile,data=as.character(species_itis),label="Species_itis",append=T)

  # cleans landings data and length data of NAs
  landingsData <- landingsData %>% dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
      dplyr::summarise(landings_land=sum(landings_land, na.rm=T),landings_nn=sum(landings_nn, na.rm=T),len_totalNumLen=sum(len_totalNumLen,na.rm=T),len_numLengthSamples=sum(len_numLengthSamples, na.rm=T)) %>%
      dplyr::ungroup()
  lengthData <- lengthData %>% dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH) %>%
      dplyr::summarise(NUMLEN = sum(as.numeric(NUMLEN),na.rm=T)) %>%
      dplyr::ungroup()

  # create list for data.
  # landing and lengths will from this point on be parts of a list.
  data <- list()
  data$landings <- landingsData
  data$lengthData <- lengthData
  sampleStartYear <- min(as.numeric(unique(data$lengthData$YEAR)))
  numYears <- length(unique(data$landings$YEAR))
  #######################################################
  ####### GEARs #########################################
  #######################################################

  # Now deal with Gary's schematic.
  # 1. aggregate the gears based on landings

  data <- aggregate_gear(data,otherGear,landingsThresholdGear,species_itis,outputDir,outputPlots)
  gearList <- unique(data$landings$NEGEAR)
  # look at the summary stats/plots after aggregation
  summary_stats(data$landings,species_itis,outputDir,outputPlots)
  # take a look at length distribution of size categories
  plot_length_histogram(data$lengthData,species_itis,outputDir,outputPlots)

  #######################################################
  ####### MARKET CODES ##################################
  #######################################################

  # 2 . combine market category (This will be difficult) market category description are unique to species and not ordinal.
  # Use distributions to aggregate instead of Market category
  # Need to keep Unclassified and Unknown categories separate from known market categories
  # if market category has landings but no length data at all. Then the landings need to be lumped into a
  # neighboring size class. Very subjective but dont lump into unclassified/ unknown

  data <- aggregate_market_codes(data,pValue,outputDir,outputPlots,logfile)
  marketCodeList <- unique(data$landings$MARKET_CODE)
  #######################################################
  ####### QTR, SEMESTER, ANNUAL #########################
  #######################################################

  # 3. look at QTR to see if need to lump quarters or borrow from other years
  # plot all diagnostics again with current aggregated data
  plot_market_code_by_qtr(data,9,outputDir,outputPlots)
  write_to_logfile(outputDir,logfile,paste0("Length samples started in ",as.character(sampleStartYear),". All landings prior to this year will use this years data"),label=NULL,append = T)
  #write_to_logfile(outputDir,logfile,"Other gear (code 998) will be aggregated similarly to other gears",label="market code by qrt",append = T)

  # find set of years where samples were not taken but landings were. Early years
  landYrs <- unique(data$landings$YEAR)
  missingEarlyYears <- seq(min(landYrs),sampleStartYear-1)

  # Need to start in latest year and work backward, filling in for each gear type
  write_to_logfile(outputDir,logfile,data="",label="Length samples by QTR.  YEAR-QRT missing: YEAR-QTR used",append=T)
  yrsList <- unique(data$landings$YEAR) # full list of years in landings data

  # loop through NEGEAR / MARKET_CODE combinations to determine where to borrow length samples from
  # rules
  mainGearType <- gearList[1] #  need to process NEGEAR in order by decreasing landings
  for (gearType in gearList) { # loop over gear types
    for (marketCode in marketCodeList) { # loop over market category
      if (marketCode == "UN") next
      print(c(gearType,marketCode))
      # if (gearType == "100")
      #   return(data)
      # filter data by gear, market code and years where samples were taken
      QTRData <- data$landings %>% dplyr::filter(YEAR >= sampleStartYear & NEGEAR == gearType & MARKET_CODE == marketCode)
      # find number of times no samples seen by QTR and YEAR
      aggQTRData <- QTRData %>% dplyr::group_by(QTR) %>% dplyr::summarise(numSamples=sum(len_numLengthSamples < nLengthSamples))

      aggYEARData <- QTRData %>% dplyr::group_by(YEAR) %>% dplyr::summarize(totLand=sum(landings_land),numSamples=all(len_numLengthSamples < nLengthSamples))
      # determines if number of years missing data is small enough to borrow data from other years
      write_to_logfile(outputDir,logfile,data=paste0("NEGEAR: ",gearType," - MARKET_CODE: ",marketCode," - Approx : ",mean(aggQTRData$numSamples)," years have NO length samples - based on mean(QTRs)"),label=NULL,append=T)
      write_to_logfile(outputDir,logfile,data=paste0("NEGEAR: ",gearType," - MARKET_CODE: ",marketCode," - There are ",sum(aggYEARData$numSamples)," years out of ",length(aggYEARData$numSamples)," (in which there are landings) where no length samples exist - based on # YEARS"),label=NULL,append=T)

      numYearsLengthsStarted <-  length(unique(QTRData$YEAR))

      # deal with other gear differently since by definition may be sparse. aggregate to year may be prefereable
      if (gearType == otherGear) {
        # if (mean(aggQTRData$numSamples) < proportionMissing*numYearsLengthsStarted) {
        #   data <- aggregate_to_qtr(data,gearType,marketCode,QTRData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile)
        # } else {
          data <- aggregate_to_year(data,gearType,mainGearType,marketCode,aggYEARData,sampleStartYear,missingEarlyYears,proportionMissing,nLengthSamples,pValue,outputDir,logfile)
        # }
        next
      }

      # several choices from user.
      # 1. aggregate all to QTRs and borrow where necessary
      # 2. aggregate all to years and borrow where necessary
      # 3. a mix of both (need to work out how to expand unclassifieds)
      # 4. combine market categories prior to this point

      if (aggregate_to == "QTR") {
        data <- aggregate_to_qtr(data,gearType,mainGearType,marketCode,QTRData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile)
      } else if (aggregate_to == "YEAR") {
        data <- aggregate_to_year(data,gearType,mainGearType,marketCode,aggYEARData,sampleStartYear,missingEarlyYears,proportionMissing,nLengthSamples,pValue,outputDir,logfile)
      } else if (aggregate_to == "MIX") {

      ###################################################################################################
      # if mean number of missing years < specified tolerance
      if (mean(aggQTRData$numSamples) < proportionMissing*numYearsLengthsStarted) {

        # fill in missing QTRS using previous QTR(s): Borrow length sample data from previous QTR
        data <- aggregate_to_qtr(data,gearType,mainGearType,marketCode,QTRData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile)

      } else if (0) {
        # maybe add rules for semester aggregation if we can work out a plan

      } else  { # too many QTRs missing. aggregate the entire MARKET_CODE over QTR to annual data

        print(aggYEARData)
        if ((sum(aggYEARData$numSamples==1)/length(aggYEARData$numSamples)) > proportionMissing ){
          print(paste0("Aggregate over QTRS to YEARly-",marketCode))
          data <- aggregate_to_year(data,gearType,mainGearType,marketCode,aggYEARData,sampleStartYear,missingEarlyYears,proportionMissing,nLengthSamples,pValue,outputDir,logfile)
        } else {
          # need to aggregate MARKET_CODE over QTRs to YEAR
          stop("# need to aggregate MARKET_CODE over QTRs to YEAR")

        }

      }

      }
      ###################################################################################################

    } # marketCode
  } # gearType


  ## Aggregate UNclassified category for each gear type
  # We aggregate to the same level that the MARKET_CODE was aggregated. Either QTR or annual
  # for otherGear we always aggreagte to annual
  marketCode <- "UN"
  for (gearType in gearList) {
    conditionalOn <- rbind(c("NEGEAR",gearType),c("MARKET_CODE",marketCode))
    if (aggregate_to == "QTR") {
      # dont need to do anything already at QTR level
    } else if (aggregate_to == "YEAR") {
      for (iq in 1:4) {
        landingsData <- aggregate_data_by_class(data$landings,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="landings")
        data$landings <- landingsData
        lengthData <- aggregate_data_by_class(data$lengthData,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="lengths")
        data$lengthData <- lengthData
        write_to_logfile(outputDir,logfile,data=paste0("Gear: ",otherGear," - QTR ",iq," coded to 0 - MARKET_CODE:",marketCode),label=NULL,append=T)
      }
    } else {
      stop(paste0("ERROR: Not coded for this yet - aggregate_to = ",aggregate_to," in unclassifieds"))
    }

    if (gearType == otherGear) {
      # now deal with "other gear" category which should have market categories aggregated annually rather than by QTR.
      # By definition other gear category will have few landings and therefor aggregated to annual
      # we need to aggregate UN category to annual so we can expand
      conditionalOn <- rbind(c("NEGEAR",otherGear),c("MARKET_CODE",marketCode))
      for (iq in 1:4) {
        landingsData <- aggregate_data_by_class(data$landings,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="landings")
        data$landings <- landingsData
        lengthData <- aggregate_data_by_class(data$lengthData,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="lengths")
        data$lengthData <- lengthData
        write_to_logfile(outputDir,logfile,data=paste0("Gear: ",otherGear," - QTR ",iq," coded to 0 - MARKET_CODE:",marketCode),label=NULL,append=T)
      }
    }

  }

  # produce report on decisions made and include figures


  #  write_to_logfile(outputDir,logfile,"Other gear (code 998) will be aggregated similarly to other gears\n",label=NULL,append = T)
  message(paste0("Check the logfile = ",outputDir,"/",logfile," for details regarding aggregation choices") )



  return(data)


}
