#' Aggregate data to YEAR and borrow length samples from other years
#'
#'If the number of length samples are too sparse at NEGEAR/MARKET_CODE level to retain at QTR year then landings are aggregated
#'to YEAR level. YEARs with missing length samples are filled with length sample data from the closest YEAR
#'
#'
#' @param data List. Landings data and length data
#' @param gearType Character string. NEGEAR gear code
#' @param gearList Character vector. Vector of NEGEARs in decreasing order by landings. (eg. gearList[1] is NEGEAR that landed most fish)
#' @param marketCode Character string. MARKET_CODE designation from cfdbs
#' @param aggYearData Tibble. (n x 3). YEAR, total landings, presence/absence of min mumber of samples
#' @param sampleStartYear Numeric scalar. Year first length sample was taken
#' @param missingEarlyYears Numeric vector. Years prior to first length sample was taken
#' @param proportionMissing numeric scalar. Proportion of missing samples allowed per YEAR for each MARKET_CODE/GEAR combination). Passed from \code{aggregate_landings}
#' @param nLenthSamples Numeric scalar. Number of length samples deemed to be "enough" for calculations. This is passed from \code{aggregate_landings}
#' @param pValue Numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#' @param outputDir Character string. Path to output directory (png files saved here)
#' @param logFile Character string. Specify the name for the log file generated describing all decisions made.
#'
#' @return List. Same as input data
#'
#'@noRd


aggregate_to_year <- function(data,gearType,gearList,marketCode,aggYEARData,sampleStartYear,missingEarlyYears,proportionMissing,nLengthSamples,pValue,outputDir,logfile) {

    # recode all QTRS to 0
    foundGear <- F
    # Too many YEARs without length data for MARKET_CODE. Aggregate with another MARKET_CODE
    message("Annual data: There are ",sum(aggYEARData$numSamples)," years out of ",length(aggYEARData$numSamples)," (in which there are landings) where no length samples exist")
    #message("Insufficient length samples at aggregate level: YEAR")
    summarizedData <- data$landings %>%
      dplyr::group_by(MARKET_CODE) %>%
      dplyr::filter(NEGEAR == gearType) %>%
      dplyr::summarise(totalLandings = sum(landings_land, na.rm = TRUE),len_numLengthSamples=sum(len_numLengthSamples,na.rm=T)) %>%
      dplyr::arrange(desc(totalLandings))
    #print(summarizedData)

    # do ks test to help make a decision. compare all length distributions of this year
    landings <- data$landings %>% dplyr::filter(NEGEAR==gearType)
    lengthData <- data$lengthData %>% dplyr::filter(NEGEAR==gearType)

    # Future work. Length distribution comparisons on a year by year basis
    while(0){
      codesToAggregate <- compare_length_distributions(landings,lengthData,variableToAggregate = "MARKET_CODE", groupBy=c("LENGTH","NUMLEN","MARKET_CODE"), pValue,outputDir,logfile)

      # plot number of samples by year
      if (!is.null(codesToAggregate)) {
        # we need to aggregate size classes
        message("length distributions (by MARKET_CODE for gear = ",gearType,") are NOT significantly different. You CAN aggregate based on lengths.")
      } else {
        message("length distributions (by MARKET_CODE for gear = ",gearType,") are significantly different. Can not aggregate based on lengths.")
        write_to_logfile(outputDir,logfile,data=paste0("Length distributions (by MARKET_CODE for gear = ",gearType,") are significantly different. Can not aggregate based on lengths."),label=NULL,append=T)
      }
    }

    message("1. Grab missing lengths from previous years or ")
    message("2. aggregate MARKET_CODE")
    #options <- readline(prompt=paste0("There are 2 options for NEGEAR = ",gearType,": MARKET_CODE = ",marketCode, ". Enter 1 or 2: "))
    message("There are 2 options for NEGEAR = ",gearType,": MARKET_CODE = ",marketCode, ". Enter 1 or 2: Hard Coded: 1")
    options <- 1
    if (options == 2) {
      newCode <- readline(prompt=paste0("Gear = ",gearType,": Which Market category would you like to combine ",marketCode, " with: "))
      message(paste0("OK. We will combine ",sum(data$landings$MARKET_CODE == marketCode)," records for ",marketCode, " with ",newCode))
      # aggregate again. This takes care or early years also
      filteredLandings <- aggregate_data_by_class(data$landings,variable="MARKET_CODE",classes=c(marketCode,newCode),conditionalOn=c("NEGEAR",gearType),dataset="landings")
      data$landings <- filteredLandings
      lengthData <- aggregate_data_by_class(data$lengthData,variable="MARKET_CODE",classes=c(marketCode,newCode),conditionalOn=c("NEGEAR",gearType),dataset="lengths")
      data$lengthData <- lengthData
      write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",paste0(c(marketCode,newCode),collapse=" to ")),label="market code relabelling, (lack of length samples at QTR & YEAR level) from:to",append=T)

    } else if (options == 1) {
      # fill in using previous years lengths
      # aggregate over QTR to YEAR for NEGEAR,MARKET_CODE. plot/test length over time
      # code all QTRs to 0 . This is essentially Annual data
      write_to_logfile(outputDir,logfile,data=paste0("Aggregate over QRTs to Annual data"),label=NULL,append=T)

      conditionalOn <- rbind(c("NEGEAR",gearType),c("MARKET_CODE",marketCode))

      # aggregate QTR to annual. Code QTR = 0
      for (iq in 1:4) {
        filteredLandings <- aggregate_data_by_class(data$landings,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="landings")
        data$landings <- filteredLandings
        lengthData <- aggregate_data_by_class(data$lengthData,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="lengths")
        data$lengthData <- lengthData
        write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - QTR ",iq," codes to 0 - MARKET_CODE:",marketCode),label=NULL,append=T)
      }

      YEARData <- data$landings %>%
        dplyr::filter(YEAR >= sampleStartYear & QTR == 0 & NEGEAR == gearType & MARKET_CODE == marketCode)

      if (sum(YEARData$len_numLengthSamples) < nLengthSamples){
        # not enough length Samples. Borrow from other fleet
        message("No length samples present for this market code. Borrow from other fleet")
        # loop through all gear types ordered by most landings from most
        for (borrowGear in gearList) {
          if (borrowGear == gearType) {
            next # current gear is main gear
          }
          message(paste0("Trying to borrow from ", borrowGear))
          # pull landings for gear we want samples from
          YEARData <- data$landings %>%
            dplyr::filter(YEAR >= sampleStartYear & NEGEAR == borrowGear & MARKET_CODE == marketCode)
          # check to see if there are samples
          if (sum(YEARData$len_numLengthSamples) >= nLengthSamples) {
            # Found a gear that has length samples
            write_to_logfile(outputDir,logfile,data=paste0("Using length samples from Gear: ",borrowGear),label=NULL,append=T)
            foundGear <- T
            foundGearType <- borrowGear
            break
          }
        }

        if (!foundGear) {
          stop("ERROR: Could not find another gear with length samples")
        }

      }

      # For each year with missing length find closest year with length samples
      missingYrs <- aggYEARData %>% dplyr::filter(numSamples == T)
      if (nrow(missingYrs) > 0) {
        for (iyear in 1:dim(missingYrs)[1]) {
          targetYEAR <- missingYrs$YEAR[iyear]
          numSamples <- missing_length_by_year(YEARData,targetYEAR,nLengthSamples)
          if (dim(numSamples)[1] == 0){
            # no length samples present for this market code
            # This should never happen. Should have errored before now
            message("NO LENGTH SAMPLES - ERROR")
          } else if (dim(numSamples)[1] > 1){
            numSamples <- head(numSamples %>% dplyr::arrange(len_numLengthSamples),1)
          }

          #duplicate length samples to missing year
          missingRow <- expand.grid(YEAR=targetYEAR,QTR=0)
          if (foundGear) { # used samples from another gear type
            data <- update_length_samples(data,missingRow,gearType,marketCode,numSamples,foundGearType)
          } else { # used samples from own gear type
            data <- update_length_samples(data,missingRow,gearType,marketCode,numSamples)
          }

          write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",targetYEAR," used length samples from ",numSamples$YEAR," - MARKET_CODE:",marketCode),label=NULL,append=T)
        }
      }

      # now deal with Early Years where we have landings data but no length samples were taken
      # repeat the length samples from the first year of sampling to all earlier years.
      # populate len_numLengthSamples in landings data

      # updated YEARData ow lengths have been filled in
      YEARData <- data$landings %>%
        dplyr::filter(YEAR >= sampleStartYear & QTR == 0 & NEGEAR == gearType & MARKET_CODE == marketCode)

      for (iyear in missingEarlyYears) {
        for (iqtr in 0) {
          missingRow <- expand.grid(YEAR=iyear,QTR=iqtr)

          # check to see if any landings were recorded. if not then dont need lengths
          earlyData <- data$landings %>% dplyr::filter(YEAR == iyear & QTR == iqtr & NEGEAR == gearType & MARKET_CODE == marketCode )
          if (dim(earlyData)[1] == 0) { # not recorded landings
            next
          }
          numSamples <- update_early_years(YEARData,max(missingEarlyYears),iqtr,nLengthSamples)
          # numSamples <- QTRData %>% dplyr::filter(YEAR==(max(missingEarlyYears)+1) & QTR==iqtr) %>%
          #   dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)
          data <- update_length_samples(data,missingRow,gearType,marketCode,numSamples)

          write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",iyear,"-",iqtr," used length samples from ",numSamples$YEAR," - MARKET_CODE:",marketCode),label=NULL,append=T)
        }

      }

    }

  return(data)

}
