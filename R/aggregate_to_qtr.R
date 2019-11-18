#' Missing QTR length samples are filled using previous years QTR
#'
#' Fills in missing length samples for YEAR/QTR combinations by using the previous YEARs value for the same QTR.
#' If this is also missing, then the previous YEARs value for the same QTR is used and so on.
#' For YEARs where there are landings before any length samples were taken all YEAR/QTRs are assigned the length samples
#' from the most recent YEAR where length samples were taken (from the same QTR)
#'
#' @param data List. Landings data and length data
#' @param gearType Character string. NEGEAR gear code
#' @param marketCode Character string. MARKET_CODE designation from cfdbs
#' @param QTRData Tibble. (n x 8). Filtered landings data by NEGEAR, MARKET_CODE, YEARs > earlyYears
#' @param missingEarlyYears Numeric vector. Years prior to first length sample was taken
#' @param nLenthSampels Numeric scalar. Number of length samples deemed to be "enough" for calculations. This is passed from \code{aggregate_landings}
#' @param pValue Numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#' @param outputDir Character string. Path to output directory (png files saved here)
#' @param logFile Character string. Specify the name for the log file generated describing all decisions made.
#'
#' @return List. Same as input data



aggregate_to_qtr <- function(data,gearType,marketCode,QTRData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile){

  # Deal with Early Years where we have landings data but no length samples
  # Repeat the length samples from the first year of sampling to all earlier years.
  # populate len_numLengthSamples in landings data
  for (iyear in missingEarlyYears) {
    for (iqtr in 1:4) {
      missingRow <- expand.grid(YEAR=iyear,QTR=iqtr)
      # check to see if any landings were recorded. if not then dont need lengths
      earlyData <- data$landings %>% dplyr::filter(YEAR == iyear & QTR == iqtr & NEGEAR == gearType & MARKET_CODE == marketCode )
      if (dim(earlyData)[1] == 0) { # not recorded landings
        next
      }
      numSamples <- update_early_years(QTRData,max(missingEarlyYears),iqtr,nLengthSamples)
      # numSamples <- QTRData %>% dplyr::filter(YEAR==(max(missingEarlyYears)+1) & QTR==iqtr) %>%
      #   dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)
      data <- update_length_samples(data,missingRow,gearType,marketCode,numSamples)

      write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",iyear,"-",iqtr," used length samples from ",numSamples$YEAR," - MARKET_CODE:",marketCode),label=NULL,append=T)
    }
  }

  # new QTRData to reflect addition of early years
  QTRData <- data$landings %>% dplyr::filter(NEGEAR == gearType & MARKET_CODE == marketCode)

  # determine which QTRs have missing length samples
  missingQTRs <- QTRData %>% dplyr::group_by(YEAR,QTR) %>% dplyr::summarize(numSamples = sum(len_numLengthSamples < nLengthSamples)) %>% dplyr::filter(numSamples >= nLengthSamples)
  #message("The following table shows YEAR/QTR that have missing samples:")
  message("We will borrow length data from the same QTR in previous YEAR(s).")
  #print(missingQTRs[,c(1:2)])
  # cycle through the table of YEAR/QTR combos
  for (iyear in 1:dim(missingQTRs)[1]) {
    mainGear <- F
    # select same quarter in the previous year if not zero
    minYear <- min(data$lengthData$YEAR)
    numSamples <- missing_length_by_qtr(QTRData,missingQTRs$YEAR[iyear],missingQTRs$QTR[iyear],nLengthSamples,minYear)
    if (dim(numSamples)[1]==0) {
      message(paste0(missingQTRs$YEAR[iyear]," - No lengthData available for QTR = ",missingQTRs$QTR[iyear],". Using nearest neighbor."))
      # still zero after going back many years!! This could be a problem.
      write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingQTRs$YEAR[iyear],"-",missingQTRs$QTR[iyear],". No samples found. Looking at nearest neighbor. - MARKET_CODE:",marketCode),label=NULL,append=T)
      # selet length samples closest in time to target year/qtr
      numSamples <- missing_length_by_qtr_neighbor(QTRData,missingQTRs$YEAR[iyear],missingQTRs$QTR[iyear],nLengthSamples)
      if (dim(numSamples)[1]==0) {
        # Still no samples, borrow from main Fleet in closest year. Presumably dealer still categorizes a small as a small
        # regardless of NEGEAR
        QTRDataGear <- data$landings %>% dplyr::filter(MARKET_CODE == marketCode)
#        QTRDataGear <- data$landings %>% dplyr::filter(NEGEAR == mainGearType & MARKET_CODE == marketCode)
        numSamples <- missing_length_by_qtr_neighbor(QTRDataGear,missingQTRs$YEAR[iyear],missingQTRs$QTR[iyear],nLengthSamples)
        mainGearType <- numSamples$NEGEAR
        mainGear <- T
      }
      #stop("PROBLEM!!!. Finding zero length samples in all previous years")
    }

    if (mainGear == T) {
      # update year/qtr info with filled in data
      data <- update_length_samples(data,missingQTRs[iyear,],gearType,marketCode,numSamples,mainGearType)
      # write to logfile
      write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingQTRs$YEAR[iyear],"-",missingQTRs$QTR[iyear]," used length samples from ",numSamples$YEAR,"-",numSamples$QTR,"   - MARKET_CODE:",marketCode,". From NEGEAR = ",mainGearType),label=NULL,append=T)

    } else {
      # update year/qtr info with filled in data
      data <- update_length_samples(data,missingQTRs[iyear,],gearType,marketCode,numSamples)
      # write to logfile
      write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingQTRs$YEAR[iyear],"-",missingQTRs$QTR[iyear]," used length samples from ",numSamples$YEAR,"-",numSamples$QTR,"   - MARKET_CODE:",marketCode),label=NULL,append=T)
    }
  }


  return(data)
}
