#' Missing QTR length samples are either borrowed or QTRs are combined.
#'
#' Fills in missing length samples by borrowing or no borrowing occures by QTRs are collapsed
#' (in most cases this will amount to the same thing)
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
#'
#' @noRd



aggregate_to_qtr2 <- function(data,howAggregate,gearType,marketCode,QTRData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile){

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

  # determine which QTRs have missing length samples
  missingQTRs <- QTRData %>%
    dplyr::filter(YEAR > max(missingEarlyYears)) %>%
    dplyr::group_by(YEAR,QTR) %>%
    dplyr::summarize(numSamples = sum(len_numLengthSamples < nLengthSamples)) %>%
    dplyr::filter(numSamples >= nLengthSamples)

print(missingQTRs)
  ###################### Either borrow or combine ################################


  if (howAggregate == "borrow") {
  # cycle through the table of YEAR/QTR combos
    if (nrow(missingQTRs) != 0) {

      message(paste0("Gear: ",gearType," Market Code: ",marketCode,". We will borrow length data from the nearest neighbour"))

      # Select type of nearest neighbour
      # cycle through the table of YEAR/SEMESTER combos
      for (iyear in 1:nrow(missingQTRs)) {

        # select length samples closest in time to target year/qtr
        numSamples <- missing_length_by_qtr_neighbor(QTRData,missingQTRs$YEAR[iyear],missingQTRs$QTR[iyear],nLengthSamples,outputDir,logfile)
      }

      if (nrow(numSamples) > 0) {

        # Update the data with length samples
        # update year/QRT info with filled in data
        data <- update_length_samples(data,missingQTRs[iyear,],gearType,marketCode,numSamples,TIME="QTR")
        # write to logfile
        write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingQTRs$YEAR[iyear],"-",missingQTRs$QTR[iyear]," used length samples from ",numSamples$YEAR,"-",numSamples$QTR,"   - MARKET_CODE:",marketCode),label=NULL,append=T)
      }
    }
  } else if (howAggregate == "combine") {
    # combine into year
    # step through each semester in turn
    if (nrow(missingQTRs) != 0) {

      message(paste0("Gear: ",gearType," Market Code: ",marketCode,". We will combine QTRs to SEMESTERs"))
      # cycle through the table of YEAR/QTR combos
      for (iyear in 1:nrow(missingQTRs)) {
        # select length samples closest in time to target year/SEMESTER
        targetYr <- missingQTRs$YEAR[iyear]
        print(c(targetYr))

        data <- missing_length_by_qtr_combine(data,targetYr,gearType,marketCode,combine="SEMESTER",outputDir,logfile)
        #data <- missing_length_by_qtr_combine(data,targetYr,gearType,marketCode,combine="YEAR",outputDir,logfile)

      }
      # Could have case where still have zero length samples at combined level. repeat and aggregate to year
      missingNewQTRs <- data$landings %>%
        dplyr::filter(MARKET_CODE == marketCode, NEGEAR == gearType) %>%
        dplyr::filter(YEAR > max(missingEarlyYears)) %>%
        dplyr::group_by(YEAR,QTR) %>%
        dplyr::summarize(numSamples = sum(len_numLengthSamples < nLengthSamples)) %>%
        dplyr::filter(numSamples >= nLengthSamples)

      if (nrow(missingNewQTRs) !=0 ) {
        write_to_logfile(outputDir,logfile,data=paste0("Samples at SEMESTER level still have too few samples. Aggregate to ANNUAL level"),label=NULL,append=T)
        message(paste0("Gear: ",gearType," Market Code: ",marketCode,". We will combine QTRs to YEARs"))
        # cycle through the table of YEAR/QTR combos
        for (iyear in 1:nrow(missingNewQTRs)) {
          # select length samples closest in time to target year/SEMESTER
          targetYr <- missingNewQTRs$YEAR[iyear]
          print(c(targetYr))

          data <- missing_length_by_qtr_combine(data,targetYr,gearType,marketCode,combine="YEAR",outputDir,logfile)

        }
      }




    }

  }


  return(data)
}
