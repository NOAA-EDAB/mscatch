#' Missing QTR length samples are filled using previous years QTR
#'
#'
#'
#'
#'
#'@export

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
  # determine which QTRs have missing length samples
  missingQTRs <- QTRData %>% dplyr::group_by(YEAR,QTR) %>% dplyr::summarize(numSamples = sum(len_numLengthSamples < nLengthSamples)) %>% dplyr::filter(numSamples >= nLengthSamples)
  message("The following table shows YEAR/QTR that have missing samples:")
  message("We will borrow length data from the same QTR in the previous YEAR.")
  print(missingQTRs[,c(1:2)])
  # cycle through the table of YEAR/QTR combos
  for (iyear in 1:dim(missingQTRs)[1]) {
    # select same quarter in the previous year if not zero
    numSamples <- missing_length_by_qtr(QTRData,missingQTRs$YEAR[iyear],missingQTRs$QTR[iyear],nLengthSamples)
    if (numSamples$len_numLengthSamples < nLengthSamples) {
      # still zero after going back many years!! This could be a problem.
      stop("PROBLEM!!!. Finding zero length samples in all previous years")
    }
    print(missingQTRs$YEAR[ iyear])
    print(numSamples)
    # update year/qtr info with filled in data
    data <- update_length_samples(data,missingQTRs[iyear,],gearType,marketCode,numSamples)
    # write to logfile
    write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingQTRs$YEAR[iyear],"-",missingQTRs$QTR[iyear]," used length samples from ",numSamples$YEAR,"-",numSamples$QTR,"   - MARKET_CODE:",marketCode),label=NULL,append=T)
  }


  return(data)
}
