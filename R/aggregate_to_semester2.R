#' Missing SEMESTER length samples are filled using previous years SEMESTER
#'
#' Fills in missing length samples for YEAR/SEMESTER combinations by using the previous YEARs value for the same SEMESTER
#' If this is also missing, then the previous YEARs value for the same SEMESTER is used and so on.
#' For YEARs where there are landings before any length samples were taken all YEAR/SEMESTERs are assigned the length samples
#' from the most recent YEAR where length samples were taken (from the same SEMESTER)
#'
#' @param data List. Landings data and length data
#' @param gearType Character string. NEGEAR gear code
#' @param marketCode Character string. MARKET_CODE designation from cfdbs
#' @param SEMESTERData Tibble. (n x 8). Filtered landings data by NEGEAR, MARKET_CODE, YEARs > earlyYears
#' @param missingEarlyYears Numeric vector. Years prior to first length sample was taken
#' @param nLenthSamples Numeric scalar. Number of length samples deemed to be "enough" for calculations. This is passed from \code{aggregate_landings}
#' @param pValue Numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#' @param outputDir Character string. Path to output directory (png files saved here)
#' @param logFile Character string. Specify the name for the log file generated describing all decisions made.
#'
#' @return List. Same as input data
#'
#' @noRd



aggregate_to_semester2 <- function(data,howAggregate,gearType,marketCode,SEMESTERData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile){

  # Deal with Early Years where we have landings data but no length samples
  # Repeat the length samples from the first year of sampling to all earlier years.
  # populate len_numLengthSamples in landings data
  for (iyear in missingEarlyYears) {
    for (isem in 1:2) {
      missingRow <- expand.grid(YEAR=iyear,SEMESTER=isem)
      # check to see if any landings were recorded. if not then dont need lengths
      earlyData <- data$landings %>% dplyr::filter(YEAR == iyear & SEMESTER == isem & NEGEAR == gearType & MARKET_CODE == marketCode )
      if (dim(earlyData)[1] == 0) { # not recorded landings
        next
      }
      numSamples <- update_early_years_semester(SEMESTERData,max(missingEarlyYears),isem,nLengthSamples)
      data <- update_length_samples(data,missingRow,gearType,marketCode,numSamples,TIME="SEMESTER")

      write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",iyear,"-",isem," used length samples from ",numSamples$YEAR," - MARKET_CODE:",marketCode),label=NULL,append=T)
    }
  }

  # determine which SEMESTERs have missing/not enough length samples
  missingSEMESTERs <- SEMESTERData %>%
    dplyr::filter(YEAR > max(missingEarlyYears)) %>%
    dplyr::group_by(YEAR,SEMESTER) %>%
    dplyr::summarize(numSamples = sum(len_numLengthSamples < nLengthSamples)) %>%
    dplyr::filter(numSamples >= nLengthSamples)


  ###################### Either borrow or combine ################################

  # Borrow length samples
  if (howAggregate == "borrow") {
    # step through each semester in turn
    if (nrow(missingSEMESTERs) != 0) {

      message(paste0("Gear: ",gearType," Market Code: ",marketCode,". We will borrow length data from the nearest neighbour"))
      # Select type of nearest neighbour
      # cycle through the table of YEAR/SEMESTER combos
      for (iyear in 1:nrow(missingSEMESTERs)) {
        row <- missingSEMESTERs[iyear,]

        # select length samples closest in time to target year/SEMESTER
        numSamples <- missing_length_by_semester_neighbor(SEMESTERData,missingSEMESTERs$YEAR[iyear],missingSEMESTERs$SEMESTER[iyear],nLengthSamples,outputDir,logfile)
      }

      if (nrow(numSamples) > 0) {

        # Update the data with length samples
        # update year/SEMESTER info with filled in data
        data <- update_length_samples(data,missingSEMESTERs[iyear,],gearType,marketCode,numSamples,TIME="SEMESTER")
        # write to logfile
        write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingSEMESTERs$YEAR[iyear],"-",missingSEMESTERs$SEMESTER[iyear]," used length samples from ",numSamples$YEAR,"-",numSamples$SEMESTER,"   - MARKET_CODE:",marketCode),label=NULL,append=T)
      }
    }

  } else if (howAggregate == "combine"){

    # combine into year
    # step through each semester in turn
    if (nrow(missingSEMESTERs) != 0) {

      message(paste0("Gear: ",gearType," Market Code: ",marketCode,". We will combine SEMESTERS to YEAR"))
      # cycle through the table of YEAR/SEMESTER combos
      for (iyear in 1:nrow(missingSEMESTERs)) {
        # select length samples closest in time to target year/SEMESTER
        targetYr <- missingSEMESTERs$YEAR[iyear]
        print(targetYr)

        data <- missing_length_by_semester_combine(data,targetYr,gearType,marketCode,outputDir,logfile)

        # Could have case where still have zero length samples.

      }

    }
  }



  return(data)
}
