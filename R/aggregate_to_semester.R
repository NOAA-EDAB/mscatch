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
#' @param nLenthSampels Numeric scalar. Number of length samples deemed to be "enough" for calculations. This is passed from \code{aggregate_landings}
#' @param pValue Numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#' @param outputDir Character string. Path to output directory (png files saved here)
#' @param logFile Character string. Specify the name for the log file generated describing all decisions made.
#'
#' @return List. Same as input data
#'
#' @noRd



aggregate_to_semester <- function(data,gearType,marketCode,SEMESTERData,missingEarlyYears,nLengthSamples,pValue,outputDir,logfile){

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

  # new SEMESTERData to reflect addition of early years
  SEMESTERData <- data$landings %>% dplyr::filter(NEGEAR == gearType & MARKET_CODE == marketCode)

  # determine which SEMESTERs have missing length samples
  missingSEMESTERs <- SEMESTERData %>% dplyr::group_by(YEAR,SEMESTER) %>% dplyr::summarize(numSamples = sum(len_numLengthSamples < nLengthSamples)) %>% dplyr::filter(numSamples >= nLengthSamples)
  #message("The following table shows YEAR/SEMESTER that have missing samples:")
  message(paste0("Gear: ",gearType," Market Code: ",marketCode,". We will borrow length data from the same SEMESTER in previous YEAR(s)."))


  if (nrow(missingSEMESTERs) != 0) {
    # cycle through the table of YEAR/SEMESTER combos
    for (iyear in 1:nrow(missingSEMESTERs)) {
      mainGear <- F
      # select same quarter in the previous year if not zero
      minYear <- min(data$lengthData$YEAR)
      numSamples <- missing_length_by_semester(SEMESTERData,missingSEMESTERs$YEAR[iyear],missingSEMESTERs$SEMESTER[iyear],nLengthSamples,minYear)
      if (dim(numSamples)[1]==0) { # no samples available
        message(paste0(missingSEMESTERs$YEAR[iyear]," - No lengthData available for SEMESTER = ",missingSEMESTERs$SEMESTER[iyear],". Using nearest neighbor."))
        # still zero after going back many years!! This could be a problem.
        write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingSEMESTERs$YEAR[iyear],"-",missingSEMESTERs$SEMESTER[iyear],". No samples found. Looking at nearest neighbor. - MARKET_CODE:",marketCode),label=NULL,append=T)
        # select length samples closest in time to target year/SEMESTER
        numSamples <- missing_length_by_semester_neighbor(SEMESTERData,missingSEMESTERs$YEAR[iyear],missingSEMESTERs$SEMESTER[iyear],nLengthSamples,outputDir,logfile)
        if (dim(numSamples)[1]==0) {
          # Still no samples, borrow from main Fleet in closest year. Presumably dealer still categorizes a small as a small
          # regardless of NEGEAR
          SEMESTERDataGear <- data$landings %>% dplyr::filter(MARKET_CODE == marketCode)
          numSamples <- missing_length_by_semester_neighbor(SEMESTERDataGear,missingSEMESTERs$YEAR[iyear],missingSEMESTERs$SEMESTER[iyear],nLengthSamples,outputDir,logfile)
          mainGearType <- numSamples$NEGEAR
          mainGear <- T
        }
        #stop("PROBLEM!!!. Finding zero length samples in all previous years")
      }

      # Update the data with length samples

      if (mainGear == T) {
        # update year/SEMESTER info with filled in data
        data <- update_length_samples(data,missingSEMESTERs[iyear,],gearType,marketCode,numSamples,mainGearType,TIME="SEMESTER")
        # write to logfile
        write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingSEMESTERs$YEAR[iyear],"-",missingSEMESTERs$SEMESTER[iyear]," used length samples from ",numSamples$YEAR,"-",numSamples$SEMESTER,"   - MARKET_CODE:",marketCode,". From NEGEAR = ",mainGearType),label=NULL,append=T)

      } else {
        # update year/SEMESTER info with filled in data
        data <- update_length_samples(data,missingSEMESTERs[iyear,],gearType,marketCode,numSamples,TIME="SEMESTER")
        # write to logfile
        write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingSEMESTERs$YEAR[iyear],"-",missingSEMESTERs$SEMESTER[iyear]," used length samples from ",numSamples$YEAR,"-",numSamples$SEMESTER,"   - MARKET_CODE:",marketCode),label=NULL,append=T)
      }
    }
  }

  return(data)
}
