#' Aggregate unclassifeid according to rules
#'
#' @param data List. Landings data and length data
#' @param variable Character string.
#' @param nLenthSamples Numeric scalar. Number of length samples deemed to be "enough" for calculations. This is passed from \code{aggregate_landings}
#' @param outputDir Character string. Path to output directory (png files saved here)
#' @param logFile Character string. Specify the name for the log file generated describing all decisions made.
#'
#'
#'@noRd


aggregate_unclassifieds <- function(data,variable,nLengthSamples,outputDir,logfile) {

  # When we go to expand unclasified. We have a problem if there are landings for "UN" but we
  # a. dont have any length samples. Cant expand
  #.b. dont have any landings for other market categories. Cant obtain a scaling factor
  # so we need to check for this prior to expanding


  # unclassified over NEGEAR and season (QTR)
  # select all cases where we have UNclassified landings but no/not enough length samples
  unclass <- data$landings %>%
    dplyr::filter(MARKET_CODE == "UN" & len_totalNumLen < nLengthSamples )  %>%
    dplyr::distinct(YEAR,.data[[variable]],NEGEAR)
  nUnclass <- dim(unclass)[1] # number of cases


  # for each row, select length distribution from master and expand
  for(irow in 1:nUnclass) {
    missingRow <- unclass[irow,]

    # pull all lengths for YEAR, QTR/SEMESTER, NEGEAR where MARKET CODE != "UN"
    lengthDist <- data$lengthData %>%
      dplyr::filter(YEAR == missingRow$YEAR & get(variable) == missingRow[[variable]] & NEGEAR == missingRow$NEGEAR & MARKET_CODE != "UN")
    # pull number of samples and number of lengths for YEAR, QTR/SEMESTER, NEGEAR where MARKET CODE != "UN"
    landDist <- data$landings %>%
      dplyr::filter(YEAR == missingRow$YEAR & get(variable) == missingRow[[variable]] & NEGEAR == missingRow$NEGEAR & MARKET_CODE != "UN")
    # no length samples for any market codes. Therefore cant obtain a scaling.
    # Have to find nearest neighbor where Unclassifieds have samples.
    if (nrow(lengthDist) == 0){
      message(paste0("Using nearest neighbor: Unclassified have no samples in ",missingRow$YEAR," by NEGEAR ",missingRow$NEGEAR," for any MARKET_CODE "  ))
      write_to_logfile(outputDir,logfile,data=paste0("Using nearest neighbor: Unclassified have no samples in ",missingRow$YEAR," by NEGEAR ",missingRow$NEGEAR," for any MARKET_CODE "),label=NULL,append=T)

      UNData <- data$landings %>% dplyr::filter(NEGEAR == missingRow$NEGEAR,MARKET_CODE=="UN")
      if (aggregate_to == "SEMESTER") {
        numSamples <- missing_length_by_semester_neighbor(UNData,missingRow$YEAR,missingRow[[variable]],nLengthSamples,outputDir,logfile)
      } else {
        numSamples <- missing_length_by_qtr_neighbor(UNData,missingRow$YEAR,missingRow[[variable]],nLengthSamples,outputDir,logfile)
      }
      if(nrow(numSamples) == 0){ # no samples for this gear Code
        stop(paste0("No samples for UNclassified for gear type = ",missingRow$NEGEAR))
      }

      data <- update_length_samples(data,missingRow,missingRow$NEGEAR,marketCode="UN",numSamples,mainGearType = NULL,TIME = variable)
      #readline(prompt = "Press [Enter] to continue ...")
    } else {
      # use length distribution of other market codes to assign to unclassifieds
      message(paste0("Unclassified have no samples in ",missingRow$YEAR,". Using all samples from this YEAR"))
      write_to_logfile(outputDir,logfile,data=paste0("Unclassified have no samples in ",missingRow$YEAR," for NEGEAR = ",missingRow$NEGEAR," . Using all samples from this YEAR"),label=NULL,append=T)

      newLengthData <- lengthDist %>%
        dplyr::group_by(YEAR,.data[[variable]],NEGEAR,LENGTH) %>%
        dplyr::summarize(NUMLEN = sum(NUMLEN),.groups="drop") %>%
        dplyr::mutate(MARKET_CODE = "UN") %>%
        dplyr::relocate(YEAR,.data[[variable]],NEGEAR,MARKET_CODE,LENGTH,NUMLEN)

      numSamples <- landDist %>%
        dplyr::group_by(YEAR,.data[[variable]],NEGEAR) %>%
        dplyr::summarise(len_totalNumLen = sum(len_totalNumLen),len_numLengthSamples=sum(len_numLengthSamples),.groups="drop")

      # pick out row with missing data (zero length samples) in LANDINGS
      ind <- (data$landings$YEAR == missingRow$YEAR) &
        (data$landings[[variable]] == missingRow[[variable]]) &
        (data$landings$NEGEAR == missingRow$NEGEAR) &
        (data$landings$MARKET_CODE == "UN")

      data$landings[ind,]$len_totalNumLen <- numSamples$len_totalNumLen
      data$landings[ind,]$len_numLengthSamples <- numSamples$len_numLengthSamples

      data$lengthData <- rbind(data$lengthData,newLengthData)

    }
  }


  return(data)


}
