#' Function to fill in missing sample lengths by year
#'
#' Obtain the YEAR length samples to be used for missing YEAR data
#' Assumption is the data has been aggregated to YEAR from QTR.
#' The closest year that contains length samples is chosen
#'
#'@param YEARData dataframe. YEAR and number of length samples for the year
#'@param targetYear numeric scalar. Year with missing length samples
#'@param minNumSamples numeric scalar. Threshold for number of samples (This is set in main function and passed)
#'
#'@return
#'
#'\item{numSamples}{tibble (nx4). Columns =  YEAR, QTR, len_totalNumLen, len_numLengthSamples}
#'
#'@noRd


missing_length_by_year <- function(YEARData,targetYEAR,minNumSamples,aggregate_to) {


  # aggregate QTR/SEMESTER to annual. Code QTR = 0
  if (aggregate_to %in% c("QTR","YEAR")) {
    aggregate_to <- "QTR"
  } else if (aggregate_to == "SEMESTER") {
  } else {
    stop("Check how you plan to aggregate data to annual data")
  }

  # select a vector of Years with +ve number of length samples
  closestYears <- YEARData %>%
    dplyr::filter(len_numLengthSamples >= minNumSamples) %>%
    dplyr::select(YEAR) %>%
    dplyr::pull(YEAR)
  # find closest YEAR to target
  diffYear <- abs(targetYEAR-closestYears)
  useYear <- min(closestYears[which(diffYear == min(diffYear))])

  numSamples <- YEARData %>% dplyr::filter(YEAR==useYear) %>%
    dplyr::select(YEAR,.data[[aggregate_to]],len_totalNumLen,len_numLengthSamples)


  return(numSamples)

}

