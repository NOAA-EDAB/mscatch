#' Function to fill in missing sample lengths by year
#'
#' Obtain the YEAR length samples to be used for missing YEAR data
#' Assuption is the data has been aggregated to YEAR from QTR.
#' The closest year that contains length samples is chosen
#'
#'
#' Internal function
#' @export

missing_length_by_year <- function(YEARData,targetYEAR,minNumSamples) {

  closestYears <- dplyr::pull(YEARData %>% dplyr::filter(numSamples > minNumSamples) %>% dplyr::select(YEAR),YEAR)
  diffYear <- abs(targetYEAR-closestYears)
  useYear <- min(closestYears[which(diffYear == min(diffYear))])
  print(c(targetYEAR,useYear))
  return(useYear)


}
