#' Recursive function to fill in missing sample lengths by quarter
#'
#' Obtain the YEAR and QTR length samples to be used for missing YEAR/QTR data.
#' Looks for nearest neighbor. Not recursive
#'
#' @param QTRData Tibble. (n x 8). Filtered landings data by NEGEAR, MARKET_CODE, YEARs > earlyYears
#' @param iyear Numeric scalar. YEAR in which length data are missing and need to be replaced
#' @param iqtr Numeric scalar. QTR of YEAR in which length data are missing and need to be replaced
#' @param nLengthSamples numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Dfault = 1
#' @param minYear numeric Scalar. Earliest year present in data.
#'
#' @return
#'\item{numSamples}{tibble (nx4). Columns =  YEAR, QTR, len_totalNumLen, len_numLengthSamples}
#'

missing_length_by_qtr_neighbor <- function(QTRData,iyear,iqtr,nLengthSamples,minYear) {

  # select a vector of Years/QTRs with +ve number of length samples
  closestYearQtrs <- QTRData %>%
    dplyr::filter(len_numLengthSamples >= nLengthSamples) %>%
    dplyr::mutate(DecYEAR = (as.double(YEAR) + 0.2*(as.double(QTR)-1))) %>%
    dplyr::select(YEAR,QTR,DecYEAR)

  targetYEAR <- as.double(iyear) + 0.2*(as.double(iqtr)-1)

  # find closest YEAR to target
  diffYear <- abs(targetYEAR-closestYearQtrs$DecYEAR)
  useYear <- closestYearQtrs[which(diffYear == min(diffYear)),][1,]

  numSamples <- QTRData %>%
    dplyr::filter(YEAR==useYear$YEAR, QTR==useYear$QTR, len_numLengthSamples >= nLengthSamples ) %>%
    dplyr::select(YEAR,QTR,NEGEAR,len_totalNumLen,len_numLengthSamples)

  # incase there are multiple fleets with same number of length samples
  numSamples <- numSamples %>% dplyr::filter(len_numLengthSamples==max(len_numLengthSamples))
  numSamples <- head(numSamples,1)
  return(numSamples)
}
