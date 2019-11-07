#' Recursive function to fill in missing sample lengths by quarter
#'
#' Obtain the YEAR and QTR length samples to be used for missing YEAR/QTR data
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

##NOTE: recursion needs to be stopped at some point.
missing_length_by_qtr <- function(QTRData,iyear,iqtr,nLengthSamples,minYear) {

  numSamples <- QTRData %>%
    dplyr::filter(YEAR==(iyear-1) & QTR==iqtr) %>%
    dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)

  #print(c(iyear,minYear))
  if (iyear >= minYear+1) {
    #print(iyear)
    if (dim(numSamples)[1] == 0)  { # empty tibble or zero length samples
      # look another year back
      numSamples <- missing_length_by_qtr(QTRData,iyear-1,iqtr,nLengthSamples,minYear)
    } else if (numSamples$len_numLengthSamples < nLengthSamples) {
      # previous year doesn't have ENOUGH samples. need to look back another year
      numSamples <- missing_length_by_qtr(QTRData,iyear-1,iqtr,nLengthSamples,minYear)
    }
  }

  return(numSamples)
}
