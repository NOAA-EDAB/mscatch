#' Fill in missing length sample data for early years
#'
#' Data shows landings recorded prior to a time when length samples were taken.
#' eg. Landings in 1965 but length sampling began in 1969. Length samples are asscoiated with these earlier
#' landings.
#'
#'@param QTRData Tibble. (n x 8). Filtered landings data by NEGEAR, MARKET_CODE, YEARs > earlyYears
#'@param maxEarlyYear numeric scalar. The last year before length samplings began
#'@param iqtr numic scalar. QTR in which to pull length data from
#'@param nLengthSamples numeric scalar.The minimum number of length sample sizes required to avoid combination of data. Dfeault = 1
#'
#'@return
#'\item{numSamples}{tibble (nx4). Columns =  YEAR, QTR, len_totalNumLen, len_numLengthSamples}
#'


update_early_years <- function(QTRData,maxEarlyYear,iqtr,nLengthSamples) {

  numSamples <- QTRData %>% dplyr::filter(YEAR==(maxEarlyYear+1) & QTR==iqtr) %>%
  dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)

  if (dim(numSamples)[1] == 0)  { # empty tibble or zero length samples
    # look another year back
    numSamples <- update_early_years(QTRData,maxEarlyYear+1,iqtr,nLengthSamples)

  } else if (numSamples$len_numLengthSamples < nLengthSamples) { # look back a year
    numSamples <- update_early_years(QTRData,maxEarlyYear+1,iqtr,nLengthSamples)
  }


  return(numSamples)

}
