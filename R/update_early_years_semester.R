#' Fill in missing length sample data for early years
#'
#' Data shows landings recorded prior to a time when length samples were taken.
#' eg. Landings in 1965 but length sampling began in 1969. Length samples are asscoiated with these earlier
#' landings.
#'
#'@param SEMESTERData Tibble. (n x 8). Filtered landings data by NEGEAR, MARKET_CODE, YEARs > earlyYears
#'@param maxEarlyYear numeric scalar. The last year before length sampling began
#'@param isem numericic scalar. SEMESTER in which to pull length data from
#'@param nLengthSamples numeric scalar.The minimum number of length sample sizes required to avoid combination of data. Default = 1
#'
#'@return
#'\item{numSamples}{tibble (nx4). Columns =  YEAR, QTR, len_totalNumLen, len_numLengthSamples}
#'
#'@noRd


update_early_years_semester <- function(SEMESTERData,maxEarlyYear,isem,nLengthSamples) {

  numSamples <- SEMESTERData %>%
    dplyr::filter(YEAR==(maxEarlyYear+1) & SEMESTER==isem) %>%
    dplyr::select(YEAR,SEMESTER,len_totalNumLen,len_numLengthSamples)

  if (dim(numSamples)[1] == 0)  { # empty tibble or zero length samples
    # look another year back
    numSamples <- update_early_years_semester(SEMESTERData,maxEarlyYear+1,isem,nLengthSamples)

  } else if (numSamples$len_numLengthSamples < nLengthSamples) { # look back a year
    numSamples <- update_early_years_semester(SEMESTERData,maxEarlyYear+1,isem,nLengthSamples)
  } else {
    # Done
  }


  return(numSamples)

}
