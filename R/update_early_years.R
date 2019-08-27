#' Fill in missing length sample data for early years
#'
#' Data shows landings recorded prior to a time when length samples were taken.
#' eg. Landings in 1965 but length sampling began in 1969. Length samples are asscoiated with these earlier
#' landings.
#'
#'@param QTRData data
#'@param maxEarlyYear numeric scalar. The last year before length samplings began
#'@param iqtr numic scalar. QTR in which to pull length data from
#'
#'@return
#'\item{numSamples}{description}
#'
#'
#'
#' @export

update_early_years <- function(QTRData,maxEarlyYear,iqtr) {

  numSamples <- QTRData %>% dplyr::filter(YEAR==(maxEarlyYear+1) & QTR==iqtr) %>%
  dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)

  if (dim(numSamples)[1] == 0)  { # empty tibble or zero length samples
    # look another year back
    numSamples <- update_early_years(QTRData,maxEarlyYear+1,iqtr)
  } else if (numSamples$len_numLengthSamples == 0) {
    numSamples <- update_early_years(QTRData,maxEarlyYear+1,iqtr)
  }


  return(numSamples)

}
