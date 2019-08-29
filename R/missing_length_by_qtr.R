#' Recursive function to fill in missing sample lengths by quarter
#'
#' Obtain the YEAR and QTR length samples to be used for missing YEAR/QTR data
#'
#' @param QTRData description
#' @param iyear numic scalar. YEAR in which length data are missing and need to be replaced
#' @param iqtr numic scalar. QTR of YEAR in which length data are missing and need to be replaced
#' @param nLengthSamples numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Dfault = 1
#'
#' @return
#'\item{numSamples}{tibble (nx4). Columns =  YEAR, QTR, len_totalNumLen, len_numLengthSamples}
#'
#' @export

missing_length_by_qtr <- function(QTRData,iyear,iqtr,nLengthSamples) {

  numSamples <- QTRData %>%
    dplyr::filter(YEAR==(iyear-1) & QTR==iqtr) %>%
    dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)

  print(numSamples)
  if (dim(numSamples)[1] == 0)  { # empty tibble or zero length samples
    # look anothe year back
    numSamples <- missing_length_by_qtr(QTRData,iyear-1,iqtr,nLengthSamples)
  } else if (numSamples$len_numLengthSamples < nLengthSamples) {
    # previous year also has no samples need to look back another year
    numSamples <- missing_length_by_qtr(QTRData,iyear-1,iqtr,nLengthSamples)
  }

  return(numSamples)
}
