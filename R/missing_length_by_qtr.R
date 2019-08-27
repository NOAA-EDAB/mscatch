#' Recursive function to fill in missing sample lengths by quarter
#'
#' Obtain the YEAR and QTR length samples to be used for missing YEAR/QTR data
#'
#'
#' @export

missing_length_by_qtr <- function(QTRData,missingQTRs,iyear) {

  print(QTRData)
  print(missingQTRs)
  print(missingQTRs$YEAR[iyear])
  numSamples <- QTRData %>%
    dplyr::filter(YEAR==(missingQTRs$YEAR[iyear]-1) & QTR==missingQTRs$QTR[iyear]) %>%
    dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)

  print(numSamples)
  if (dim(numSamples)[1] == 0)  { # empty tibble or zero length samples
    # look anothe year back
    numSamples <- missing_length_by_qtr(QTRData,missingQTRs,iyear-1)
  } else if (numSamples$len_numLengthSamples == 0) {
    # previous year also has no samples need to look back another year
    numSamples <- missing_length_by_qtr(QTRData,missingQTRs,iyear-1)
  }

  return(numSamples)
}
