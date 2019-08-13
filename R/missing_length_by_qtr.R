#' Recursive function to fill in missing sample lengths by quarter
#'
#' Obtain the YEAR and QTR length samples to be used for missing YEAR/QTR data
#'
#'
#' Internal function

missing_length_by_qtr <- function(QTRData,missingQTRs,iyear) {

  numSamples <- QTRData %>% dplyr::filter(YEAR==(missingQTRs$YEAR[iyear]-1) & QTR==missingQTRs$QTR[iyear]) %>%
    dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)

  if (numSamples$len_numLengthSamples == 0) {
    # previous year also has no samples need to look back another year
    numSamples <- missing_length_by_qtr(QTRData,missingQTRs,iyear-1)
  }

  return(numSamples)
}
