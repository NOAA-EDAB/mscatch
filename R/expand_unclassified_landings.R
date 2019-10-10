#' Expand unclassified landings
#'
#'@param landingsData Tibble. Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples).
#'@param lengthData Tibble. Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)
#'
#'@export


expand_unclassified_landings <- function(landingsData,lengthData){

  ## now deal with unclassified. Treat as if none have length samples.
  # unclassified over NEGEAR and season (QTR)
  # First remove all UN from length data.

  lengthData <- lengthData %>% dplyr::filter(MARKET_CODE != "UN")

  unclass <- landingsData %>%
    dplyr::filter(MARKET_CODE == "UN" & len_totalNumLen < nLengthSamples )  %>%
    dplyr::distinct(YEAR,QTR,NEGEAR)
  print(unclass)
  nUnclass <- dim(unclass)[1]
  # for each row, select length distribution from master and expand
  for(irow in 1:nUnclass) {
    rowData <- master %>% dplyr::filter(YEAR == unclass$YEAR[irow] & QTR == unclass$QTR[irow] & NEGEAR == unclass$NEGEAR[irow])
    if (dim(rowData)[1] ==0) {
      rowData <- master %>% dplyr::filter(YEAR == unclass$YEAR[irow] & QTR == 0 & NEGEAR == unclass$NEGEAR[irow])
    }
    if (dim(rowData)[1] ==0) {
      print(unclass[irow,])
    }
  }

    return(list(landings = landingsData,lengthData = lengthData))

}
