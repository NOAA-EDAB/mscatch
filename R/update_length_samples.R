#' update missing length values
#'
#' For each YEAR/QRT/NEGEAR/MARKET_CODE with missing length samples we update with previous years data
#'
#'
#'
#'
#'
#'internal

update_length_samples <- function(data,missingRow,gearType,marketCode,numSamples) {

  # pick out row with missing data in LANDINGS
  ind <- (data$landings$YEAR == missingRow$YEAR) &
    (data$landings$QTR == missingRow$QTR) &
    (data$landings$NEGEAR == gearType) &
    (data$landings$MARKET_CODE == marketCode)
  # fill in missing data
  data$landings[ind,]$len_totalNumLen <- numSamples$len_totalNumLen
  data$landings[ind,]$len_numLengthSamples <- numSamples$len_numLengthSamples

  # since no lengths exist for this YEAR/QTR/NEGEAR/MARKET_CODE duplicate rows from year to use
  newLengthData <- data$lengthData %>% dplyr::filter(YEAR==numSamples$YEAR & QTR==numSamples$QTR & NEGEAR==gearType & MARKET_CODE==marketCode)
  newLengthData$YEAR <- missingRow$YEAR
  data$lengthData <- rbind(data$lengthData,newLengthData)

  return(data)
}
