#' update missing length values
#'
#' For each YEAR/QRT/NEGEAR/MARKET_CODE with missing length samples we update with previous years data.
#' No aggregation is undertaken. Simply populating empty cells or adding length data to previously omitted cells.
#'
#' @param data List. landings and lengthData
#' @param missingRow Dataframe. Specifies the YEAR and QTR where length samples are missing
#' @param gearType Character string. Name of NEGEAR
#' @param mainGearType Character string. Name of NEGEAR for which samples are borrowed from
#' @param marketCode Character string. Name of MARKET_CODE
#' @param numSamples Tibble. Filtered landings data (YEAR, QTR, len_totalNumLen, len_numLengthSamples) by NEGEAR, MARKET_CODE
#'
##' @return List of landings and associated length samples .Same as input data
#' \item{landings}{Tibble (n x 8). Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples) }
#' \item{lengthData}{Tibble (m x 8 ). Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)}
#'
#'@noRd

update_length_samples <- function(data,missingRow,gearType,marketCode,numSamples,mainGearType = NULL) {

  # pick out row with missing data in LANDINGS
  ind <- (data$landings$YEAR == missingRow$YEAR) &
    (data$landings$QTR == missingRow$QTR) &
    (data$landings$NEGEAR == gearType) &
    (data$landings$MARKET_CODE == marketCode)

  # fill in missing data
  data$landings[ind,]$len_totalNumLen <- numSamples$len_totalNumLen
  data$landings[ind,]$len_numLengthSamples <- numSamples$len_numLengthSamples


  # since no lengths exist for this YEAR/QTR/NEGEAR/MARKET_CODE duplicate rows from year to use
  if (is.null(mainGearType)) {
    newLengthData <- data$lengthData %>% dplyr::filter(YEAR==numSamples$YEAR & QTR==numSamples$QTR & NEGEAR==gearType & MARKET_CODE==marketCode)
  } else { # borrow from different gear
    newLengthData <- data$lengthData %>% dplyr::filter(YEAR==numSamples$YEAR & QTR==numSamples$QTR & NEGEAR==mainGearType & MARKET_CODE==marketCode)
    #newLengthData <- newLengthData %>% dplyr::mutate(NEGEAR = replace(NEGEAR,NEGEAR==gearType,mainGearType))
    # now replace the data for mainGearType to data for current gear type
    newLengthData$NEGEAR <- gearType
  }
  # replace the YEAR and QTR with data for the data which was missing
  newLengthData$YEAR <- missingRow$YEAR
  newLengthData$QTR <- missingRow$QTR
  data$lengthData <- rbind(data$lengthData,newLengthData)

  return(data)
}
