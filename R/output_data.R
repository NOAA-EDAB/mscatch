#' Aggregates data to Time interval and summarizes
#'
#' groups the data by time and summarizes
#'
#'@param data List. Landings data and length data
#'@param aggregate_to Character string. Level of aggregation for all MARKET_CODES and gears ("QTR", "YEAR", "SEMESTER", MIX").
#'
#'@return List
#'
#'\item{landings}{same as input}
#'\item{lengthData}{Same as input}
#'
#'@noRd

output_data <- function(data,aggregate_to) {

  # aggregate data over time
  if (aggregate_to == "YEAR") {
    data$landings <- data$landings %>%
      dplyr::group_by(.data$YEAR, .data$NEGEAR, .data$MARKET_CODE) %>%
      dplyr::summarise(landings_land = sum(.data$landings_land),
                       len_totalNumLen = sum(.data$len_totalNumLen),
                       len_numLengthSamples = sum(.data$len_numLengthSamples),
                       landings_nn = sum(.data$landings_nn),
                       .groups="drop")

    data$lengthData <- data$lengthData %>%
      dplyr::group_by(.data$YEAR, .data$NEGEAR,.data$MARKET_CODE,.data$LENGTH) %>%
      dplyr::summarise(NUMLEN = sum(.data$NUMLEN),.groups="drop")

  } else if (aggregate_to == "QTR") {
    data$landings <- data$landings %>%
      dplyr::group_by(.data$YEAR, .data$QTR, .data$NEGEAR,.data$MARKET_CODE) %>%
      dplyr::summarise(landings_land = sum(.data$landings_land),
                       len_totalNumLen = sum(.data$len_totalNumLen),
                       len_numLengthSamples = sum(.data$len_numLengthSamples),
                       landings_nn = sum(.data$landings_nn),
                       .groups="drop")

    data$lengthData <- data$lengthData %>%
      dplyr::group_by(.data$YEAR,.data$QTR, .data$NEGEAR,.data$MARKET_CODE,.data$LENGTH) %>%
      dplyr::summarise(NUMLEN = sum(.data$NUMLEN),.groups="drop")

  } else if (aggregate_to == "SEMESTER") {
    ## Assumes 1st SEMESTER = QTR 1 + 2
    ## Assumes 2nd SEMESTER = QTR 3 + 4
    data$landings <- data$landings %>%
      dplyr::group_by(.data$YEAR, .data$SEMESTER, .data$NEGEAR,.data$MARKET_CODE) %>%
      dplyr::summarise(landings_land = sum(.data$landings_land),
                       len_totalNumLen = sum(.data$len_totalNumLen),
                       len_numLengthSamples = sum(.data$len_numLengthSamples),
                       landings_nn = sum(.data$landings_nn),
                       .groups="drop")

    data$lengthData <- data$lengthData %>%
      dplyr::group_by(.data$YEAR,.data$SEMESTER, .data$NEGEAR,.data$MARKET_CODE,.data$LENGTH) %>%
      dplyr::summarise(NUMLEN = sum(.data$NUMLEN),.groups="drop")
  } else {# do nothing
    stop("This isn't currently supported, please select a level of aggregation (\"QTR\", \"SEMESTER\", \"YEAR\")")

  }

  return(data)
}
