#'Proportions commercial landings based on length sample distribution
#'
#'@param landingsData Tibble. Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples).
#'@param lengthData Tibble. Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)
#'@param lengthWeightParams List. alpha = intercept, betas = slope(s), var = residual variance used to formulate the mean (?see Notes section below)
#'
#'@return A Tibble of expanded landings to represent weight of landings by length
#'\item{YEAR}{Year of landings}
#'\item{NEGEAR}{3 digit gear code as defined in cfdbs.gear}
#'\item{QTR}{Quarter year of landings (The presence of this field depends on whether it was present in the \code{landingsData}}
#'\item{MARKET_CODE}{Market code assigned to landed fish (The presence of this field depends on whether it was present in the \code{landingsData}}
#'\item{LENGTH}{Length of sampled fish}
#'\item{NUMLEN}{number of sampled for fish stated LENGTH}
#'\item{weight}{expanded weight of all fish of given LENGTH in YEAR, NEGEAR etc..}
#'
#'@section Notes:
#'
#'The length weight relationship (see \code{\link{fit_length_weight}}) was fit assuming log normal errors (normal on the log scale)
#'Therefor when exponentiation we need to correct for the mean:
#'
#'E(W) = \eqn{ L^\beta exp(\alpha + \sigma^2 / 2)}
#'
#'@section Expansion calculations:
#'
#' For each unique category (YEAR, QTR, NEGEAR, MARKET_CODE) weights (mean weights, metric tons)
#' are attributed to the sampled individuals lengths using the weight-length relationship above.
#' This distribution of weights by length is then scaled such that
#' the sum of weights (over lengths) = the total landed weight from the landingsData.
#' This scaling assumes that the landed (commercial) fish have the same length distribution as the sampled fish.
#'
#'
#'@export

expand_landings_to_lengths <- function(landingsData,lengthData,lengthWeightParams){


  # determine fields to join by. MARKET_CODE & QTR may be missing
  mainList <- c("YEAR","QTR","NEGEAR","MARKET_CODE")
  varsPresent <- mainList[mainList %in% names(landingsData) ]

  # join tables together for length and landings
  #joined <- dplyr::left_join(lengthData,landingsData,by=c("YEAR","QTR","NEGEAR","MARKET_CODE"))
  joined <- dplyr::left_join(landingsData,lengthData,by=varsPresent)
  # atribute weight to each fish in group and scale up by "expansion factor
  landingsExpanded <- joined %>%
    dplyr::group_by(dplyr::across(varsPresent)) %>%
    dplyr::mutate(weight = expand_to_weight(LENGTH,NUMLEN,landings_land,lengthWeightParams)) %>%
    dplyr::select(!!varsPresent,LENGTH,NUMLEN,weight) %>%
    dplyr::ungroup()


  # join tables together for length and landings
  #joined <- dplyr::left_join(lengthData,landingsData,by=c("YEAR","QTR","NEGEAR","MARKET_CODE"))
  # joined <- dplyr::left_join(landingsData,lengthData,by=varsPresent)
  # # attribute weight to each fish in group and scale up by "expansion factor
  # master <- joined %>%
  #   dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
  #   dplyr::mutate(weight = expand_to_weight(LENGTH,NUMLEN,landings_land,lengthWeightParams)) %>%
  #   dplyr::select(YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH,NUMLEN,weight) %>%
  #   dplyr::ungroup()

  return(landingsExpanded)
}
