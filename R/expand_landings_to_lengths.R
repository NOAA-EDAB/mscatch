#'Proportions commercial landings based on length sample distribution
#'
#'@param landingsData Tibble. Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples).
#'@param lengthData Tibble. Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)
#'@param lengthWeightParams List. alpha = intercept, betas = slope(s), var = residual variance used to formulate the mean (?see Notes section below)
#'
#'
#'@return A Tibble with same columns as \code{landingsData}
#'\item{expandedLandings}{landingsData expanded to represent weight of landings by length}
#'
#'@section Notes:
#'
#'The length weight relationship (see \code{\url{fit_length_weight}}) was fit assuming log normal errors (normal on the log scale)
#'Therefor when exponentiation we need to correct for the mean:
#'
#'E(W) = \eqn{ L^\beta exp(\alpha + \sigma^2 / 2)}
#'
#'@section Expansion calculations:
#'
#' For each unique category (YEAR, QTR, NEGEAR, MARKET_CODE) weights (mean weights, metric tons) are attributed to the sampled individuals lengths using the weight-length relationship above.
#' This distribution of weights by length is then scaled such that the sum of weights (over lengths) = the total landed weight from the landingsData.
#' This scaling assumes that the landed (commercial) fish have the same length distribution as the sampled fish.
#'
#'
#'@export

expand_landings_to_lengths <- function(landingsData,lengthData,lengthWeightParams){

  # join tables together for length and landings
  joined <- dplyr::left_join(lengthData,landingsData,by=c("YEAR","QTR","NEGEAR","MARKET_CODE"))
  # atribute weight to each fish in group and scale up by "expansion factor
  master <- joined %>% dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>% dplyr::mutate(weight = expand_to_weight(LENGTH,NUMLEN,landings_land,lengthWeightParams))

  return(master)
}
