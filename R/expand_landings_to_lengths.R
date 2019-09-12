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
#'
#'
#'@export

expand_landings_to_lengths <- function(landingsData,lengthData,lengthWeightParams){
  print(lengthWeightParams)

  return()
}
