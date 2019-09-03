#'Proportions commercial landings based on length sample distribution
#'
#'@param landingsData Tibble. Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples).
#'@param lengthData Tibble. Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)}
#'@param lengthWeightKey how define this? one over all time, time varying?
#'
#'
#'@return Tibble
#'\item{expandedLandings}{landingsData expanded to represent weight of landings by length}
#'
#'@export

expand_landings_to_lengths <- function(landingsData,lengthData,lengthWeightKey){



}
