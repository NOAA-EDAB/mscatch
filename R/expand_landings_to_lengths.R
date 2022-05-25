#'Proportions commercial landings based on length sample distribution
#'
#'@param landingsData Tibble. Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples).
#'@param lengthData Tibble. Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)
#'@param lengthWeightParams List. alpha = intercept, betas = slope(s), var = residual variance used to formulate the mean (?see Notes section below)
#'
#'@return A Tibble of expanded landings to represent weight of landings by length
#'\item{YEAR}{Year of landings}
#'\item{NEGEAR}{3 digit gear code as defined in cfdbs.gear}
#'\item{TIME}{Quarter/Half year of landings (The presence of this field depends on whether it was present in the \code{landingsData}}
#'\item{MARKET_CODE}{Market code assigned to landed fish (The presence of this field depends on whether it was present in the \code{landingsData}}
#'\item{LENGTH}{Length of sampled fish}
#'\item{NUMLEN}{number of sampled for fish stated LENGTH}
#'\item{weight}{expanded weight (mt) of all fish of given LENGTH in YEAR, NEGEAR etc..}
#'
#'@section Notes:
#'
#'The length weight relationship (see \code{\link{fit_length_weight}}) is fit assuming log normal errors (normal on the log scale).
#'Therefore when exponentiating a correction for the estimate is required:
#'
#'E(W) = \eqn{\alpha  L^\beta exp(\sigma^2 / 2)}
#'
#'@section Expansion calculations:
#'
#' For each unique category (YEAR, TIME, NEGEAR, MARKET_CODE) weights (mean weights, metric tons)
#' are attributed to the sampled individuals lengths using the weight-length relationship above.
#' This distribution of weights by length is then scaled such that
#' the sum of weights (over lengths) = the total landed weight from the landingsData.
#' This scaling assumes that the landed (commercial) fish have the same length distribution as the sampled fish.
#'
#'
#'@export

expand_landings_to_lengths <- function(landingsData,lengthData,lengthWeightParams){


  varType <- lengthWeightParams$varType

  # determine fields to join by. MARKET_CODE  may be missing
  mainList <- c("YEAR","TIME","NEGEAR","MARKET_CODE","SEX")
  varsPresent <- mainList[mainList %in% names(landingsData) ]
  # how the lengthWeight relationship was fit. This determines how many slope parameters and over which variable


  # join tables together for length and landings
  #joined <- dplyr::left_join(lengthData,landingsData,by=c("YEAR","QTR","NEGEAR","MARKET_CODE"))
  joined <- dplyr::left_join(landingsData,lengthData,by=varsPresent)

  landingsExpanded <- NULL


  if (toupper(varType) == "SINGLE") {
    landingsExpanded <- joined %>%
      dplyr::group_by(dplyr::across(varsPresent)) %>%
      dplyr::mutate(weight = expand_to_weight(LENGTH,NUMLEN,landings_land,lengthWeightParams)) %>%
      dplyr::select(!!varsPresent,LENGTH,NUMLEN,weight) %>%
      dplyr::ungroup()

  } else if (toupper(varType) %in% c("QUARTER","SEMESTER")) {
    # loop over TIME
    timeVals <- unique(joined$TIME)
    for (it in 1:length(timeVals)) {
      lengthWeightPs <- lengthWeightParams
      lengthWeightPs$betas <- lengthWeightParams$betas[it]

      landingsExp <- joined %>%
        dplyr::filter(TIME == timeVals[it]) %>%
        dplyr::group_by(dplyr::across(varsPresent)) %>%
        dplyr::mutate(weight = expand_to_weight(LENGTH,NUMLEN,landings_land,lengthWeightPs)) %>%
        dplyr::select(!!varsPresent,LENGTH,NUMLEN,weight) %>%
        dplyr::ungroup()

      landingsExpanded <- rbind(landingsExpanded,landingsExp)
    }

  } else if (toupper(varType) == "YEAR") {
    # Loop over year
    timeVals <- unique(joined$YEAR)
    for (it in 1:length(timeVals)) {
      lengthWeightPs <- lengthWeightParams
      lengthWeightPs$beta <- lengthWeightParams$beta[it]

      landingsExp <- joined %>%
        dplyr::filter(YEAR == timeVals[it]) %>%
        dplyr::group_by(dplyr::across(varsPresent)) %>%
        dplyr::mutate(weight = expand_to_weight(LENGTH,NUMLEN,landings_land,lengthWeightPs)) %>%
        dplyr::select(!!varsPresent,LENGTH,NUMLEN,weight) %>%
        dplyr::ungroup()


      landingsExpanded <- rbind(landingsExpanded,landingsExp)
    }

  } else if (toupper(varType) == "SEX") {
    # Loop over sex
    timeVals <- unique(joined$SEX)
    for (it in 1:length(timeVals)) {
      lengthWeightPs <- lengthWeightParams
      lengthWeightPs$beta <- lengthWeightParams$beta[it]

      landingsExp <- joined %>%
        dplyr::filter(SEX == timeVals[it]) %>%
        dplyr::group_by(dplyr::across(varsPresent)) %>%
        dplyr::mutate(weight = expand_to_weight(LENGTH,NUMLEN,landings_land,lengthWeightPs)) %>%
        dplyr::select(!!varsPresent,LENGTH,NUMLEN,weight) %>%
        dplyr::ungroup()


      landingsExpanded <- rbind(landingsExpanded,landingsExp)
    }

  } else {
    stop(paste0("Not coded for variable type = ",varType))
  }


  return(landingsExpanded)
}
