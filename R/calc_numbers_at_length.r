#'Calculates the numbers of fish at length
#'
#'Given the landings (expanded by length), and the length-weight relationship,
#'the number of fish at a given length is calculated.
#'
#'THIS NEEDS TO BE GENERALIZED.
#'
#'@param expLandings Tibble. Expanded langings by length (from \code{expand_landings_to_lengths})
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
#'@export

calc_numbers_at_length <- function(expLandings,lengthWeightParams){

  varType <- lengthWeightParams$varType
  lengthWeightParameters <- lengthWeightParams[[toupper(varType)]]

  # calculate the average weight of a fish of a given length and the estimated
  # total number of fish of a given length
  landingsExpanded <- NULL
  # number of length weight relationships
  numFits <- length(lengthWeightParameters$betas)
  # number of levels of aggregation
  temporalAgg <- length(unique(expLandings$TIME))

  # Note: the level of aggregation may not necessarily = the number og l-w fits
  # Eg. Aggregate to semester level but use a single length weight relationship
  # Eg aggregate to quarter but use semester l-w relationships
  if (numFits > 4) {
    stop("We have not coded for for > quarterly l-w relationships. Eg. annual or by Sex")
  }

  if (numFits > temporalAgg) {
    stop("Level of aggegation is smaller than level of l-w relationships. Eg. Tring to apply quartly l-w relaionships to catch data
         aggregated to the the semester level")
  }

  # MAY BE ABLE TO DO ALL OF THIS IN ONE, USING DPLYR

  if (numFits == temporalAgg) { # eg. quarter - quarter

    for (it in 1:numFits) {
      lengthWeightPs <- lengthWeightParameters
      lengthWeightPs$betas <- lengthWeightParameters$betas[it]

      if (temporalAgg == 1) { # aggregation to annual level. all TIME is recoded ==0
        landingsExp <- expLandings %>%
          dplyr::mutate(numbers = expand_to_num(LENGTH,weight,lengthWeightPs,"numbers")) %>%
          dplyr::mutate(fishWeight = expand_to_num(LENGTH,weight,lengthWeightPs,"fishweight"))
      } else {
        landingsExp <- expLandings %>%
          dplyr::filter(TIME == it) %>%
          dplyr::mutate(numbers = expand_to_num(LENGTH,weight,lengthWeightPs,"numbers")) %>%
          dplyr::mutate(fishWeight = expand_to_num(LENGTH,weight,lengthWeightPs,"fishweight"))
      }

      landingsExpanded <- rbind(landingsExpanded,landingsExp)
    }

  } else if ((temporalAgg==4) & (numFits == 2)) { # quarter -> semester

    for (it in 1:numFits) {
      lengthWeightPs <- lengthWeightParameters
      lengthWeightPs$betas <- lengthWeightParameters$betas[it]

      # ugly code. sort out
      sem <- c(1,2)
      itt <- 2*(it-1) + sem
      landingsExp <- expLandings %>%
        dplyr::filter(TIME %in% itt) %>%
        dplyr::mutate(numbers = expand_to_num(LENGTH,weight,lengthWeightPs,"numbers")) %>%
        dplyr::mutate(fishWeight = expand_to_num(LENGTH,weight,lengthWeightPs,"fishweight"))

      landingsExpanded <- rbind(landingsExpanded,landingsExp)
    }

  } else if (numFits == 1) { # ANYTHING -> year
print("########################## HERE ##########################")
    for (it in 1:numFits) {
      lengthWeightPs <- lengthWeightParameters
      lengthWeightPs$betas <- lengthWeightParameters$betas[it]

      # no need to filter time since apply same l-w
      landingsExp <- expLandings %>%
        dplyr::mutate(numbers = expand_to_num(LENGTH,weight,lengthWeightPs,"numbers")) %>%
        dplyr::mutate(fishWeight = expand_to_num(LENGTH,weight,lengthWeightPs,"fishweight"))

      landingsExpanded <- rbind(landingsExpanded,landingsExp)
    }

  } else {
    stop(paste0("temporalAgg = ",temporalAgg ," and numFits = ",numFits))
  }

  return(landingsExpanded)
}
