#'Calculates the numbers of fish at age
#'
#'Given the landings (expanded by length), the age-length key and the length-weight relationship,
#'the number of fish at a given age is calculated
#'
#'@param expLandings Tibble. Expanded langings by length (from \code{expand_landings_to_lengths})
#'@param ageLengthKeys Tibble. Age Length Keys (from \code{create_yr_semester_age_length_key}
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

calc_numbers_at_age <- function(expLandings,ageLenKeys,lengthWeightParams){

  varType <- lengthWeightParams$varType
  lengthWeightParameters <- lengthWeightParams[[toupper(varType)]]

  # calculate the average weight of a fish of a given length and the estimated
  # total number of fish of a given length
  landingsExpanded <- NULL
  numTimes <- length(lengthWeightParameters$betas)
  for (it in 1:numTimes) {
    lengthWeightPs <- lengthWeightParameters
    lengthWeightPs$betas <- lengthWeightParameters$betas[it]
    landingsExp <- expLandings %>%
      dplyr::filter(TIME == it) %>%
      dplyr::mutate(numbers = expand_to_num(LENGTH,weight,lengthWeightPs,"numbers")) %>%
      dplyr::mutate(fishWeight = expand_to_num(LENGTH,weight,lengthWeightPs,"fishweight"))

      landingsExpanded <- rbind(landingsExpanded,landingsExp)
  }

  # multiply my age Length key to obtain number of fish by age
  # long format for age length key

  nms <- as.numeric(colnames(ageLenKeys))
  log <- !is.na(nms)
  ages <- as.character(nms[log])

  agedf <- landingsExpanded %>%
    dplyr::left_join(ageLenKeys,by=c("YEAR","TIME","LENGTH")) %>%
    dplyr::mutate(dplyr::across(.cols=ages, ~.x*numbers, .names = "AGE_{.col}")) %>%
    dplyr::select(-c(ages))

  return(agedf)
}
