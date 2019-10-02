#' Length expansion calculation
#'
#' Expand lengths in sample to weight and scale up to landings total.
#'
#' @param fishLength Numeric vector. Lengths of fish in group
#' @param numAtLength Numeric vector. Number of fish at specified \code{fishLength}'s
#' @param landings Numeric vector. Landings total for group
#' @param lengthWeightParams List (3). List. alpha = intercept, betas = slope(s), var = residual variance used to formulate the mean
#'
#' @return vector
#' \item{fishWeight}{scaled landings for fish of specified \code{fishLength}}
#'
#' @section Notes:
#'
#' This an internal function called within a dplyr::mutate statement operating on a group
#'
#'
#'

expand_to_weight <- function(fishLength,numAtLength,landings,lengthWeightParams) {

  # length-weight parameters (for ease of reading code)
  alpha <- exp(as.double(lengthWeightParams$alpha))
  beta <- as.double(lengthWeightParams$betas)
  sigma2 <- as.double(lengthWeightParams$var)

  # vector of total weight for the number of fish (in the sample) of a given length
  fishWeight <- (alpha*fishLength^beta)*numAtLength

  #mean Sample Weight
  sampleWeight <- sum(fishWeight)
  # proportion of total landed weight
  expansionFactor <- landings/sampleWeight

  # scaled weight to landings total.
  fishWeight <- fishWeight * expansionFactor

  return(fishWeight)
}
