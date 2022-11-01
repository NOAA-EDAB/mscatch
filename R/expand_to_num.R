#' Number expansion calculation
#'
#' Calculate the expected number of fish of given length based on
#' length-weight relationship.
#'
#' @param fishLength Numeric vector. Lengths of fish in group
#' @param weight Numeric vector. Landings total for group (kg)
#' @param lengthWeightParams List (3). List. alpha = intercept, betas = slope(s), var = residual variance used to formulate the mean
#' Assumes the weight of individual fish is in kg
#' @param flag Character. Which feature to calculate. Either "numbers" (estimate number of fish)
#'  or "fishweight" (estimate the weight of an individual fish)
#'
#' @return vector
#' \item{fishWeight}{scaled landings for fish of specified \code{fishLength}}
#'
#' @section Notes:
#'
#' This an internal function called within a dplyr::mutate statement operating on a group
#'
#'
#' @noRd

expand_to_num <- function(fishLength,weight,lengthWeightParams,flag) {

  # length-weight parameters (for ease of reading code)
  alpha <- exp(as.double(lengthWeightParams$logAlpha))
  beta <- as.double(lengthWeightParams$betas)
  sigma2 <- as.double(lengthWeightParams$var)

  # vector of weight of individual fish (in the sample) of a given length
  # in kg

  # length weight relationship in kg - cm
  # weight assumed to be in kg
  fishWeight <- (alpha*fishLength^beta)*exp(sigma2/2)

  # number of fish
  #fishnum <- weight/(fishWeight/kgToMt)
  fishnum <- weight/fishWeight

  if (flag == "numbers") {
    output <- fishnum
  } else if (flag == "fishweight") {
    output <- fishWeight
  } else {
    stop(paste0("Not coded for ",flag))
  }

  return(output)
}
