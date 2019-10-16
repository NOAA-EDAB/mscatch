#' Expand unclassified landings
#'
#'@param landingsData Tibble. Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples).
#'@param lengthData Tibble. Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)
#'@param nLengthSamples Numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Default = 1
#'@param otherGear Character string. Code to indicate the class for "other Gear". This is the group of gear types that land the species of interest but in small numbers
#'
#'@export


expand_unclassified_landings <- function(landingsData,lengthData,nLengthSamples,otherGear){

  ## Now deal with unclassifieds.
  # If have length samples they have already been expanded for all gear codes not "other"

  # First pass, MARKET CODEs were aggregated to the same time level eg. all market codes for each qtr
  # .eg. either all Yearly or all QTR

  # unclassified over NEGEAR and season (QTR)
  # select all cases where we have UNclassified landings but no length samples
  unclass <- landingsData %>%
    dplyr::filter(MARKET_CODE == "UN" & NEGEAR != otherGear & len_totalNumLen < nLengthSamples )  %>%
    dplyr::distinct(YEAR,QTR,NEGEAR)

  nUnclass <- dim(unclass)[1] # number of cases

  # for each row, select length distribution from master and expand
  for(irow in 1:nUnclass) {
    # pull all lengths for YEAR, QTR, NEGEAR where MARKET CODE != "UN"
    lengthDist <- lengthData %>%
      dplyr::filter(YEAR == unclass$YEAR[irow] & QTR == unclass$QTR[irow] & NEGEAR == unclass$NEGEAR[irow] & MARKET_CODE != "UN")
    # pull all landings for YEAR, QTR, NEGEAR
    landDist <- landingsData %>%
      dplyr::filter(YEAR == unclass$YEAR[irow] & QTR == unclass$QTR[irow] & NEGEAR == unclass$NEGEAR[irow])

    # find landingd for "UN" and not "UN"
    land <- sum((landDist %>% dplyr::filter(MARKET_CODE != "UN"))$landings_land)
    UNLand <- sum((landDist %>% dplyr::filter(MARKET_CODE == "UN"))$landings_land)
    # find the proportion of "UN" to all landings
    scaling <- UNLand/land

    # scale all the lengths of non "UN" then add them to lengthsData as expanded "UN"
    lengthDist <- lengthDist %>% dplyr::mutate(weight = weight*scaling)
    lengthDist$MARKET_CODE <-  "UN"
    lengthData <- rbind(lengthData,lengthDist)
  }

  # now deal with "other gear" category which will ge aggregated annually rather than by QTR.
  # By definition other gear category will have few landings and therefor aggregated to annual
  # note unclassifieds with length samples already expanded
  unclass <- landingsData %>%
    dplyr::filter(MARKET_CODE == "UN" & NEGEAR == otherGear & len_totalNumLen < nLengthSamples )  %>%
    dplyr::distinct(YEAR,QTR,NEGEAR)

  nUnclass <- dim(unclass)[1] # number of cases





  return(list(landings = landingsData,lengthData = lengthData))
}
