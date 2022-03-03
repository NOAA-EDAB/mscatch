#' Recursive function to fill in missing sample lengths by SEMESTER
#'
#' Obtain the YEAR and SEMESTER length samples to be used for missing YEAR/SEMESTER data.
#' Looks for nearest neighbor. Not recursive
#'
#' @param SEMESTERData Tibble. (n x 8). Filtered landings data by NEGEAR, MARKET_CODE, YEARs > earlyYears
#' @param iyear Numeric scalar. YEAR in which length data are missing and need to be replaced
#' @param isem Numeric scalar. SEMESTER of YEAR in which length data are missing and need to be replaced
#' @param nLengthSamples numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Default = 1
#'
#' @return
#'\item{numSamples}{tibble (nx4). Columns =  YEAR, SEMESTER, len_totalNumLen, len_numLengthSamples}
#'
#'@noRd

missing_length_by_semester_neighbor <- function(SEMESTERData,iyear,isem,nLengthSamples) {

  # select a vector of Years/SEMESTERs with +ve number of length samples
  closestYearSEMESTERs <- SEMESTERData %>%
    dplyr::filter(len_numLengthSamples >= nLengthSamples) %>%
    dplyr::mutate(DecimalYEAR = (as.double(YEAR) + 0.5*(as.double(SEMESTER)-1))) %>%
    dplyr::select(YEAR,QTR,DecimalYEAR)

  targetYEAR <- as.double(iyear) + 0.5*(as.double(isem)-1)

  # find closest YEAR to target
  diffYear <- abs(targetYEAR-closestYearSEMESTERSs$DecimalYEAR)
  useYear <- closestYearSEMESTERs[which(diffYear == min(diffYear)),][1,]

  numSamples <- SEMESTERData %>%
    dplyr::filter(YEAR==useYear$YEAR, SEMESTER==useYear$SEMESTER, len_numLengthSamples >= nLengthSamples ) %>%
    dplyr::select(YEAR,SEMESTER,NEGEAR,len_totalNumLen,len_numLengthSamples)

  # incase there are multiple fleets with same number of length samples
  numSamples <- numSamples %>% dplyr::filter(len_numLengthSamples==max(len_numLengthSamples))
  numSamples <- head(numSamples,1)
  return(numSamples)
}
