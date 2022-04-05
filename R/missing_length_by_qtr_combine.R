#' combine QTR in given year to either SEMESTER or ANNUAL
#'
#' Combine landings and length samples
#'
#' @param iyear Numeric scalar. YEAR in which length data are missing and need to be replaced
#' @param targetYr Numeric scalar. YEAR in which landings and lengths are combined
#' @param gearType Character string. Gear types. Either NEGEAR or custom recoded values
#' @param marketCode Character string. Market code designation
#' @param combine Character string. level in which to combine, SEMESTER or YEAR
#' @param outputDir Character string. Path to output directory (png files saved here)
#' @param logFile Character string. Specify the name for the log file generated describing all decisions made.
#'
#' @return
#'\item{numSamples}{tibble (nx4). Columns =  YEAR, SEMESTER, len_totalNumLen, len_numLengthSamples}
#'
#'@noRd

missing_length_by_qtr_combine <- function(data,targetYr,gearType,marketCode,combine,outputDir,logfile) {


  if (combine == "SEMESTER") {
    # landings rename SEMESTER to 0
    rowids <- (data$landings$YEAR == targetYr) &
      (data$landings$QTR %in% c(1,2)) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == marketCode)
    # fill in missing data
    data$landings[rowids,]$QTR <- 1.5

    rowids <- (data$landings$YEAR == targetYr) &
      (data$landings$QTR %in% c(3,4)) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == marketCode)
    # fill in missing data
    data$landings[rowids,]$QTR <- 3.5

    # lengths rename SEMESTER to 0
    rowids <- (data$lengthData$YEAR == targetYr) &
      (data$landings$QTR %in% c(1,2)) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == marketCode)
    # fill in missing data
    # relabel SEMESTERS to 0 to indicate Annual
    data$lengthData[rowids,]$QTR <- 1.5

    # lengths rename SEMESTER to 0
    rowids <- (data$lengthData$YEAR == targetYr) &
      (data$landings$QTR %in% c(3,4)) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == marketCode)
    # fill in missing data
    # relabel SEMESTERS to 0 to indicate Annual
    data$lengthData[rowids,]$QTR <- 3.5

    # aggregate unclassifieds to same level
    # landings rename SEMESTER to 0
    rowids <- (data$landings$YEAR == targetYr) &
      (data$landings$QTR %in% c(1,2)) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == "UN")
    # fill in missing data
    data$landings[rowids,]$QTR <- 1.5

    # lengths rename SEMESTER to 0
    rowids <- (data$lengthData$YEAR == targetYr) &
      (data$landings$QTR %in% c(1,2)) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == "UN")

    data$lengthData[rowids,]$QTR <- 1.5

    rowids <- (data$landings$YEAR == targetYr) &
      (data$landings$QTR %in% c(3,4)) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == "UN")
    # fill in missing data
    data$landings[rowids,]$QTR <- 3.5

    # lengths rename SEMESTER to 0
    rowids <- (data$lengthData$YEAR == targetYr) &
      (data$landings$QTR %in% c(3,4)) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == "UN")

    data$lengthData[rowids,]$QTR <- 3.5

    message(paste0("Combine all QTRs in ",targetYr," to SEMESTERS. Relabel QTR = 1.5, 3.5"))
    write_to_logfile(outputDir,logfile,data=paste0("Combine all QTRs in ",targetYr," to SEMESTERS. Relabel QTR = 1.5, 3.5"),label=NULL)


  } else if (combine == "YEAR") {
    # landings rename SEMESTER to 0
    rowids <- (data$landings$YEAR == targetYr) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == marketCode)
    # fill in missing data
      data$landings[rowids,]$QTR <- 0

    # lengths rename SEMESTER to 0
    rowids <- (data$lengthData$YEAR == targetYr) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == marketCode)
    # fill in missing data
    # relabel SEMESTERS to 0 to indicate Annual
    data$lengthData[rowids,]$QTR <- 0

    # aggregate unclassifieds to same level
    # landings rename SEMESTER to 0
    rowids <- (data$landings$YEAR == targetYr) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == "UN")
    # fill in missing data
    data$landings[rowids,]$QTR <- 0

    # lengths rename SEMESTER to 0
    rowids <- (data$lengthData$YEAR == targetYr) &
      (data$landings$NEGEAR == gearType) &
      (data$landings$MARKET_CODE == "UN")

    data$lengthData[rowids,]$QTR <- 0

    message(paste0("Combine all QTRs in ",targetYr,". Relabel QTR = 0"))
    write_to_logfile(outputDir,logfile,data=paste0("Combine all QTRs in ",targetYr,". Relabel QTR = 0"),label=NULL)
  }

  landingsData <- data$landings %>%
    dplyr::group_by(.data$YEAR,.data$QTR,.data$NEGEAR,.data$MARKET_CODE) %>%
    dplyr::summarise(landings_land=sum(.data$landings_land, na.rm=T),
                     landings_nn=sum(.data$landings_nn, na.rm=T),
                     len_totalNumLen=sum(.data$len_totalNumLen,na.rm=T),
                     len_numLengthSamples=sum(.data$len_numLengthSamples, na.rm=T),
                     .groups="drop")
  lengthData <- data$lengthData %>%
    dplyr::group_by(.data$YEAR,.data$QTR,.data$NEGEAR,.data$MARKET_CODE,.data$LENGTH) %>%
    dplyr::summarise(NUMLEN = sum(as.numeric(.data$NUMLEN),na.rm=T),.groups="drop")

  data$landings <- landingsData
  data$lengthData <- lengthData

  return(data)
}
