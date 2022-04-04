#' combine SEMESTERs in given year
#'
#' Combine landings and length samples
#'
#' @param iyear Numeric scalar. YEAR in which length data are missing and need to be replaced
#' @param targetYr Numeric scalar. YEAR in which landings and lengths are combined
#' @param gearType Character string. Gear types. Either NEGEAR or custom recoded values
#' @param marketCode Character string. Market code designation
#' @param outputDir Character string. Path to output directory (png files saved here)
#' @param logFile Character string. Specify the name for the log file generated describing all decisions made.
#'
#' @return
#'\item{numSamples}{tibble (nx4). Columns =  YEAR, SEMESTER, len_totalNumLen, len_numLengthSamples}
#'
#'@noRd

missing_length_by_semester_combine <- function(data,targetYr,gearType,marketCode,outputDir,logfile) {

  # landings rename SEMESTER to 0
  rowids <- (data$landings$YEAR == targetYr) &
    (data$landings$NEGEAR == gearType) &
    (data$landings$MARKET_CODE == marketCode)
  # fill in missing data
    data$landings[rowids,]$SEMESTER <- 0

  # lengths rename SEMESTER to 0
  rowids <- (data$lengthData$YEAR == targetYr) &
    (data$landings$NEGEAR == gearType) &
    (data$landings$MARKET_CODE == marketCode)
  # fill in missing data
  # relabel SEMESTERS to 0 to indicate Annual
  data$lengthData[rowids,]$SEMESTER <- 0


  # aggregate unclassifieds to same level

  # landings rename SEMESTER to 0
  rowids <- (data$landings$YEAR == targetYr) &
    (data$landings$NEGEAR == gearType) &
    (data$landings$MARKET_CODE == "UN")
  # fill in missing data
  data$landings[rowids,]$SEMESTER <- 0

  # lengths rename SEMESTER to 0
  rowids <- (data$lengthData$YEAR == targetYr) &
    (data$landings$NEGEAR == gearType) &
    (data$landings$MARKET_CODE == "UN")

  data$lengthData[rowids,]$SEMESTER <- 0


  message(paste0("Combine all SEMESTERs in ",targetYr,". Relabel SEMESTER = 0"))
  write_to_logfile(outputDir,logfile,data=paste0("Combine all SEMESTERs in ",targetYr,". Relabel SEMESTER = 0"),label=NULL)

  landingsData <- data$landings %>%
    dplyr::group_by(.data$YEAR,.data$SEMESTER,.data$NEGEAR,.data$MARKET_CODE) %>%
    dplyr::summarise(landings_land=sum(.data$landings_land, na.rm=T),
                     landings_nn=sum(.data$landings_nn, na.rm=T),
                     len_totalNumLen=sum(.data$len_totalNumLen,na.rm=T),
                     len_numLengthSamples=sum(.data$len_numLengthSamples, na.rm=T),
                     .groups="drop")
  lengthData <- data$lengthData %>%
    dplyr::group_by(.data$YEAR,.data$SEMESTER,.data$NEGEAR,.data$MARKET_CODE,.data$LENGTH) %>%
    dplyr::summarise(NUMLEN = sum(as.numeric(.data$NUMLEN),na.rm=T),.groups="drop")

  data$landings <- landingsData
  data$lengthData <- lengthData

  return(data)
}
