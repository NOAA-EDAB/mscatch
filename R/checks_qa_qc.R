#' Aggregates data to Time interval and summarizes
#'
#' groups the data by time and summarizes, landings and length sample data after removing NA's
#'
#'@param landingsData Tidy data frame. Landings by YEAR,QTR,NEGEAR,MARKET_CODE,landings_land,landings_nn,len_totalNumLen,len_numLengthSampls
#'@param lengthData Tidy data frame. Length data by YEAR,QTR,NEGEAR,MARKET_CODE, LENGTH, NUMLEN
#'@param speciesName Character string. speciesName for data used. (This is used in plotting only)
#'@param aggregate_to Character string. Level of aggregation for all MARKET_CODES and gears ("QTR", "YEAR", "SEMESTER", MIX").
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param logfile Character string. Specify the name for the log file generated describing all decisions made.

#'@return List
#'
#'\item{landings}{same as input}
#'\item{lengthData}{Same as input}
#'
#'@noRd

checks_qa_qc <- function(landingsData, lengthData, speciesName, aggregate_to, outputDir,logfile) {

  if (!(aggregate_to %in% c("YEAR","QTR","MIX","SEMESTER"))) {
    stop(paste0("Aggregation to ",aggregate_to," is not currently implemented. Please create an issue
                at https://github.com/NOAA-EDAB/mscatch/issues"))

  }

  write_to_logfile(outputDir,logfile,"",label="DECISIONS MADE DURING AGGREGATION OF DATA")
  write_to_logfile(outputDir,logfile,data=as.character(speciesName),label="Species Name:",append=T)

  # cleans landings data and length data of NAs
  landingsData <- landingsData %>%
    dplyr::group_by(.data$YEAR,.data$QTR,.data$NEGEAR,.data$MARKET_CODE) %>%
    dplyr::summarise(landings_land=sum(.data$landings_land, na.rm=T),
                     landings_nn=sum(.data$landings_nn, na.rm=T),
                     len_totalNumLen=sum(.data$len_totalNumLen,na.rm=T),
                     len_numLengthSamples=sum(.data$len_numLengthSamples, na.rm=T),
                     .groups="drop")
  lengthData <- lengthData %>%
    dplyr::group_by(.data$YEAR,.data$QTR,.data$NEGEAR,.data$MARKET_CODE,.data$LENGTH) %>%
    dplyr::summarise(NUMLEN = sum(as.numeric(.data$NUMLEN),na.rm=T),.groups="drop")


  # Check for QTRs labelled anythign other that 1:4
  # Check for NEGEARs that are not 3 character digits
  # Check for unusually large lengths




  # create list for data.
  # landing and lengths will from this point on be parts of a list.
  data <- list()
  data$landings <- landingsData
  data$lengthData <- lengthData

  return(data)
}
