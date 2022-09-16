#' Pull species data from cfdbs
#'
#' Pulls data from cfdbs via \code{\link{[comlandr]}}
#' Palmer method applied to assign missing attributes (NEGEAR, QTR, AREA) to landings records
#'
#'@param species_itis Numeric. Itis code for species (Default = NA, Returns a NA object for user to define)
#'@param stock Character string. Define the stock associated with species of interest.
#'eg. for winter flounder stock = "GB" or "SNEMA". For black sea bass, "North" or "South"
#'@param outPath Character string. Path to directory where data will be saved
#'@param fileName character string. File name of data to be saved
#'
#'@return List. Data is also written to disk.
#'
#'\item{landins}{Data frame containing landings data by YEAR, QTR, NEGEAR, MARKET_CODE}
#'\item{lengths}{Data frame containing fish lengths by YEAR, QTR, NEGEAR, MARKET_CODE }
#'
#'
#'
#'@export

get_species_data <- function(species_itis = NULL, stock = NULL, outPath= here::here(), fileName = NULL) {

  if (is.null(species_itis)) {
    stop(paste0("Please supply a species itis code"))
  }

  #speciesRules <- get_species_object(species_itis)

  # pull data from comlandr over spatial unit of interest, eg GB. Include discards



  # Rename columns of interest, eg MARKET_CODE, NEGEAR etc

  # aggregate months to QTRs or SEMESTERs

  # aggregate landings by YEAR, QTR, NEGEAR, MARKET_CODE

  # pull length and age data over entire stock area (since all will be valid)

  # format the data in the format required for mscatch and save to machine

  if (is.null(fileName)) {
    saveRDS(data,file = paste0(outPath,"/","speciesData_",species_itis,".rds"))
  } else {
    saveRDS(data,file = paste0(outPath,"/",fileName))
  }


  return(data)

}

