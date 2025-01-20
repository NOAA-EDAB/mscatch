#' Pull species data from cfdbs
#'
#' Pulls data from cfdbs via \code{\link{[comlandr]}}
#' Palmer method applied to assign missing attributes (NEGEAR, QTR, AREA) to landings records
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}.
#' This object is used to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#'@param speciesRules List. Containing species specific rules. Default = NULL (Fully automated).
#'Note:  Predefined \code{speciesRules} will be bundled with the package for select species
#'@param year Numeric vector. Containing the years to search over. Default = "all"
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

get_species_data <- function(channel,speciesRules = NULL, year="all", outPath= here::here(), fileName = NULL) {

  if (is.null(speciesRules)) {
    stop(paste0("Please supply a species rules object"))
  }

  #speciesRules <- get_species_object(species_itis)
  stockArea <- speciesRules$statStockArea
  itis <- speciesRules$species_itis

  ##########################################################################
  ##########################################################################
  ##########################################################################
  ##########################################################################
  # This section needs to be replaced with comlandr pull
  ##########################################################################
  ##########################################################################
  ##########################################################################

  # pull data from comlandr over spatial unit of interest, eg GB. Include discards
  message("Pulling landings data from STOCKEFF ...")
  testDataPullLandings <- cfdbs::get_landings(channel,year="all",area=stockArea,species=itis,species_itis=T)
  ## for test data assume we this is EPU data. To achieve this we just sum over AREAS for now
  lands <- testDataPullLandings$data %>%
    dplyr::group_by(YEAR, MONTH, NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(landings=sum(as.numeric(SPPLNDLB)),n=dplyr::n(),.groups="drop")
  lands <- dplyr::mutate(lands,QTR = as.character(ceiling(as.numeric(MONTH)/3 )))

  # aggregate landings by variables
  sampleLandings <- lands %>%
    dplyr::group_by(YEAR,QTR,NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(landings_land = sum(landings),landings_nn=sum(n),.groups="drop")
  # this needs to be checked.
  # filter all entries labelled quarter = 0
  sampleLandings %>%
    dplyr::filter(QTR == "0") %>%
    print()

  sampleLandings <- sampleLandings %>%
    dplyr::select_all() %>%
    dplyr::filter(QTR != "0")

  # pull sample length data and massage it
  # option to pull lengths for a different spatial area
  message("Pulling length data ...")
  testDataPullLength <- cfdbs::get_landings_length(channel,
                                                   year="all",
                                                   area=stockArea,
                                                   species=itis,
                                                   species_itis=T)
  # create unique tripid since NUMSAMP is replicated for each species reported within a trip
  lengths <- testDataPullLength$data %>%
    dplyr::mutate(tripid = paste0(PERMIT,YEAR,MONTH,DAY))
  # aggregate
  lengthsData <- lengths %>%
    dplyr::group_by(YEAR, QTR, NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(len_totalNumLen=sum(as.numeric(NUMLEN)),len_numLengthSamples=length(unique(tripid)),.groups="drop")

  # full join of tables by common fields
  sampleData <- as.data.frame(dplyr::full_join(sampleLandings,lengthsData, by=c("YEAR","QTR","NEGEAR","MARKET_CODE")))
  sampleData$YEAR <- as.integer(sampleData$YEAR)
  sampleData$QTR <- as.integer(sampleData$QTR)
  # just extract the lengths and the number at length for the year, qr etc
  sampleLengths <- lengths %>% dplyr::select(YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH,NUMLEN,tripid)
  sampleLengths$YEAR <- as.integer(sampleLengths$YEAR)
  sampleLengths$QTR <- as.integer(sampleLengths$QTR)
  sampleLengths$LENGTH <- as.numeric(sampleLengths$LENGTH)
  sampleLengths$NUMLEN <- as.integer(sampleLengths$NUMLEN)

  sampleLengths <- dplyr::as_tibble(sampleLengths)
  sampleData <- dplyr::as_tibble(sampleData)
  data <- list()
  data$sampleLengths <- sampleLengths
  data$sampleData <- sampleData


  ## pull data from comlandr


  ##########################################################################
  ##########################################################################
  ##########################################################################
  ##########################################################################
  # This section needs to be replaced with comlandr pull
  ##########################################################################
  ##########################################################################
  ########################################################################

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

