#' Pull a sample data set
#'
#' Data in the format needed for processing.
#' Landings are pulled from STOCKEFF database using the tables:
#' "MV_CF_Landings" for landings and
#' "MV_CF_LEN" for lengths
#'
#' THIS IS A SAMPLE DATASET.
#'
#' There are missing Areas in the database which wont be used.
#'
#' There are QTR values coded as zero which wont be used.
#'
#' All of the missing data needs to be dealt with prior to using the processing algorithm
#'
#' To get access to the data base you will need credentials, permissions, and software. See \code{\link[dbutils]{connect_to_database}}
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}.
#'This object is used to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#'@param species Numeric scalar. Species_itis or NESPP3 code. Default = 164744 (147, Haddock)
#'@param species_itis. Boolean . TRUE indicates species code is species_itis, FALSE = nespp3
#'@param area Numeric vector. Statistical areas for which to pull data from. Default = "all"
#'
#'@return A list of two objects
#'
#'\item{landingsData}{Tibble containing landing data in format required for processing}
#'\item{lengthData}{Tibble containing landing data in format required for processing}
#'
#'@examples
#'\dontrun{
#'#Pull all Haddock data
#'channel <- dbutils::connect_to_database("serverName","userName")
#'data <- get_sample_data(channel,species = 164744, area="all")
#'}
#'
#'@export

get_sample_data <- function(channel,species=164744,species_itis = T,area="all"){


  ################ pull sample landings data and massage it
  #############################################################################################################
  message("Pulling landings data from STOCKEFF ...")
  message("This coule take a few minutes ...")

  ## For test data, use raw data. Eventually we will want to use massage it using comlandr
  testDataPullLandings <- cfdbs::get_landings(channel,year="all",area=area,species=species,species_itis=species_itis)

  # summarize landings
  lands <- testDataPullLandings$data %>%
    dplyr::group_by(YEAR, MONTH, NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(landings=sum(as.numeric(SPPLNDLB)),n=dplyr::n(),.groups="drop")
  lands <- dplyr::mutate(lands,QTR = as.character(ceiling(as.numeric(MONTH)/3 )))

  # aggregate landings by variables and count the number of trips
  sampleLandings <- lands %>%
    dplyr::group_by(YEAR,QTR,NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(landings_land = sum(landings),landings_nn=sum(n),.groups="drop")

  # filter all entries labeled, QTR = 0
  sampleLandings <- sampleLandings %>%
    dplyr::select_all() %>%
    dplyr::filter(QTR != "0")

  #############################################################################################################
  # pull sample length data and massage it
  message("Puling length data ...")
  testDataPullLength <- cfdbs::get_landings_length(channel,year="all",area=area,species=species,species_itis=species_itis)
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

  sampleLengths <- dplyr::as_tibble(sampleLengths) %>%
    dplyr::select(-tripid)
  sampleData <- dplyr::as_tibble(sampleData)

  sampleData <- list(lengthData=sampleLengths,landingsData=sampleData)

  return(sampleData)


}
