#' sample Data pull
#'
#'All missing areas need to dealt with prior to pulling species data.
#'Not the case in this sample. We aggregate all data as if from one EPU
#'
#'@param channel obtained from logging into the sole database.
#'@param species nespp3 code. default = 212 (atlantic mackerel)
#'
#'
#' @importFrom magrittr "%>%"
#'
#'@export
#'
#channel <- cfdbs::connect_to_database("sole","abeet") eventually remove this

test_data_pull <- function(channel,species=212){

  # pull sample landings data and massage it
  testDataPullLandings <- cfdbs::get_landings(channel,year="all",species=species)
  ## for test data assume we this is EPU data. To achive this we just sum over AREAS for now
  lands <- testDataPullLandings$data %>% dplyr::group_by(YEAR, MONTH, NEGEAR, MARKET_CODE) %>% dplyr::summarize(landings=sum(as.numeric(SPPLNDLB)),n=n())
  lands <- dplyr::mutate(lands,qtr = ceiling(as.numeric(MONTH)/3 ))
  sampleLandings <- lands %>% dplyr::group_by(YEAR,qtr,NEGEAR, MARKET_CODE) %>% dplyr::summarize(land = sum(landings),nn=sum(n))

  # pull sample length data and massage it
  testDataPullLength <- cfdbs::get_landings_length(channel,year="all",species=species)
  lengths <- dplyr::mutate(testDataPullLength$data,tripid = paste0(PERMIT,YEAR,MONTH,DAY)) # create unique tripid
  sampleLengths <- lengths %>% dplyr::group_by(YEAR, QTR, NEGEAR, MARKET_CODE) %>% dplyr::summarize(totalNumLen=sum(as.numeric(NUMLEN)),numLengthSamples=length(unique(tripid)))


  nYrs <- length(unique(sampleData$YEAR))
  nGrs <- length(unique(sampleData$NEGEAR))
  nMCodes <- length(unique(sampleData$MARKET_CODE))
  # A fully represented grid has nRows
  nRows <- prod(nYrs,nGrs,nMCodes,4)
  print(paste0("Proportion of cell represented = ",dim(sampleData)[1]/nRows))

  # save data
  save(sampleLengths,sampleLandings,file=paste0(here::here("data"),"/sampleData_",species,".RData"))




#  return(sampleData)


}
