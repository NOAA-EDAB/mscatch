#' sample Data pull
#'
#'All missing areas need to dealt with prior to pulling species data.
#'Not the case in this sample. We aggregate all data as if from one EPU
#'
#'@param channel obtained from logging into the sole database.
#'@param species nespp3 code. default = 147 (haddock - should be the easiest with most complete data)
#'
#'@section Other species of interest:
#'212 - atlantic mackerel - (K.Curti assessment)
#'081 - cod (mike palmer assessment)
#'
#'
#' @importFrom magrittr "%>%"
#'
#'@export
#'
#channel <- cfdbs::connect_to_database("sole","abeet") eventually remove this

test_data_pull <- function(channel,species=147){

  # pull sample landings data and massage it
  testDataPullLandings <- cfdbs::get_landings(channel,year="all",species=species)
  ## for test data assume we this is EPU data. To achive this we just sum over AREAS for now
  lands <- testDataPullLandings$data %>% dplyr::group_by(YEAR, MONTH, NEGEAR, MARKET_CODE) %>% dplyr::summarize(landings=sum(as.numeric(SPPLNDLB)),n=n())
  lands <- dplyr::mutate(lands,QTR = as.character(ceiling(as.numeric(MONTH)/3 )))
  sampleLandings <- lands %>% dplyr::group_by(YEAR,QTR,NEGEAR, MARKET_CODE) %>% dplyr::summarize(landings_land = sum(landings),landings_nn=sum(n))

  # pull sample length data and massage it
  testDataPullLength <- cfdbs::get_landings_length(channel,year="all",species=species)
  lengths <- dplyr::mutate(testDataPullLength$data,tripid = paste0(PERMIT,YEAR,MONTH,DAY)) # create unique tripid
  sampleLengths <- lengths %>% dplyr::group_by(YEAR, QTR, NEGEAR, MARKET_CODE) %>% dplyr::summarize(len_totalNumLen=sum(as.numeric(NUMLEN)),len_numLengthSamples=length(unique(tripid)))

  # full join of tables by common fields
  sampleData <- dplyr::full_join(sampleLandings,sampleLengths, by=c("YEAR","QTR","NEGEAR","MARKET_CODE"))

  # save data
  vName <- paste0("sampleData_",species)
  assign(vName,sampleData)
  save(list=vName,file=paste0(here::here("data"),"/sampleData_",species,".RData"))

#  return(sampleData)


}
