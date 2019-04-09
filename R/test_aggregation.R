#' Test aggregation
#'
#'This will all change so no point documenting it too much
#'
#'@param landingsThreshold vector (length 2). minimum landings (\%) and minimum sample size to avoid combination of data. Default = c(90,1)
#'
#'
#'@importFrom magrittr "%>%"
#'
#'@export

#channel <- cfdbs::connect_to_database("sole","abeet") #eventually remove this

test_aggregation <- function(landingsThreshold=c(90,1)) {

  # sample Data is Cod data. One variable "sampleData"
  #load(paste0(here::here("data"),"/sampleData_147.Rdata"))
  landings <- sampleData_147
  # Now deal with Gary's schematic.
  # 1 . combine gears. look at
  numGears <- length(unique(landings$NEGEAR))

  plot_landings_data(landings,"NEGEAR",landingsThreshold[1])

  # for (ryear in min(sampleData$YEAR):max(sampleData$YEAR)) {
  #
  # }


  # 2 . combine market category (This will be difficult) market category description are unique to species




  #return(sampleData)


}
