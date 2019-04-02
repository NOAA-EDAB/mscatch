#' Test aggregation
#'
#'This will all change so no point documenting it!!
#'
#'
#'
#'
#'
#' @importFrom magrittr "%>%"
# @importFrom ggplot2 "aes" "geom_line" "facet_wrap" "ylab" "xlab"
#'
#'@export
#channel <- cfdbs::connect_to_database("sole","abeet") eventually remove this

test_aggregation <- function(channel){

  # pull sample data and massage it
  testDataPull <- cfdbs::get_landings(channel,year=1994:1998,species=81)
  ## for test data assume we this is EPU data. To achive this we just sum over AREAS for now
  lands <- testDataPull$data %>% dplyr::group_by(YEAR, MONTH, NEGEAR, MARKET_CODE) %>% dplyr::summarize(landings=sum(as.numeric(SPPLNDLB)),n=n())
  land2 <- dplyr::mutate(lands,qtr = ceiling(as.numeric(MONTH)/3 ))
  sampleData <- land2 %>% dplyr::group_by(YEAR,qtr,NEGEAR,MARKET_CODE) %>% dplyr::summarize(land = sum(landings),nn=sum(n))
  #
  nYrs <- length(unique(sampleData$YEAR))
  nGrs <- length(unique(sampleData$NEGEAR))
  nMCodes <- length(unique(sampleData$MARKET_CODE))
  # A fully represented grid has nRows
  nRows <- prod(nYrs,nGrs,nMCodes,4)
  print(paste0("Proportion of cell represented = ",dim(sampleData)[1]/nRows))

  # so how deal with this? Gary's schematic



  return(sampleData)


}
