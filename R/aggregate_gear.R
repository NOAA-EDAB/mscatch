#' Aggregates landings and length data based on gear
#'
#'
#'
#'@param data List. Landings data and length data
#'@param recodeOtherGear Numeric scalar. Arbitrary code to use for "other gears" that contribute little to landings
#'@param landingsThresholdGear Numeric scalar. (Proportion). Determins the proportion of landings that have to be satisfied before all other landings by gear are lumped into "other gear" category
#'
#'@return List
#'
#'\item{landings}{same as input}
#'\item{lengthData}{Same as input}
#'
#'
#'@export

aggregate_gear <- function(data,recodeOtherGear,landingsThresholdGear) {

  landings <- data$landings
  lengthData <- data$lengthData
  # 1 . combine gears. look at gears contributing to top threshold % of landings
  numGears <- length(unique(landings$NEGEAR))

  # find gears that make up "threshold" % of catch overall. Valid assumption?
  # totl landings by group
  aggTopPercent <- landings %>% group_by(NEGEAR) %>% summarise(totalLandings = sum(landings_land, na.rm = TRUE)) %>% arrange(desc(totalLandings))

  # convert to % of total and reorder
  aggTopPercent <- mutate(aggTopPercent,cumsum=cumsum(totalLandings),percent=cumsum/sum(totalLandings))
  # select the gear that make up at least threshold %
  nGearsChosen <- dim(aggTopPercent %>% filter(percent<= landingsThresholdGear) %>% select(NEGEAR))[1]
  gearsChosen <- aggTopPercent$NEGEAR[nGearsChosen + 1]
  filteredLandings <- landings %>% filter(NEGEAR %in% gearsChosen)
  theRestLandings <- landings %>% filter(!(NEGEAR %in% gearsChosen))
  #########################################################################################################################
  # At this point may need to examine lengths distribution to determine further grouping (plots of length distributions)
  #########################################################################################################################
  # aggregate all the other fleets to one fleet
  theRestLandings$NEGEAR <- recodeOtherGear # need to make sure we pick an unused code
  theRestLandings <- theRestLandings %>% group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
    summarize(landings_land=sum(landings_land),landings_nn=sum(landings_nn),len_totalNumLen= sum(len_totalNumLen),len_numLengthSamples=sum(len_numLengthSamples))

  # concatenate 2 data frames
  filteredLandings <- rbind(filteredLandings,as.data.frame(theRestLandings))

  # update sample lengthsData to reflect gear aggregation
  lengthData$NEGEAR[!(lengthData$NEGEAR  %in% gearsChosen)] <- recodeOtherGear

  aggregatedData <- list()
  aggregatedData$landings <- filteredLandings
  aggregatedData$lengthData <- lengthData

  return(aggregatedData)
}
