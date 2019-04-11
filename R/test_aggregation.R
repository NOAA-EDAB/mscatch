#' Test aggregation
#'
#'This will all change so no point documenting it too much.
#'Eventually split each section into its own function for easier future development
#'
#'
#'@param landingsThreshold numeric scalar (proportion). Minimum proportion of cumulative landings to avoid aggregation of gear. Default = .9
#'@param nLengthSamples numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Dfault = 1
#'
#'@importFrom dplyr "summarize" "summarise" "group_by" "filter" "select" "arrange" "mutate"
#'@importFrom magrittr "%>%"
#'
#'@export

#channel <- cfdbs::connect_to_database("sole","abeet") #eventually remove this

test_aggregation <- function(landingsThreshold = .90, nLengthSamples = 1) {

  # sample Data is Haddock (147).
  landings <- sampleData_147
  speciesNESPP3 <- 147 # hard coded. will eventually make this a variable

  # Now deal with Gary's schematic.
  # 1 . combine gears. look at gears contributing to top threshold % of landings
  numGears <- length(unique(landings$NEGEAR))

  # find gears that make up "threshold" % of catch overall. Valid assumption?
  # totl landings by group
  aggTopPercent <- landings %>% group_by(NEGEAR) %>% summarise(totalLandings = sum(landings_land, na.rm = TRUE)) %>% arrange(desc(totalLandings))
  # convert to % of total and reorder
  aggTopPercent <- mutate(aggTopPercent,cumsum=cumsum(totalLandings),percent=cumsum/sum(totalLandings))
  # select the gear that make up at least threshold %
  nGearsChosen <- dim(aggTopPercent %>% filter(percent<= landingsThreshold) %>% select(NEGEAR))[1]
  gearsChosen <- aggTopPercent$NEGEAR[nGearsChosen + 1]
  filteredLandings <- landings %>% filter(NEGEAR %in% gearsChosen)
  theRestLandings <- landings %>% filter(!(NEGEAR %in% gearsChosen))
  #########################################################################################################################
  # At this point may need to examine lengths distribution to determin if further grouping (plots of length distributions)
  #########################################################################################################################
  # aggregate all the other fleets to one fleet
  theRestLandings$NEGEAR <- "998"
  theRestLandings <- theRestLandings %>% group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
    summarize(landings_land=sum(landings_land),landings_nn=sum(landings_nn),len_totalNumLen= sum(len_totalNumLen),len_numLengthSamples=sum(len_numLengthSamples))
  # join 2 data frames
  filteredLandings <- rbind(filteredLandings,theRestLandings)
  # look at the summary stats after aggregation
  summary_stats(filteredLandings)

  # 2 . combine market category (This will be difficult) market category description are unique to species and not ordinal





  #return(sampleData)


}
