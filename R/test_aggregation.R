#' Test aggregation
#'
#'This will all change so no point documenting it too much.
#'Eventually split each section into its own function for easier future development
#'
#'
#'@param landingsThresholdGear numeric scalar (proportion). Minimum proportion of cumulative landings to avoid aggregation of gear. Default = .9
#'@param nLengthSamples numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Dfault = 1
#'@param outputDir Character string. Path to output directory (png files saved here)
#'
#'@importFrom dplyr "summarize" "summarise" "group_by" "filter" "select" "arrange" "mutate"
#'@importFrom magrittr "%>%"
#'
#'@export

#channel <- cfdbs::connect_to_database("sole","abeet") #eventually remove this

test_aggregation <- function(landingsThresholdGear = .90, nLengthSamples = 1, outputDir=here::here("output")) {

  if (!dir.exists(outputDir)){dir.create(outputDir)} # create directory to store exploratory/informational plots
  # sample Data is Haddock (147).
  landings <- sampleData_164744            # eventually passed as argument
  lengthData <- sampleLengths_164744      # eventually passed as argument
  recodeOtherGear <- "998"
  species_itis <- 164744 # hard coded. will eventually make this a variable

  # Now deal with Gary's schematic.
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

  # join 2 data frames
  filteredLandings <- rbind(filteredLandings,as.data.frame(theRestLandings))

  # update sample lengthsData to reflect gear aggregation
  lengthData$NEGEAR[!(lengthData$NEGEAR  %in% gearsChosen)] <- recodeOtherGear



  ####### GEARs DONE . move to own function ####################################

  # 2 . combine market category (This will be difficult) market category description are unique to species and not ordinal.
  # Use distributions to aggregate instead of Market category

  # take a look at length distribution of size categories
  plot_length_histogram(lengthData,species_itis,outputDir)
  # look at the summary stats/plots after aggregation
  summary_stats(filteredLandings,species_itis,outputDir)


  market <- filteredLandings %>% group_by(MARKET_CODE) %>% summarise(totalLandings = sum(landings_land, na.rm = TRUE),len_numLengthSamples=sum(len_numLengthSamples,na.rm=T)) %>% arrange(desc(totalLandings))
  market <- mutate(market,cumsum=cumsum(totalLandings),percent=cumsum/sum(totalLandings))
  # write to a table
  #print(market)






  return(lengthData)


}
