#' Test aggregation
#'
#'This will all change so no point documenting it too much.
#'Eventually split each section into its own function for easier future development
#'All plotting functions will need to be finined/generalized
#'Also need to adapt code to allow user to process on a year by year basis
#'
#'
#'@param landingsThresholdGear numeric scalar (proportion). Minimum proportion of cumulative landings to avoid aggregation of gear. Default = .9
#'@param nLengthSamples numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Dfault = 1
#'@param pValue numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F (Default = F)
#'@param logFile character string. Specify the name for the log file generated describing all decisions made.
#'
#'@importFrom dplyr "summarize" "summarise" "group_by" "filter" "select" "arrange" "mutate"
#'@importFrom magrittr "%>%"
#'
#'@export

#channel <- cfdbs::connect_to_database("sole","abeet") #eventually remove this

test_aggregation <- function(landingsThresholdGear = .90, nLengthSamples = 1, pValue = 0.05, outputDir=here::here("output"), outputPlots=F, logfile="logFile.txt") {

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

  # concatenate 2 data frames
  filteredLandings <- rbind(filteredLandings,as.data.frame(theRestLandings))

  # update sample lengthsData to reflect gear aggregation
  lengthData$NEGEAR[!(lengthData$NEGEAR  %in% gearsChosen)] <- recodeOtherGear

  # look at the summary stats/plots after aggregation
  summary_stats(filteredLandings,species_itis,outputDir,outputPlots)
  # take a look at length distribution of size categories
  plot_length_histogram(lengthData,species_itis,outputDir,outputPlots)
  ############################################################################################################
  ####### GEARs DONE . move to own function ####################################
  #############################################################################################################

  # 2 . combine market category (This will be difficult) market category description are unique to species and not ordinal.
  # Use distributions to aggregate instead of Market category
  # Need to keep Unclassified and Unknown categories separate from known market categories

  # find market_codes contribution to landings
  market <- filteredLandings %>%
    group_by(MARKET_CODE) %>%
    summarise(totalLandings = sum(landings_land, na.rm = TRUE),len_numLengthSamples=sum(len_numLengthSamples,na.rm=T)) %>%
    arrange(desc(totalLandings))
  market <- dplyr::mutate(market,percent = totalLandings/sum(totalLandings) , cumsum=cumsum(totalLandings),cum_percent=cumsum/sum(totalLandings))
  # plot market data
  plot_market_codes(market,outputDir,outputPlots)

  # IF NO LENGTH SAMPLES AT ALL FOR ANY MARKET CODE LUMP LANDINGS INTO NEIGHBORING SIZECLASS
  # this is subjective. If landings are minimal, not really an issue, if substantial??
  if (any (market$len_numLengthSamples == 0)) {
    message("Some market codes have NO length samples. Below is a table of market codes and their relative contribution to landings \n")
    print(market)
    zeroLandings <- market$MARKET_CODE[market$len_numLengthSamples == 0]
    message(paste0("As you can see, code = ",as.character(zeroLandings)," need to be combined with other market categories. \n" ))
    message("We'll walk through each one in turn:\n")
    mapCodes <- NULL
    for (acode in zeroLandings) {
      newCode <- readline(prompt=paste0("Which Market category should we combine with ",acode,"?: \n"))
      message(paste0("OK. We will combine ",acode, " with ",newCode))
      mapCodes <- rbind(mapCodes,c(acode,newCode))
      # rename MARKET_CODES
      print(sum(filteredLandings$MARKET_CODE == acode))
      filteredLandings$MARKET_CODE[filteredLandings$MARKET_CODE == acode] <- newCode
    }
    write_to_logfile(outputDir,logfile,mapCodes,label="market code relabelling")

    newmarket <- filteredLandings %>%
      group_by(MARKET_CODE) %>%
      summarise(totalLandings = sum(landings_land, na.rm = TRUE),len_numLengthSamples=sum(len_numLengthSamples,na.rm=T)) %>%
      arrange(desc(totalLandings)) %>%
      mutate(percent = totalLandings/sum(totalLandings) , cumsum=cumsum(totalLandings),cum_percent=cumsum/sum(totalLandings))

    print(newmarket)
  }

  # now test similarities of market codes length distributions
  codesToAggregate <- compare_length_distributions(filteredLandings,lengthData,"MARKET_CODE",pValue,outputDir,logfile)
  print(codesToAggregate)

  filteredLandings <- aggregate_data_by_class(filteredLandings,variable="MARKET_CODE",codesToAggregate,dataset="landings")
  lengthData <- aggregate_data_by_class(lengthData,variable="MARKET_CODE",codesToAggregate,dataset="lengths")


  # if market caegory has landings but no length data at all. Then the landings need to be lumped into a
  # neighboring size class. Very subjective but dont lump into unclassified/ unknown







  return(list(f =filteredLandings,l=lengthData))


}
