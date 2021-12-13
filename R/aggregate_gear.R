#' Aggregates landings and length data based on gear
#'
#' Selects unique gear types that comprise a certain percentage of total landings. All landings by other gear types are deemed minor
#'  and thus relabeled as "other gear" category. The percentage is a function argument
#'
#'@param data List. Landings data and length data
#'@param recodeOtherGear Numeric scalar. Arbitrary code to use for "other gears" that contribute little to landings
#'@param landingsThresholdGear Numeric scalar. (Proportion). Determines the proportion of landings that have to be satisfied before all other landings by gear are lumped into "other gear" category.
#'This is passed via the main function (\code{aggregate_landings})
#'
#'@return List
#'
#'\item{landings}{same as input}
#'\item{lengthData}{Same as input}
#'
#'@noRd

aggregate_gear <- function(data,recodeOtherGear,landingsThresholdGear,speciesName,logfile,outputDir,outputPlots) {

  landings <- data$landings
  lengthData <- data$lengthData
  # 1 . combine gears. look at gears contributing to top threshold % of landings
  numGears <- length(unique(landings$NEGEAR))

  # find gears that make up "threshold" % of catch overall. Valid assumption?
  # totl landings by group
  aggTopPercent <- landings %>%
    group_by(NEGEAR) %>%
    summarise(totalLandings = sum(landings_land, na.rm = TRUE)) %>%
    arrange(desc(totalLandings))

  plot_landings_by_gear(speciesName,landings,1,outputPlots,outputDir,"1b")

  # convert to % of total and reorder
  aggTopPercent <- mutate(aggTopPercent,cum_sum=cumsum(totalLandings),percent=cum_sum/sum(totalLandings))
  print(aggTopPercent)

  write_to_logfile(outputDir,logfile,as.data.frame(aggTopPercent),label="Landings by gear type (NEGEAR):",append = T)

  # png("test.png",height = 1000*nrow(aggTopPercent), width = 800*ncol(aggTopPercent))
  # gridExtra::grid.table(aggTopPercent)
  # dev.off()

  # select the gear that make up at least threshold %
  nGearsChosen <- dim(aggTopPercent %>% filter(percent<= landingsThresholdGear) %>% select(NEGEAR))[1]
  gearsChosen <- aggTopPercent$NEGEAR[1:(nGearsChosen + 1)]
  filteredLandings <- landings %>% filter(NEGEAR %in% gearsChosen)
  theRestLandings <- landings %>% filter(!(NEGEAR %in% gearsChosen))

  print(nGearsChosen)
  print(gearsChosen)

  # print(aggTopPercent[1:(nGearsChosen+2),])
  # png("test2.png",height = 800, width = 800)
  # gridExtra::grid.table(aggTopPercent[1:(nGearsChosen+2),])
  # dev.off()
  #########################################################################################################################
  # At this point may need to examine lengths distribution to determine further grouping (plots of length distributions)
  #########################################################################################################################
  # aggregate all the other fleets to one fleet
  theRestLandings$NEGEAR <- recodeOtherGear # need to make sure we pick an unused code
  theRestLandings <- theRestLandings %>% group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
    summarize(landings_land=sum(landings_land,na.rm=T),landings_nn=sum(landings_nn,na.rm=T),len_totalNumLen= sum(len_totalNumLen,na.rm=T),len_numLengthSamples=sum(len_numLengthSamples,na.rm=T))

  # concatenate 2 data frames
  filteredLandings <- rbind(filteredLandings,as.data.frame(theRestLandings))

  plot_landings_by_gear(speciesName,filteredLandings,landingsThresholdGear,outputPlots,outputDir,"1c")

  # update sample lengthsData to reflect gear aggregation
  lengthData$NEGEAR[!(lengthData$NEGEAR  %in% gearsChosen)] <- recodeOtherGear

  aggregatedData <- list()
  aggregatedData$landings <- filteredLandings
  aggregatedData$lengthData <- lengthData

  return(aggregatedData)
}
