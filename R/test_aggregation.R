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
  write_to_logfile(outputDir,logfile,"---------------\n",label="DECISIONS MADE DURING AGGREGATION OF DATA")
  write_to_logfile(outputDir,logfile,"164744\n",label="Species_itis",append=T)
  # sample Data is Haddock (147).
  landings <- sampleData_164744            # eventually passed as argument
  lengthData <- sampleLengths_164744      # eventually passed as argument
  recodeOtherGear <- "998"
  species_itis <- 164744 # hard coded. will eventually make this a variable

  data <- list()
  data$landings <- landings
  data$lengthData <- lengthData

  #######################################################
  ####### GEARs #########################################
  #######################################################
  # Now deal with Gary's schematic.
  # 1. aggregate the gears based on landings
  data <- aggregate_gear(data,recodeOtherGear,landingsThresholdGear)
  gearList <- unique(data$landings$NEGEAR)
  # look at the summary stats/plots after aggregation
  summary_stats(data$landings,species_itis,outputDir,outputPlots)
  # take a look at length distribution of size categories
  plot_length_histogram(data$lengthData,species_itis,outputDir,outputPlots)

  #######################################################
  ####### MARKET CODES ##################################
  #######################################################

  # 2 . combine market category (This will be difficult) market category description are unique to species and not ordinal.
  # Use distributions to aggregate instead of Market category
  # Need to keep Unclassified and Unknown categories separate from known market categories
  # if market category has landings but no length data at all. Then the landings need to be lumped into a
  # neighboring size class. Very subjective but dont lump into unclassified/ unknown
  data <- aggregate_market_codes(data,pValue,outputDir,outputPlots,logfile)

  #######################################################
  ####### QTR, SEMESTER, ANNUAL #########################
  #######################################################

  # 3. look at QTR to see if need to lump quarters or borrow from other years
  # plot all diagnostics again with current aggregated data
  plot_market_code_by_qtr(data$landings,9,outputDir,outputPlots)
  write_to_logfile(outputDir,logfile,paste0("Length samples started in ",as.character(min(lengthData$YEAR)),". All landings prior to this year will use this years data \n"),label=NULL,append = T)
  write_to_logfile(outputDir,logfile,"Other gear (code 998) will be aggregated similarly to other gears\n",label="market code by qrt",append = T)

  # Take a look at length distribution by QTR and MARKET CODE
  d <- data$lengthData %>% dplyr::filter(NEGEAR == "050") %>% dplyr::group_by(MARKET_CODE,QTR,LENGTH) %>% summarise(numlens=sum(as.numeric(NUMLEN)))
  p <-   ggplot2::ggplot(data = d) +
    ggplot2::geom_bar(stat="identity",mapping= ggplot2::aes(x=LENGTH,y=numlens),na.rm=T) +
    ggplot2::facet_wrap(~QTR+MARKET_CODE)
  print(p)

  # Need to start in latest year and work backward, filling in for each gear type
  yrsList <- unique(data$landings$YEAR)
  for (gearType in gearList) {
    QTRData <- data$landings %>% dplyr::filter(NEGEAR == gearType & MARKET_CODE !="UN")
    if (gearType == "998") next
    for (ayear in rev(yrsList) ) {
      yrData <- QTRData %>% dplyr::filter(YEAR == ayear)
      if (any(yrData$len_numLengthSamples == 0)) {
        print(ayear)
        print(yrData)
        return(yrData)
        # fill in gaps
      }
    }

    }




  #  write_to_logfile(outputDir,logfile,"Other gear (code 998) will be aggregated similarly to other gears\n",label=NULL,append = T)
  message(paste0("Check the logfile = ",outputDir,"/",logfile," for details regarding aggregation choices") )



  return(data)


}
