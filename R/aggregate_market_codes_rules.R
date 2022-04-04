#' Aggregates landings and length data based on MARKET_CODE
#'
#' Aggregates data by MARKET_CODE. User intervention may be required since this is subjective.
#'
#'
#'@param data List. Landings data and length data
#'@param speciesObject List. Defines Rules of aggregation
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F (Default = F)
#'@param logFile character string. Specify the name for the log file generated describing all decisions made.
#'
#'@return List
#'
#'\item{landings}{same as input}
#'\item{lengthData}{Same as input}
#'
#'@noRd


aggregate_market_codes_rules <- function(data,speciesObject,outputDir,outputPlots,logfile) {


  landings <- data$landings
  lengthData <- data$lengthData

  # group by code to look at total landings by market code
  market <- landings %>%
    dplyr::group_by(MARKET_CODE) %>%
    dplyr::summarise(totalLandings = sum(landings_land, na.rm = TRUE),
                     len_numLengthSamples=sum(len_numLengthSamples,na.rm=T)) %>%
    dplyr::arrange(desc(totalLandings)) %>%
    dplyr::mutate(percent = totalLandings/sum(totalLandings),
                  cum_percent=cumsum(percent)) %>%
    dplyr::ungroup()

  print(market)

  plot_landings_by_type(speciesObject$speciesName,landings,1,outputPlots,outputDir,"2a",type="market")
  plot_lengths_by_type(speciesObject$speciesName,landings,1,outputPlots,outputDir,"2c",type="market")

  plot_market_codes(market,7,outputDir,outputPlots)

  ## Group based on user preference
  ## Recode MARKET_CODEs based on user object preferences
  marketCodes <- speciesObject$marketCodes
  if (nrow(marketCodes) == 1)  {
    # rename all NEGEAR codes to a single gear code
    landings$MARKET_CODE <- marketCodes$use
    lengthData$MARKET_CODE <- marketCodes$use

    write_to_logfile(outputDir,logfile,"",label=paste0("All MARKET_CODEs assigned code = ",marketCodes$use),append = T)

  } else {
    # find all Gear codes
    allMarketCodes <- unique(landings$MARKET_CODE)
    codesToAggregate <- setdiff(allMarketCodes,marketCodes$combine)
    # loop over all pairs of codes
    for (icode in 1:nrow(marketCodes)) {
      use <- marketCodes$use[icode]
      combine <- marketCodes$combine[icode]

      if (combine != "all") { # recode specific codes
        landings <- landings %>%
          dplyr::mutate(MARKET_CODE = dplyr::case_when(MARKET_CODE == combine ~ use, TRUE ~ MARKET_CODE))
        lengthData <- lengthData %>%
          dplyr::mutate(MARKET_CODE = dplyr::case_when(MARKET_CODE == combine ~ use, TRUE ~ MARKET_CODE))

      } else { # assigns the rest

        landings <- landings %>%
          dplyr::mutate(MARKET_CODE = dplyr::case_when(MARKET_CODE %in% codesToAggregate ~ use, TRUE ~ MARKET_CODE))
        lengthData <- lengthData %>%
          dplyr::mutate(MARKET_CODE = dplyr::case_when(MARKET_CODE %in% codesToAggregate ~ use, TRUE ~ MARKET_CODE))
      }

      write_to_logfile(outputDir,logfile,"",label=paste0("MARKET_CODE: ",combine," assigned code = ",use),append = T)
    }
  }



  ## update landings and lengths
  landings <- landings %>%
    group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
    summarize(landings_land=sum(landings_land,na.rm=T),
              landings_nn=sum(landings_nn,na.rm=T),
              len_totalNumLen= sum(len_totalNumLen,na.rm=T),
              len_numLengthSamples=sum(len_numLengthSamples,na.rm=T),
              .groups = "drop")


  lengthData<- lengthData %>%
    dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH) %>%
    dplyr::summarise(NUMLEN = sum(as.numeric(NUMLEN),na.rm=T),
                     .groups="drop")

  plot_landings_by_type(speciesObject$speciesName,landings,1,outputPlots,outputDir,"2b",type="market")
  plot_lengths_by_type(speciesObject$speciesName,landings,1,outputPlots,outputDir,"2d",type="market")

  png(paste0(outputDir,"/6a_market_category_lengths.png"))
  lengthDataGEARS <- lengthData %>%
    dplyr::group_by(NEGEAR,MARKET_CODE,LENGTH) %>%
    dplyr::summarise(numlens=sum(as.numeric(NUMLEN)))
  p <- ggplot(data = lengthDataGEARS) +
    geom_bar(stat="identity",mapping = aes(x=LENGTH,y=numlens),na.rm=T) +
    facet_wrap(~MARKET_CODE,scales="free_y",nrow = length(unique(lengthData$MARKET_CODE)),ncol=1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_discrete(name="Length (cm)", breaks=seq(0, max(lengthData$LENGTH),10))
  print(p)

  dev.off()

  newmarket <- landings %>%
    dplyr::group_by(MARKET_CODE) %>%
    dplyr::summarise(totalLandings = sum(landings_land, na.rm = TRUE),len_numLengthSamples=sum(len_numLengthSamples,na.rm=T)) %>%
    dplyr::arrange(desc(totalLandings)) %>%
    dplyr::mutate(percent = totalLandings/sum(totalLandings) , cumsum=cumsum(totalLandings),cum_percent=cumsum/sum(totalLandings))

  message("New market code aggregation")
  print(newmarket)
  plot_market_codes(newmarket,8,outputDir,outputPlots)

  write_to_logfile(outputDir,logfile,as.data.frame(newmarket),label="Relabelled landings and length samples by MARKET_CODE:",append = T)

  # update sample lengthsData to reflect gear aggregation
  aggregatedData <- list()
  aggregatedData$landings <- landings
  aggregatedData$lengthData <- lengthData

  return(aggregatedData)

}
