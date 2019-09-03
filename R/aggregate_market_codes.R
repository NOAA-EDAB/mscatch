#' Aggregates landings and length data based on MARKET_CODE
#'
#' Aggregates data by MARKET_CODE. User intervention is required since this is prety subjective.
#'
#'
#'@param data List. Landings data and length data
#'@param pValue numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F (Default = F)
#'@param logFile character string. Specify the name for the log file generated describing all decisions made.
#'
#'@return List
#'
#'\item{landings}{same as input}
#'\item{lengthData}{Same as input}
#'


aggregate_market_codes <- function(data,pValue,outputDir,outputPlots,logfile) {

  # group by code to look at total landings by market code
  market <- data$landings %>%
    group_by(MARKET_CODE) %>%
    summarise(totalLandings = sum(landings_land, na.rm = TRUE),len_numLengthSamples=sum(len_numLengthSamples,na.rm=T)) %>%
    arrange(desc(totalLandings))
  market <- dplyr::mutate(market,percent = totalLandings/sum(totalLandings) , cumsum=cumsum(totalLandings),cum_percent=cumsum/sum(totalLandings))

  plot_market_codes(market,7,outputDir,outputPlots)


  # IF NO LENGTH SAMPLES AT ALL FOR ANY MARKET CODE LUMP LANDINGS INTO NEIGHBORING SIZECLASS
  # this is subjective. If landings are minimal, not really an issue, if substantial??
  # nEED TO RESTRICT USER TO ONLY MARKET CODES EXISTING IN THE DATA
  if (any (market$len_numLengthSamples == 0)) {
    message("Some market codes have NO length samples. Below is a table of market codes and their relative contribution to landings \n")
    print(market)
    zeroLandings <- market$MARKET_CODE[market$len_numLengthSamples == 0]
    message(paste0("As you can see, code = ",as.character(zeroLandings)," need to be combined with other market categories. \n" ))
    message("We'll walk through each one in turn:\n")
    mapCodes <- NULL
    for (acode in zeroLandings) {
      newCode <- readline(prompt=paste0("Which Market category should we combine with ",acode,"?: \n"))
      message(paste0("OK. We will combine ",sum(data$landings$MARKET_CODE == acode)," records for ",acode, " with ",newCode))
      mapCodes <- rbind(mapCodes,c(acode,newCode))
      # rename MARKET_CODES in landings data
      data$landings$MARKET_CODE[data$landings$MARKET_CODE == acode] <- newCode
      # since this if for MARKET_CODES without any length samples, lengthData is unaffected
    }

    write_to_logfile(outputDir,logfile,mapCodes,label="market code relabelling, from:to",append=T)

    newmarket <- data$landings %>%
      group_by(MARKET_CODE) %>%
      summarise(totalLandings = sum(landings_land, na.rm = TRUE),len_numLengthSamples=sum(len_numLengthSamples,na.rm=T)) %>%
      arrange(desc(totalLandings)) %>%
      mutate(percent = totalLandings/sum(totalLandings) , cumsum=cumsum(totalLandings),cum_percent=cumsum/sum(totalLandings))

  }
  plot_market_codes(newmarket,8,outputDir,outputPlots)

  # now test for equality of length distributions.
  # aggregate until can no longer
  message("Performing KS tests to compare length distributions of market codes")
  while (1) {
    codesToAggregate <- compare_length_distributions(data$landings,data$lengthData,variableToAggregate = "MARKET_CODE", groupBy=c("NEGEAR","LENGTH","NUMLEN","MARKET_CODE"), pValue,outputDir,logfile)
    if (is.null(codesToAggregate)) {
      break
    } else {
      codes <- codesToAggregate[1,]
      filteredLandings <- aggregate_data_by_class(data$landings,variable="MARKET_CODE",classes=codes,dataset="landings")
      data$landings <- filteredLandings
      lengthData <- aggregate_data_by_class(data$lengthData,variable="MARKET_CODE",classes=codes,dataset="lengths")
      data$lengthData <- lengthData
    }
  }

  return(data)

}
