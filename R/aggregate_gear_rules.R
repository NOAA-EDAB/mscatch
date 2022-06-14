#' Aggregates landings and length data based on user supplied gear types
#'
#' Aggregates landings by gear types as speciefied by users speciesObject
#'
#'@param data List. Landings data and length data
#'@param speciesObject List. Defines Rules of aggregation
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F (Default = F)
#'@param logFile character string. Specify the name for the log file generated describing all decisions made.
#'
#'
#'@return List
#'
#'\item{landings}{same as input}
#'\item{lengthData}{Same as input}
#'
#'@noRd

aggregate_gear_rules <- function(data,speciesObject,logfile,outputDir,outputPlots) {

  landings <- data$landings
  lengthData <- data$lengthData

  ## tally landings by gear type for logfile
  aggTopPercent <- landings %>%
    dplyr::group_by(NEGEAR) %>%
    dplyr::summarise(totalLandings = sum(landings_land, na.rm = TRUE),.groups="drop") %>%
    dplyr::arrange(desc(totalLandings)) %>%
    dplyr::mutate(cum_sum=cumsum(totalLandings),percent=cum_sum/sum(totalLandings))

  print(aggTopPercent)

  plot_landings_by_type(speciesObject$speciesName,landings,1,outputPlots,outputDir,"1a",type="gear")
  plot_lengths_by_type(speciesObject$speciesName,landings,1,outputPlots,outputDir,"1c",type="gear")

  write_to_logfile(outputDir,logfile,as.data.frame(aggTopPercent),label="Landings by gear type (NEGEAR):",append = T)


  ## Recode NEGEARs based on user object preferences
  gearCodes <- speciesObject$gearCodes

  if (nrow(gearCodes) == 1) {
    # rename all NEGEAR codes to a single gear code
    landings$NEGEAR <- gearCodes$use
    lengthData$NEGEAR <- gearCodes$use

    write_to_logfile(outputDir,logfile,"",label=paste0("All NEGEAR codes assigned code = ",gearCodes$use),append = T)

  } else {
    # find all Gear codes
    allGearCodes <- unique(landings$NEGEAR)
    codesToAggregate <- setdiff(allGearCodes,gearCodes$combine)
    # loop over all pairs of codes
    for (igear in 1:nrow(gearCodes)) {
      use <- gearCodes$use[igear]
      combine <- gearCodes$combine[igear]

      if (combine != "all") { # recode specific codes
        landings <- landings %>%
          dplyr::mutate(NEGEAR = dplyr::case_when(NEGEAR == combine ~ use, TRUE ~ NEGEAR))
        lengthData <- lengthData %>%
          dplyr::mutate(NEGEAR = dplyr::case_when(NEGEAR == combine ~ use, TRUE ~ NEGEAR))

      } else { # assigns the rest

        landings <- landings %>%
          dplyr::mutate(NEGEAR = dplyr::case_when(NEGEAR %in% codesToAggregate ~ use, TRUE ~ NEGEAR))
        lengthData <- lengthData %>%
          dplyr::mutate(NEGEAR = dplyr::case_when(NEGEAR %in% codesToAggregate ~ use, TRUE ~ NEGEAR))
      }

      write_to_logfile(outputDir,logfile,"",label=paste0("NEGEAR: ",combine," assigned code = ",use),append = T)
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

  plot_landings_by_type(speciesObject$speciesName,landings,1,outputPlots,outputDir,"1b",type="gear")
  plot_lengths_by_type(speciesObject$speciesName,landings,1,outputPlots,outputDir,"1d",type="gear")


  aggTopPercent <- landings %>%
    dplyr::group_by(NEGEAR) %>%
    dplyr::summarise(totalLandings = sum(landings_land, na.rm = TRUE)) %>%
    dplyr::arrange(desc(totalLandings)) %>%
    dplyr::mutate(cum_sum=cumsum(totalLandings),
                  percent=cum_sum/sum(totalLandings))
  print(aggTopPercent)

  write_to_logfile(outputDir,logfile,as.data.frame(aggTopPercent),label="Landings by selected gear type (NEGEAR):",append = T)



  # update sample lengthsData to reflect gear aggregation
  aggregatedData <- list()
  aggregatedData$landings <- landings
  aggregatedData$lengthData <- lengthData

  return(aggregatedData)
}
