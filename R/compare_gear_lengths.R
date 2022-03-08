#' Compare length distributions by NEGEAR
#'
#'
#'@param data List. Landings data and length data
#'@param pValue Numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#'@param outputDir Character string. Directory where output is saved
#'@param logfile Character string. Name of logfile
#'
#'@return List of updated landings and length data
#'
#'@noRd


compare_gear_lengths <- function(data,pValue,outputDir,logfile) {


  message("Performing KS tests to compare length distributions between NEGEARs")
  while (1) {
    codesToAggregate <- compare_length_distributions(data$landings,
                                                     data$lengthData,
                                                     variableToAggregate = "NEGEAR",
                                                     groupBy=c("NEGEAR","LENGTH","NUMLEN"),
                                                     pValue,
                                                     outputDir,
                                                     logfile)
    if (is.null(codesToAggregate)) {
      write_to_logfile(outputDir,logfile,data=paste0("All NEGEARs have significantly different length distributions at ",pValue, " level."),label=NULL,append=T)
      break
    } else {
      # Select the first pair of codes, combine codes (using first code),
      # update the landings and lengths, repeat
      # The while loop uses the updated data so will eventually break
      codes <- codesToAggregate[1,]

      filteredLandings <- aggregate_data_by_class(data$landings,variable="NEGEAR",classes=codes,dataset="landings")
      data$landings <- filteredLandings
      lengthData <- aggregate_data_by_class(data$lengthData,variable="NEGEAR",classes=codes,dataset="lengths")
      data$lengthData <- lengthData
    }
  }

  return(data)
}
