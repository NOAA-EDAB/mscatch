#' Compare length distributions across categories
#'
#'@param landings List. Landings data in form described below
#'@param lengthData List. length data in form described below
#'@param variableToAggreagte Character string. Denote the variable to aggregate based on length distribution
#'@param pValue Numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#'@param outputDir Character string. Directory where output is saved
#'@param logfile Character string. Name of logfile
#'
#'@return Character matrix
#'
#'\item{codes}{Two market category codes not significantly different}
#'
#'@noRd

compare_length_distributions <- function(landings,lengthData,variableToAggregate,groupBy,pValue,outputDir,logfile) {

  options(warn=-1)
  mapCodes <- NULL ;  stop <-  F

  codes <- unique(landings[[variableToAggregate]])
  for (icode in 1:(length(codes)-1)) {
    for (jcode in (icode+1):length(codes)) {
      acode <- codes[icode]
      bcode <- codes[jcode]
      if (acode == bcode) next # dont test against self

      if ((acode =="UN") | (bcode == "UN")) next # leave unclassified alone

     #`!!` unquotes argument
      sizeA <- lengthData %>%
        dplyr::select(!!groupBy) %>%
        dplyr::filter(UQ(as.name(variableToAggregate)) == acode)
      sizeA <- as.numeric(rep(sizeA$LENGTH,sizeA$NUMLEN))

      sizeB <- lengthData %>%
        dplyr::select(!!groupBy) %>%
        dplyr::filter(UQ(as.name(variableToAggregate)) == bcode)
      sizeB <- as.numeric(rep(sizeB$LENGTH,sizeB$NUMLEN))

      if ((length(sizeA) < 1) | (length(sizeB)< 1)) {
        write_to_logfile(outputDir,logfile,paste(" Not enough length Samples to compare ",acode,"with",bcode,"\n"),label="ks test aggregation",append = T)
        next
      }
      res <- ks.test(sizeA,sizeB)

      if(res$p.value > pValue) {  # length distributions are the same
        write_to_logfile(outputDir,logfile,paste(" Combine",acode,"with",bcode,". SIG = ",res$p.value,"\n"),label="ks test aggregation",append = T)
        mapCodes <- rbind(mapCodes,c(acode,bcode))

      }
    }

  }
  options(warn = 0)
  # return all pairs that are significantly different
  return(mapCodes)
}

