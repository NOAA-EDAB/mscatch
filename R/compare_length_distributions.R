#' Compare length distributions across categories
#'
#'@param data List. Landings and length data in form described below
#'@param variableToAggreagte character string. Denote the variable to aggregate based on length distribution
#'@param pValue numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#'@param outputDir Character string. Directory where output is saved
#'@param logfile Cahracter string. Name of logfile
#'
#'@return Character vector
#'\item{codes}{Two market category codes not significantly different}
#'
#'@export

compare_length_distributions <- function(data,variableToAggregate,groupBy,pValue,outputDir,logfile) {

  landings <- data$landings
  lengthData <- data$lengthData

  mapCodes <- NULL ;  stop <-  F

  codes <- unique(landings[[variableToAggregate]])
  for (icode in 1:(length(codes)-1)) {
    for (jcode in (icode+1):length(codes)) {
      acode <- codes[icode]
      bcode <- codes[jcode]
      if (acode == bcode) next # dont test against self
      if ((acode =="UN") | (bcode == "UN")) next # leave unclassified alone

     #     print(paste0(acode,"_",bcode)      )
      sizeA <- lengthData %>% dplyr::select(!!groupBy) %>% dplyr::filter(UQ(as.name(variableToAggregate)) == acode)
      sizeA <- as.numeric(rep(sizeA$LENGTH,sizeA$NUMLEN))
      sizeB <- lengthData %>% dplyr::select(!!groupBy) %>% dplyr::filter(UQ(as.name(variableToAggregate)) == bcode)
      sizeB <- as.numeric(rep(sizeB$LENGTH,sizeB$NUMLEN))

      res <- ks.test(sizeA,sizeB)
      if(res$p.value > pValue) {  # length distributions are the same
        write_to_logfile(outputDir,logfile,paste(" Combine",acode,"with",bcode,". SIG = ",res$p.value,"\n"),label="ks test aggregation",append = T)
        mapCodes <- rbind(mapCodes,c(acode,bcode))
        print(mapCodes)
        #stop <- TRUE
        #break
      }
    }
    #if (stop==T) break
  }
  # return all pairs that are significantly different
  return(mapCodes)
}

