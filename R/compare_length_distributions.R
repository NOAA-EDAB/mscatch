#' Compare length distributions across categories
#'
#'@param landings tibble. landings data in form described below
#'@param lengthData tibble. length data in form described below
#'@param variableToAggreagte character string. Denote the variable to aggregate based on length distribution
#'@param pValue numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#'@param outputDir Character string. Directory where output is saved
#'@param logfile Cahracter string. Name of logfile
#'
#'@return Updated landings and lengthsData object reflecting changes of aggregating categories over length
#'
#'
#'
#'
#'
#'@export

compare_length_distributions <- function(landings,lengthData,variableToAggreagte,pValue,outputDir,logfile) {

  mapCodes <- NULL ;  stop <-  F

  codes <- unique(landings[[variableToAggreagte]])
  print(codes)
  for (icode in 1:(length(codes)-1)) {
    for (jcode in (icode+1):length(codes)) {
      acode <- codes[icode]
      bcode <- codes[jcode]
      if (acode == bcode) next # dont test against self
      if ((acode =="UN") | (bcode == "UN")) next # leave unclassified alone

     #     print(paste0(acode,"_",bcode)      )
      sizeA <- lengthData %>% dplyr::select(NEGEAR,LENGTH,NUMLEN,MARKET_CODE) %>% dplyr::filter(MARKET_CODE == acode)
      sizeA <- as.numeric(rep(sizeA$LENGTH,sizeA$NUMLEN))
      sizeB <- lengthData %>% dplyr::select(NEGEAR,LENGTH,NUMLEN,MARKET_CODE) %>% dplyr::filter(MARKET_CODE == bcode)
      sizeB <- as.numeric(rep(sizeB$LENGTH,sizeB$NUMLEN))

      res <- ks.test(sizeA,sizeB)
      if(res$p.value > pValue) {  # length distributions are the same
        print("aggregate")
        write_to_logfile(outputDir,logfile,paste("combine",acode,"with",bcode,". SIG = ",res$p.value,"\n"),label="ks test aggregation",append = T)
        mapCodes <- c(acode,bcode)
        stop <- TRUE
        break
      }
    }
    if (stop==T) break
  }

  return(mapCodes)
}

