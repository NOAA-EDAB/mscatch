#' Aggregates rows of data by a given variable
#'
#'The landings or length data are aggregated over a variable (for example MARKET_CODE) conditional on another variable (eg NEGAR == "050")
#'
#'@param data tibble. Data set to aggreate
#'@param variable Character string. Name of the variable to aggregate over
#'@param classes Character vector. Two codes (from the variable) to aggregate (Assign 1st to 2nd)
#'@param conditionalOn Character Vector. Name and value of variable to condition the aggregation on eg. c("NEGEAR","050"). Default = NULL
#'@param dataset Characterstring. Denoting the type of data set. "landings" or "lengths
#'
#'@return tibble
#'\item{data}{The same form and content as the input tibble, just aggregated}
#'


aggregate_data_by_class <- function(data,variable,classes,conditionalOn=NULL,dataset) {

  nrows <- dim(conditionalOn)[1]
  # rename class conditional on anothe variable
  ind <- data[,variable]==classes[1]
  indCond <- 1
  if(!is.null(conditionalOn)) { # multiple condionals
    for (irow in 1:nrows){
      indCond <- as.logical(indCond) * (data[,conditionalOn[irow,1]] == conditionalOn[irow,2])
    }
  }
  data[as.logical(ind*indCond),variable] <- classes[2]

  # aggregate classes
  if (dataset == "landings") {
    data <- data %>% dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
      dplyr::summarise(landings_land=sum(landings_land, na.rm=T),landings_nn=sum(landings_nn, na.rm=T),len_totalNumLen=sum(len_totalNumLen,na.rm=T),len_numLengthSamples=sum(len_numLengthSamples, na.rm=T))

  } else if (dataset == "lengths") {
    data <- data %>% dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH) %>%
      dplyr::summarise(NUMLEN = sum(as.numeric(NUMLEN),na.rm=T))
  } else {
    stop(paste("Can not aggregate a dataset of type = ",dataset))
  }

  data <- dplyr::ungroup(data)

  return(data)

}
