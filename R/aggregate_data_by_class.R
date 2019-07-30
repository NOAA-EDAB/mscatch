#' Aggregates rows of data by a given variable
#'
#'Collapses the data based on variables to collapse over
#'
#'@param data tibble. Data set to aggreate
#'@param variable Character string. Name of the variable to aggregate over
#'@param classes Character vector. Two codes (from the variable) to aggregate
#'@param dataset Characterstring. Denoting the type of data set. "landings" or "lengths
#'
#'
#'@export

aggregate_data_by_class <- function(data,variable,classes,dataset) {

  # rename class
  ind <- data[,variable]==classes[1]
  data[ind,variable] <- classes[2]
  print(unique(data$MARKET_CODE))
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
  return(data)

}
