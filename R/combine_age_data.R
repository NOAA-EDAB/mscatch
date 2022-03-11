#' Combine age data from svdbs and cfdbs
#'
#'
#'@param ageSurvey
#'@param ageComland
#'@param aggregate_to Character string. Level of aggregation for all MARKET_CODES and gears ("QTR", "YEAR", "SEMESTER", MIX").
#'Default = YEAR.
#'
#'@return Data frame of combined length and ages across time
#'
#'\item{}{}
#'\item{}{}
#'
#'@export


combine_age_data <- function(ageSurvey,ageComland,aggregate_to) {


  if(aggregate_to == "SEMESTER") {
    ageSurvey <- ageSurvey %>%
      dplyr::mutate(TIME = dplyr::case_when(SEASON %in% c("SPRING","SUMMER") ~ 1,
                              SEASON %in% c("FALL","WINTER") ~ 2,
                              TRUE ~ 0)) %>%
      dplyr::select(YEAR,TIME,SEX,AGE,LENGTH,NUMAGE)

    ageComland <- ageComland %>%
      dplyr::mutate(TIME = dplyr::case_when(QTR %in% c(1,2) ~ 1,
                                                QTR %in% c(3,4) ~ 2,
                                                TRUE ~ 0)) %>%
      dplyr::select(YEAR,TIME,SEX,AGE,LENGTH,NUMAGE)

  }


  if((aggregate_to == "QTR") | (aggregate_to == "YEAR") ) {
    ageSurvey <- ageSurvey %>%
      dplyr::rename(TIME = QTR) %>%
      dplyr::select(YEAR,TIME,SEX,AGE,LENGTH,NUMAGE)

    ageComland <- ageComland %>%
      dplyr::rename(TIME = QTR) %>%
      dplyr::select(YEAR,TIME,SEX,AGE,LENGTH,NUMAGE)
  }

  ageData <- rbind(ageComland,ageSurvey)

  return(ageData)

}
