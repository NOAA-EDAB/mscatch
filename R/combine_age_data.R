#' Combine age data from svdbs and cfdbs
#'
#' Combines the age data from the survey and commercial samples
#'
#'@param ageSurvey Data frame
#'@param ageComland Data frame
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


  if(toupper(aggregate_to) == "SEMESTER") {
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


  if ((toupper(aggregate_to) == "QUARTER") |
      (toupper(aggregate_to) == "QTR")  |
      (toupper(aggregate_to) == "YEAR") ) {

    ageSurvey <- ageSurvey %>%
      dplyr::mutate(TIME = dplyr::case_when(SEASON %in% c("SPRING") ~ 1,
                                            SEASON %in% c("SUMMER") ~ 2,
                                            SEASON %in% c("FALL") ~ 3,
                                            SEASON %in% c("WINTER") ~ 4,
                                            TRUE ~ 0)) %>%
      dplyr::select(YEAR,TIME,SEX,AGE,LENGTH,NUMAGE)

    ageComland <- ageComland %>%
      dplyr::rename(TIME = QTR) %>%
      dplyr::select(YEAR,TIME,SEX,AGE,LENGTH,NUMAGE)
  }


  ageData <- rbind(ageComland,ageSurvey)

  return(ageData)

}
