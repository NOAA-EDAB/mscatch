#' Creates Age length key
#'
#' Uses Gerritsen et al (2006) method for predicting missing values in
#' age length key
#'
#'@param ageLengthData List. Landings data and length data
#'@param aggregate_to Character string. Level of aggregation for all MARKET_CODES and gears ("QTR", "YEAR", "SEMESTER", MIX").
#'
#'@return Data frame
#'
#'\item{}{}
#'\item{}{}
#'
#'@export


create_age_length_key <- function(ageLengthData,aggregate_to) {

  yrs <- ageLengthData %>%
    dplyr::distinct(YEAR) %>%
    dplyr::pull()

  # recode QTR to either YEAR or SEMESTER
  if (aggregate_to == "SEMESTER") {
    ageLengthData <- ageLengthData %>%
      dplyr::mutate(SEMIAN = dplyr::case_when(QTR %in% c(1:2) ~ 1, TRUE~2)) %>%
      dplyr::select(-QTR)
  } else {
    stop("Not coded yet")
  }

  semianValues <- ageLengthData %>%
    dplyr::select(SEMIAN) %>%
    dplyr::distinct() %>%
    dplyr::pull()


  # Loop over year and semian to calculate predicted ALKey from multinomial, create original ALKey (from observed data), and create filled ALKey (where holes in orig ALK are filled with multinomial predictions)
  for (yr in yrs) {
    for (semian in semianValues)  {

      # Combined age data for that semester and year
#      semian.yr.age.data <- comb.age.semian[[as.character(semian)]][[yr]]
      semian.yr.age.data <- ageLengthData %>%
        dplyr::filter(SEMIAN == semian, YEAR == yr)

      # Determine maximum and minimum length of the length data
      # min.exp.len <- min.exp.length.yr[semian,yr]
      # max.exp.len <- max.exp.length.yr[semian,yr]
      min.exp.len <- min.exp.length.yr[semian,yr]
      max.exp.len <- max.exp.length.yr[semian,yr]

      # Create vector of lengths (using the length range of the expanded length data) that will become the rownames of the ALKey
      exp.lens <- as.character(min.exp.len:max.exp.len)

      # Create vector of lengths actually observed during that year and semester
      exp.land.len.sub <- subset(expanded.land.length.semian.mat, YEAR==yr & SEMIAN==semian)
      exp.lens.obs <- sort(as.vector(unique(exp.land.len.sub$LENGTH)))

      # Minimum and maximum age
      ### ????? Is max.age the maximum age observed over all years and semesters or just the maximum age in this particular year and semester ?????
      max.age <- max(semian.yr.age.data$AGE, na.rm=TRUE)
      min.age <- min(semian.yr.age.data$AGE, na.rm=TRUE)

      # Multinomial inputs
      agedata <- semian.yr.age.data
      small.len <- min.exp.len
      big.len <- max.exp.len
      small.age <- min.age
      big.age <- max.age
      ref.age <- multinom.ref.age
      # Multinomial call; save outputs to lists
      fx.output <- get.multinomial.props(agedata, small.len, big.len, small.age, big.age, ref.age)
      ALKey.multinom[[as.character(semian)]][[yr]] <- fx.output[['pred.p']]
      multinom.converg[as.character(semian),yr] <- fx.output[['converg']]


      # Create ALKey (obs) from the observed age samples
      tmp.sum <- tapply(semian.yr.age.data$NUMAGE, as.list(semian.yr.age.data[,c('LENGTH','AGE')]), sum, na.rm=TRUE)
      obs <- data.frame(tmp.sum)
      colnames(obs) <- colnames(tmp.sum)
      rm(tmp.sum)

      # Create original ALKey (orig) that includes all lengths in the expanded length matrix (and all consecutive ages); Therefore, this alk includes holes
      alk.lens <- as.character(min.exp.len:max.exp.len)
      alk.ages <- as.character(min.age:max.age)
      orig <- data.frame(matrix(NA, length(alk.lens), length(alk.ages)))
      rownames(orig) <- alk.lens
      colnames(orig) <- alk.ages
      orig[rownames(obs),colnames(obs)] <- obs
      orig <- orig/rowSums(orig,na.rm=TRUE)
      orig[is.na(orig)]<-0

      # Lengths in orig that need to be filled
      lens.fill <- rownames(orig)[rowSums(orig)==0]

      # Create filled ALKey (filled)
      filled <- orig
      # Fill lengths without age data with predicted proportions from ALKey.multinom
      if(length(lens.fill)>0) { filled[lens.fill,] <- ALKey.multinom[[semian]][[yr]][lens.fill,] }

      # Create ALKey with plus group
      plusgrp <- filled
      if(max.age > plus.age)
      {
        plusgrp[,as.character(plus.age)] <- rowSums(plusgrp[,as.character(plus.age:max.age)])
        plusgrp[,as.character((plus.age+1):max.age)] <- NULL
      }

      # Save to lists
      ALKey.orig[[semian]][[yr]]   <- orig
      ALKey.filled[[semian]][[yr]] <- filled
      ALKey.plusgrp[[semian]][[yr]] <- plusgrp

    }  # End of semian loop
  } # End of year loop








  return(key)
}
