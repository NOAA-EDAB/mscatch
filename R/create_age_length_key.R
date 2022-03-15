#' Creates Age length key
#'
#' Uses Gerritsen et al (2006) method for predicting missing values in
#' age length key. Currently disregards gear type and only uses QTR/SEMESETER/YEAR.
#'
#'@param expandedLandings Data frame. Landings expanded by length distribution
#'@param ageLengthData Data frame. Age-length data from databases
#'@param filterYrs Numeric scalar. The First year in which age length key is required
#'@param plusAge Numeric scalar. Maximum age category. (eg , 10+)
#'@param referenceAge Numeric scalar.Used in the GLM as the age against which all
#' the other ages are estimated. It is typically the most abundant age.
#'@param printConvergence Boolean. Should convergence messages from \code{multinom} be printed to console (Default = F)
#'
#'@return Data frame
#'
#'\item{}{}
#'\item{}{}
#'
#'@export

# Adapted from K.Curti Maclerel code
create_age_length_key <- function(expandedLandings,ageLengthData,filterYrs,plusAge,referenceAge, printConvergence=F) {

  plus.age <- plusAge

  expanded <- expandedLandings %>%
    dplyr::filter(YEAR >= filterYrs)

  yrs <- unique(expanded$YEAR)

  semianValues <- ageLengthData %>%
    dplyr::select(TIME) %>%
    dplyr::distinct() %>%
    dplyr::arrange() %>%
    dplyr::pull() %>%
    sort()


  ### For each year and semester, fill holes in age data


  # List for predicted proportions-at-age from the multinomial
  ALKey.multinom <- NULL
  # List for original ALKey
  ALKey.orig <- NULL
  # List for filled ALKey
  ALKey.filled <- NULL
  # List for ALKey with plus group
  ALKey.plusgrp <- NULL


  # Matrix to log convergence
  multinom.converg <- data.frame(matrix(NA,nrow=2,ncol=length(yrs)))
  rownames(multinom.converg) <- as.character(1:2)
  colnames(multinom.converg) <- yrs
#
  # yr <- 2013
  # semian <- 1
  # browser()

  # Loop over year and semian to calculate predicted ALKey from multinomial, create original ALKey (from observed data), and create filled ALKey (where holes in orig ALK are filled with multinomial predictions)
  for (yr in yrs) {
    for (semian in semianValues)  {
#
      print(yr)
      print(semian)
      # Combined age data for that semester and year
#      semian.yr.age.data <- comb.age.semian[[as.character(semian)]][[yr]]
      semian.yr.age.data <- ageLengthData %>%
        dplyr::filter(TIME == semian, YEAR == yr)

      ## NEED TO SORT THIS OUT
      if ((nrow(semian.yr.age.data) == 0) | (length(unique(semian.yr.age.data$AGE)) <= 1)){
        # no age data
        # cant fit multinomial
        # Save to lists
        ALKey.orig[[as.character(semian)]][[(yr)]]   <- as.data.frame(NULL)
        ALKey.filled[[as.character(semian)]][[as.character(yr)]] <- as.data.frame(NULL)
        ALKey.plusgrp[[as.character(semian)]][[as.character(yr)]] <- as.data.frame(NULL)
        next
      }


      # Determine maximum and minimum length of the length data
      # min.exp.len <- min.exp.length.yr[semian,yr]
      # max.exp.len <- max.exp.length.yr[semian,yr]

      # extract expanded landings for YR,SEMESTER
      expandedSubset <- expanded %>%
        dplyr::filter(YEAR == yr,TIME == semian)

      min.exp.len <- expandedSubset %>%
        dplyr::pull(LENGTH) %>%
        min()

      max.exp.len <- expandedSubset %>%
        dplyr::pull(LENGTH) %>%
        max()

      # Create vector of lengths (using the length range of the expanded length data) that will become the rownames of the ALKey
      exp.lens <- as.character(min.exp.len:max.exp.len)

      # Create vector of lengths actually observed during that year and semester
      exp.land.len.sub <- expandedSubset %>% dplyr::pull(LENGTH)
      exp.lens.obs <- sort(unique(exp.land.len.sub))

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
      ref.age <- min(referenceAge,max.age)


      # if ((yr == 2010) & (semian == 2)) {
      #   browser()
      # }
      # Multinomial call
      fx.output <- get_multinomial_props(agedata, small.len, big.len, small.age, big.age, ref.age, printConvergence = printConvergence)

      ALKey.multinom[[as.character(semian)]][[as.character(yr)]] <- fx.output[['pred.p']]
      multinom.converg[as.character(semian),as.character(yr)] <- fx.output[['converg']]


      # Create ALKey (obs) from the observed age samples
      tmp.sum <- tapply(semian.yr.age.data$NUMAGE, as.list(semian.yr.age.data[,c('LENGTH','AGE')]), sum, na.rm=TRUE)
      obs <- data.frame(tmp.sum)
      colnames(obs) <- colnames(tmp.sum)



      # Create original ALKey (orig) that includes all lengths in the expanded length matrix (and all consecutive ages); Therefore, this alk includes holes
      alk.lens <- as.character(min.exp.len:max.exp.len)
      alk.ages <- as.character(min.age:max.age)
      orig <- data.frame(matrix(NA, length(alk.lens), length(alk.ages)))
      rownames(orig) <- alk.lens
      colnames(orig) <- alk.ages
      obs <- obs[rownames(obs) %in% rownames(orig),] # pull out only lengths seen in expanded data

      orig[rownames(obs),colnames(obs)] <- obs
      orig <- orig/rowSums(orig,na.rm=TRUE)
      orig[is.na(orig)]<-0

      # Lengths in orig that need to be filled
      lens.fill <- rownames(orig)[rowSums(orig)==0]

      # Create filled ALKey (filled)
      filled <- orig
      # Fill lengths without age data with predicted proportions from ALKey.multinom
      if(length(lens.fill)>0) {
        if (length(lens.fill) == 1){
          filled[lens.fill,] <- as.vector(ALKey.multinom[[as.character(semian)]][[as.character(yr)]][lens.fill,])
        } else {
          filled[lens.fill,] <- as.data.frame(ALKey.multinom[[as.character(semian)]][[as.character(yr)]][lens.fill,])
        }


      }

      # Create ALKey with plus group
      plusgrp <- filled
      if(max.age > plus.age)
      {
        plusgrp[,as.character(plus.age)] <- rowSums(plusgrp[,as.character(plus.age:max.age)])
        plusgrp[,as.character((plus.age+1):max.age)] <- NULL
      }

      # Save to lists
      ALKey.orig[[as.character(semian)]][[(yr)]]   <- orig
      ALKey.filled[[as.character(semian)]][[as.character(yr)]] <- filled
      ALKey.plusgrp[[as.character(semian)]][[as.character(yr)]] <- plusgrp

    }  # End of semian loop
  } # End of year loop



  ### Add YEAR, SEMIAN and LENGTH columns and collapse ALKeys to one matrix (ALKey.mat) for export

  ########################################################
  ############ NEED TO REWRITE THIS ######################
  ########################################################

  ALKey.export <- ALKey.plusgrp


  for (yr in yrs)
  {
    # yr <- '2013'
    for (semian in semianValues)
    {
      print(yr)
      print(semian)
      yr <- as.character(yr)
      semian <- as.character(semian)
      # semian <- 1
      ALKey.export[[semian]][[yr]]$YEAR <- yr
      ALKey.export[[semian]][[yr]]$TIME <- semian
      ALKey.export[[semian]][[yr]]$LENGTH <- rownames(ALKey.export[[semian]][[yr]])

    }  # End of semian loop
  } # End of year loop

  alk.semian <- lapply(ALKey.export, plyr::rbind.fill)
  ALKey.mat <- plyr::rbind.fill(alk.semian)

  # Order columns
  all.cols <- colnames(ALKey.mat)
  age.cols <- all.cols[!all.cols%in%c('YEAR','TIME','LENGTH')]
  ordered.cols <- c('YEAR','TIME','LENGTH',sort(as.numeric(age.cols)) )
  ALKey.mat <- ALKey.mat[,ordered.cols]

  # Set NAs to zeros
  ALKey.mat[is.na(ALKey.mat)] <- 0

  # ### Convert ALKey.mat from wide to long format (need to modify names of age cols to permit use of gather fx)
  # tmp.mat <- ALKey.mat
  # cols <- colnames(tmp.mat)
  # age.cols <- cols[!cols%in%c('YEAR','SEMIAN','LENGTH')]
  # colnames(tmp.mat)[colnames(tmp.mat)%in%age.cols] <- paste('age',age.cols,sep='.')

  # ALKey.long <- tidyr::gather(data=tmp.mat, key=Age, value=NumAge, age.0:age.10, factor_key=FALSE )
  # ALKey.long$Age <- do.call(rbind, strsplit(ALKey.long$Age, "\\."))[,2]
  # ### Round for export
  #
  # ALKey.rounded <- ALKey.long
  # ALKey.rounded$NumAge <- round(ALKey.rounded$NumAge,3)


  # return(list(mat = ALKey.mat,rounded=ALKey.rounded,long=ALKey.long))

  return(list(alKey=tibble::as_tibble(ALKey.mat),converged = multinom.converg))

}
