#' Fit length weight relationship to species data from SVDBS
#'
#'Fits a length weight relationship for use in catch expansion
#'
#'@param lengthWeightData Data frame. length-weight pairs. Each row represents an individual fish
#'@param speciesName Character string. Common name for species
#'@param speciesRules List. Obtained from \code{get_species_object}
#'@param outputDir Character string. Path to output directory (Default = NULL, no output written)
#'@param logfile Character string. Specify the name for the log file generated describing all decisions made.
#'(Default = NULL, no output written)

#'
#'@return List of model fit objects
#'\item{commonSlope}{\code{\link{lm}} object. Fit for single slope (beta)}
#'\item{seasonalSlope}{\code{\link{lm}} object. Fit for seasonal slopes}
#'
#'@section Notes on model fitting:
#'
#'The Weight-Length relationship is defined as
#'
#' \deqn{W_{ij} = \alpha L_{ij}^{\beta_j} e^{z_{ij}}}
#'
#'where,
#'
#' \eqn{W_{ij}} = Weight of fish \emph{i} in season \emph{j}, \eqn{i = 1, ..., n, j = 1, ... J}
#'
#' \eqn{L_{ij}} = Length of fish \emph{i} in season \emph{j},
#'
#' \eqn{z_{ij}} ~ \eqn{N(0,\sigma^2)},
#'
#' and \eqn{\beta_j} is effect of season \emph{j}
#'
#' On the more familiar log scale the model is
#'
#' \deqn{log(W_{ij}) = log(\alpha) + \beta_j log(L_{ij}) + {z_{ij}}}
#'
#' To test for a seasonal effect, we test the Null hypothesis:
#'
#'  \deqn{{H_0}:  \beta_j = \beta}
#'
#'  against the alternative,
#'
#'  \deqn{{H_1}: \beta_j \neq \beta}
#'
#' The test statistic is the standard F statistic
#'
#' \deqn{F = \frac{(RSS_{H_0}-RSS_{H_1})/(J-1)}{RSS_{H_1}/(n-J-1)}}
#'
#' which will have an F distribution with (J-1, n-J-1) degrees of freedom
#'
#' where RSS= Residual Sum of Square s
#'
#' @export

fit_length_weight <- function(lengthWeightData,speciesName,speciesRules,outputDir=NULL,logfile=NULL){

  # filter for null values
  lwd <- lengthWeightData %>% dplyr::filter(INDWT > 0) %>% dplyr::select(INDWT,LENGTH,SEX,SEASON,YEAR)

  types <- speciesRules$LengthWeightRelationships
  isSingle <- F
  # process data based on types
  # eg. if "semester" pool SEASON,
  if (length(types) == 1) {
    if(!is.na(types)) {
      if (toupper(types) == "SEMESTER") {
        lwd <- lwd %>%
          dplyr::mutate(VAR = dplyr::case_when(SEASON %in% c("SPRING","SUMMER") ~ 1, TRUE ~ 2))
      } else if (toupper(types) == "QUARTER") {
        lwd <- lwd %>%
          dplyr::rename(VAR = "SEASON")

      } else if (toupper(types) == "YEAR") {
        lwd <- lwd %>%
          dplyr::rename(VAR = "YEAR")

      } else if (toupper(types) == "SEX") {
        # filter out SEX == 0 and fit both sexes
        lwd <- lwd %>%
          dplyr::filter(SEX %in% c(1,2)) %>%
          dplyr::rename(VAR = "SEX")

        #  dplyr::rename(VAR = "SEX")

      } else if (toupper(types) == "SINGLE") {
        isSingle <- 1

      } else {
        stop(paste0("Fitting length-weight by ",types," has not been coded. Please select either:
                    QUARTER, SEMESTER, YEAR, SINGLE, SEX"))
      }

    }
  } else {
    nFits <- length(types)
    return()
  }

  # fit Weight = a.Length^b.exp(E)  where E ~ N(0,sig^2)
  # fit common/single slope
  if (isSingle){
    fit <- lm(log(INDWT) ~ log(LENGTH) , data=lwd)
    evar <- sum(fit$residuals^2)/fit$df.residual
    lwd$predWt <- exp(fit$fitted.values + evar/2)
    uniqueVals <- 1
    nAlt <- 1
  } else {
    # cast to logical
    lwd <- lwd %>%
      dplyr::mutate(VAR = as.factor(VAR))
    # fit effect
    fit <- lm(log(INDWT) ~ log(LENGTH):VAR, data=lwd )
    evar <- sum(fit$residuals^2)/fit$df.residual
    lwd$predWt <- exp(fit$fitted.values + evar/2)
    uniqueVals <- unique(lwd$VAR)
    nAlt <- length(uniqueVals)
  }

  if (!is.null(logfile)) {
    write_to_logfile(outputDir,logfile,"",label=paste0(speciesName,": LENGTH-WEIGHT RELATIONSHIPS from SVDBS"),append=T)
    write_to_logfile(outputDir,logfile,"",label=paste0("Fitted by: ",toupper(types)),append=T)
  }

  if (nAlt == 1) {
    textFit = paste0(toupper(types)," slope (blue): W = ",signif(exp(fit$coefficients[1]),6),"L^",signif(fit$coefficients[2],6))
  } else {
    textFit = c(paste0(toupper(types)," slope (blue): W = ",signif(exp(fit$coefficients[1]),6),"L^",signif(fit$coefficients[2:(1+nAlt)],6)))
  }

  # plots common slope fit and separate seasonal fits on facet plot
  figText <- data.frame(ALT = sort(uniqueVals),
                        x = c(rep(min(lwd$LENGTH),nAlt)),
                        y = c(rep(max(lwd$INDWT),nAlt)),
                        label = paste(textFit,sep="\n"))

  figText <- figText %>% dplyr::mutate_if(is.factor, as.character)
  figText$ALT <- as.factor(figText$ALT)


  if (!is.null(outputDir)) {
    png(paste0(outputDir,"/length_weight_relationship_",speciesName,"-",types,".png"),width = 1000,height = 1000,units="px")

    if (toupper(types) == "SINGLE") {
      p <- ggplot2::ggplot(data = lwd, ggplot2::aes(x=LENGTH, y = INDWT, color=as.factor(SEX))) +
        ggplot2::geom_point(shape = 1) +
        ggplot2::geom_line(ggplot2::aes(y = predWt),color = "blue") +
        ggplot2::labs(title = paste0("Length-weight (SVDBS) relationship for ",speciesName),color="SEX") +
        ggplot2::xlab("Length (cm)") +
        ggplot2::ylab("Weight (kg)") +
        ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=y,label = label ),show.legend = F,size=3,color="black",hjust="inward")
      #ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=.9*y,label = textAlt, color=NA ),show.legend = F,size=3,color="black",hjust="inward")

      print(p)
    } else {
      # plots by factor
      if(toupper(types) == "SEX") {
        p <- ggplot2::ggplot(data = lwd, ggplot2::aes(x=LENGTH, y = INDWT, color = as.factor(VAR))) +
          ggplot2::geom_point(shape = 1) +
          ggplot2::facet_wrap(facets="VAR") +
          ggplot2::geom_line(ggplot2::aes(y = predWt),color = "red") +
          ggplot2::xlab("Length (cm)") +
          ggplot2::ylab("Weight (kg)") +
          ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=y,label = label ),show.legend = F,size=3,color="black",hjust="inward") +
          ggplot2::labs(title = paste0("Length-weight (SVDBS) relationship for ",speciesName, " by ",toupper(types)),color="VAR")

      }else {
        p <- ggplot2::ggplot(data = lwd, ggplot2::aes(x=LENGTH, y = INDWT, color = as.factor(SEX))) +
          ggplot2::geom_point(shape = 1) +
          ggplot2::facet_wrap(facets="VAR") +
          ggplot2::geom_line(ggplot2::aes(y = predWt),color = "red") +
          ggplot2::xlab("Length (cm)") +
          ggplot2::ylab("Weight (kg)") +
          ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=y,label = label ),show.legend = F,size=3,color="black",hjust="inward") +
          ggplot2::labs(title = paste0("Length-weight (SVDBS) relationship for ",speciesName, " by ",toupper(types)),color="SEX")

      }

      print(p)
    }
    dev.off()
  }

  # output parameter estimates
  nParams <- length(fit$coefficients)
  lengthWeightParams <- list()
  lengthWeightParams$logAlpha <- fit$coefficients[1]
  lengthWeightParams$betas <- fit$coefficients[2:nParams]
  lengthWeightParams$var <- sum(fit$residuals^2)/fit$df.residual
  lengthWeightParams$varType <- types


  return(list(fit=fit,params=lengthWeightParams))

}
