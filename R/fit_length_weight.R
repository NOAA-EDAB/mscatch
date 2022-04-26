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
  lwd <- lengthWeightData %>% dplyr::filter(INDWT > 0) %>% dplyr::select(INDWT,LENGTH,SEX,SEASON)

  # fit Weight = a.Length^b.exp(E)  where E ~ N(0,sig^2)
  # fit  no seasonal effect
  nullFit <- lm(log(INDWT) ~ log(LENGTH) , data=lwd)
  evar <- sum(nullFit$residuals^2)/nullFit$df.residual
  lwd$predWt <- exp(nullFit$fitted.values + evar/2)
  # fit seasonal effect
  altFit <- lm(log(INDWT) ~ log(LENGTH):SEASON, data=lwd )
  evar <- sum(altFit$residuals^2)/altFit$df.residual
  lwd$predSeasWt <- exp(altFit$fitted.values + evar/2)

  # test the null H0: bi=b vs alternative H1: bi != b
  reductionSS <- sum(nullFit$residuals^2) - sum(altFit$residuals^2)
  dfModel <- nullFit$df.residual- altFit$df.residual
  SSR <- sum(altFit$residuals^2)
  df <- altFit$df.residual
  Fstat <- (reductionSS/dfModel)/(SSR/df)
  pVal <- 1-pf(Fstat,dfModel,df)

  if (!is.null(logfile)) {
    write_to_logfile(outputDir,logfile,"",label=paste0(speciesName,": LENGTH-WEIGHT RELATIONSHIPS from SVDBS"),append=T)
    write_to_logfile(outputDir,logfile,data=pVal,label="pvalue: H0: single slope (beta) vs H1:seasonal (beta)",append=T)
  }

  # plots common slope fit and separate seasonal fits on facet plot
  nSeasons <- length(unique(lwd$SEASON))
  figText <- data.frame(SEASON = sort(unique(lwd$SEASON)),
                        x = c(rep(min(lwd$LENGTH),nSeasons)),
                        y = c(rep(max(lwd$INDWT),nSeasons)),
                        text = c(rep(paste0("Common slope (red): W = ",signif(exp(nullFit$coefficients[1]),6),"L^",signif(nullFit$coefficients[2],6)),nSeasons)),
                        textSeas = c(paste0("Seasonal slope (blue): W = ",signif(exp(altFit$coefficients[1]),6),"L^",signif(altFit$coefficients[2:(1+nSeasons)],6))))

  figText <- figText %>% dplyr::mutate_if(is.factor, as.character)
  figText$SEASON <- as.factor(figText$SEASON)


  if (!is.null(outputDir)) {
    png(paste0(outputDir,"/length_weight_relationship_",speciesName,".png"),width = 1000,height = 1000,units="px")

    p <- ggplot2::ggplot(data = lwd, ggplot2::aes(x=LENGTH, y = INDWT, color = as.factor(SEX))) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::facet_wrap(facets="SEASON") +
      ggplot2::geom_line(ggplot2::aes(y = predWt),color = "red") +
      ggplot2::geom_line(ggplot2::aes(y = predSeasWt), color = "blue") +
      ggplot2::labs(title = paste0("Length-weight (SVDBS) relationship for ",speciesName),color="SEX") +
      ggplot2::xlab("Length (cm)") +
      ggplot2::ylab("Weight (kg)") +
      ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=y,label = text ),show.legend = F,size=3,color="black",hjust="inward") +
      ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=.9*y,label = textSeas ),show.legend = F,size=3,color="black",hjust="inward")

    print(p)
    dev.off()
  }

  nParams <- length(nullFit$coefficients)
  lengthWeightParamsH0 <- list()
  lengthWeightParamsH0$logAlpha <- nullFit$coefficients[1]
  lengthWeightParamsH0$betas <- nullFit$coefficients[2:nParams]
  lengthWeightParamsH0$var <- sum(nullFit$residuals^2)/nullFit$df.residual

  nParams <- length(altFit$coefficients)
  lengthWeightParamsH1 <- list()
  lengthWeightParamsH1$logAlpha <- altFit$coefficients[1]
  lengthWeightParamsH1$betas <- altFit$coefficients[2:nParams]
  lengthWeightParamsH1$var <- sum(altFit$residuals^2)/altFit$df.residual


  return(list(commonSlope=nullFit,SeasonSlope=altFit, pValue = pVal,paramsH0=lengthWeightParamsH0,paramsH1=lengthWeightParamsH1))

}
