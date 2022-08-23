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
#'@param suppressMessages Boolean. Suppress all messages
#'
#'
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

fit_length_weight <- function(lengthWeightData,speciesName,speciesRules,outputDir=NULL,logfile=NULL,suppressMessages=F){

  # filter for null values
  lwdMain <- lengthWeightData %>% dplyr::filter(INDWT > 0) %>% dplyr::select(INDWT,LENGTH,SEX,SEASON,YEAR)

  types <- speciesRules$LengthWeightRelationships
  lengthWeightParams <- list()
  fits <- list()
  modelData <- list()

  # set of models to fit
  models <- c("SEMESTER","QUARTER","YEAR","SINGLE","SEX")
  if (!is.null(logfile)) {
    write_to_logfile(outputDir,logfile,"",label=paste0(speciesName,": LENGTH-WEIGHT RELATIONSHIPS"),append=T)
    write_to_logfile(outputDir,logfile,"",label="-------------------------------------",append=T)
  }
  if(!suppressMessages){
    message("Fitting models ...")
  }

  for (mod in models) {
    # massage the data based on model
    if (mod == "SEMESTER") {
      lwd <- lwdMain %>%
        dplyr::mutate(VAR = dplyr::case_when(SEASON %in% c("SPRING","SUMMER") ~ 1, TRUE ~ 2))
    } else if (mod == "QUARTER") {
      lwd <- lwdMain %>%
        dplyr::rename(VAR = "SEASON")

    } else if (mod == "YEAR") {
      lwd <- lwdMain %>%
        dplyr::rename(VAR = "YEAR")

    } else if (mod == "SEX") {
      # filter out SEX == 0 and fit both sexes
      lwd <- lwdMain %>%
        dplyr::filter(SEX %in% c(1,2)) %>%
        dplyr::rename(VAR = "SEX")
    } else if (mod == "SINGLE") {
      # filter out SEX == 0 and fit both sexes
      lwd <- lwdMain

    } else {
      stop(paste0("Fitting length-weight by ",types," has not been coded. Please select either:
                    QUARTER, SEMESTER, YEAR, SINGLE, SEX"))
    }

    ## fit model
    # fit Weight = a.Length^b.exp(E)  where E ~ N(0,sig^2)
    # fit common/single slope
    if (mod == "SINGLE"){
      fit <- lm(log(INDWT) ~ log(LENGTH) , data=lwdMain)
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
      write_to_logfile(outputDir,logfile,"",label=paste0("Fitted by: ",mod),append=T)
    }

    # write output to a list
    nParams <- length(fit$coefficients)
    fits[[mod]] <- fit
    lengthWeightParams[[mod]]$logAlpha <- fit$coefficients[1]
    lengthWeightParams[[mod]]$betas <- fit$coefficients[2:nParams]
    lengthWeightParams[[mod]]$var <- sum(fit$residuals^2)/fit$df.residual

    modelData[[mod]] <- lwd

  }

  # create plots for length-weight relationships
  # plot common slope on all plots.
  #models <- c("SEMESTER","QUARTER","YEAR","SINGLE","SEX")

  if(!is.null(outputDir)) {
    for (mod in models) {
      if (mod == "SINGLE") {next}
      if(!suppressMessages){
        message(paste0("Creating plots for ",mod, " in output folder ..."))
      }

      ncoefs <- length(fits[[mod]]$coefficients)
      coeffs <- fits[[mod]]$coefficients
      textFit  <-  c(paste0(mod," slope (blue): W = ",signif(exp(coeffs[1]),6),"L^",signif(coeffs[2:ncoefs],6)))
      textFitH0 <- c(paste0("Common slope (red): W = ",signif(exp(fits$SINGLE$coefficients[1]),6),"L^",signif(fits$SINGLE$coefficients[2],6)))
      # xlim, ylim
      xmin = min(modelData[[mod]]$LENGTH)
      ymax = max(modelData[[mod]]$INDWT)

      # plots common slope fit and separate seasonal fits on facet plot
      figText <- data.frame(ALT = sort(ncoefs-1),
                            x = c(rep(xmin,ncoefs-1)),
                            y = c(rep(ymax,ncoefs-1)),
                            label = paste(textFit,sep="\n"),
                            labelH0 = paste(textFitH0,sep="\n") )


    #
    #   figText <- figText %>% dplyr::mutate_if(is.factor, as.character)
    #   figText$ALT <- as.factor(figText$ALT)
    #
    #

      png(paste0(outputDir,"/length_weight_relationship_",speciesName,"-",mod,".png"),width = 1000,height = 1000,units="px")
        # plots by factor
      if((mod) == "SEX") {
        p <- ggplot2::ggplot(data = modelData[[mod]], ggplot2::aes(x=LENGTH, y = INDWT, color = as.factor(VAR))) +
          ggplot2::geom_point(shape = 1) +
          ggplot2::facet_wrap(facets="VAR") +
          ggplot2::geom_line(ggplot2::aes(y = predWt),color = "red") +
          ggplot2::geom_line(data = modelData[["SINGLE"]], ggplot2::aes(y = predWt), color = "blue") +
          ggplot2::xlab("Length (cm)") +
          ggplot2::ylab("Weight (kg)") +
          ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=y,label = label),show.legend = F,size=3,color="black",hjust="inward") +
          ggplot2::labs(title = paste0("Length-weight (SVDBS) relationship for ",speciesName, " by ",mod),color="VAR") +
          ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=.9*y,label = labelH0),show.legend = F,size=3,color="black",hjust="inward")

      } else {
        p <- ggplot2::ggplot(data = modelData[[mod]], ggplot2::aes(x=LENGTH, y = INDWT, color = as.factor(SEX))) +
          ggplot2::geom_point(shape = 1) +
          ggplot2::facet_wrap(facets="VAR") +
          ggplot2::geom_line(ggplot2::aes(y = predWt),color = "red") +
          ggplot2::geom_line(data = modelData[["SINGLE"]], ggplot2::aes(y = predWt), color = "blue") +
          ggplot2::xlab("Length (cm)") +
          ggplot2::ylab("Weight (kg)") +
          ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=y,label = label ),show.legend = F,size=3,color="black",hjust="inward") +
          ggplot2::labs(title = paste0("Length-weight (SVDBS) relationship for ",speciesName, " by ",mod),color="SEX") +
          ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=.9*y,label = labelH0),show.legend = F,size=3,color="black",hjust="inward")
      }
      print(p)
      dev.off()
    }
  }

  lengthWeightParams$varType <- types
  return(list(model=fits,params=lengthWeightParams))

}
