#' Fit length weight relationship to species data from SVDBS
#'
#'Fits a length weight relationship for use in length expansion
#'
#'@param lengthWeightData Data frame. length-weight pairs. Each row represents an individual fish
#
#'
#'@return List of model fit objects
#'\item{commonSlope}{\code{\url{lm}} fit for single slope (beta)}
#'\item{seasonalSlope}{\code{\url{lm}} fit for seasonal slopes}
#'
#'@section Notes on model fitting :
#'
#'The simplest Null model (H0) is assumed to be
#'\deqn{W_i = \alpha L_i^\beta  exp(e_i)}
#'
#'where W_i = Weight and L_i = Length of fish i, e_i ~ \eqn{N(0,\sigma^2)} and
#' \eqn{\alpha} &  \eqn{\beta} are intercept and slope parameters (on the log scale) to be estimated.
#'
#'The alternative H1: \eqn{\beta_j != \beta} where \eqn{\beta_j} is the slope parameter (on the log scale) for season i.
#'i = 1, ..., 4 (spring, summer, fall, winter)
#'
#'The above hypothesis is tested and the pvalue is output in the log file.
#'Plots of model fits are also produced and saved in the output directory
#'
#'
#' @export

fit_length_weight <- function(lengthWeightData,outputDir,logfile){

  # filter for null values
  lwd <- lengthWeightData$data %>% dplyr::filter(as.numeric(INDWT) > 0) %>% dplyr::select(INDWT,LENGTH,SEX,SEASON)
  lwd$INDWT <- as.numeric(lwd$INDWT)
  lwd$LENGTH <- as.numeric(lwd$LENGTH)
  lwd$SEX <- as.factor(lwd$SEX)
  lwd$SEASON <- as.factor(lwd$SEASON)

  # fit Weight = a.Length^b.exp(E)  where E ~ N(0,sig^2)
  # fit  no seasonal effect
  fit <- lm(log(INDWT) ~ log(LENGTH) , data=lwd)
  evar <- sum(fit$residuals^2)/fit$df.residual
  lwd$predWt <- exp(fit$fitted.values + evar/2)
  # fit seasonal effect
  fit2 <- lm(log(INDWT) ~ log(LENGTH):SEASON, data=lwd )
  evar <- sum(fit2$residuals^2)/fit2$df.residual
  lwd$predSeasWt <- exp(fit2$fitted.values + evar/2)


  reductionSS <- sum(fit$residuals^2) - sum(fit2$residuals^2)
  dfModel <- fit$df.residual- fit2$df.residual
  SSR <- sum(fit2$residuals^2)
  df <- fit2$df.residual
  Fstat <- (reductionSS/dfModel)/(SSR/df)
  pVal <- 1-pf(Fstat,dfModel,df)

  write_to_logfile(outputDir,logfile,"",label="LENGTH-WEIGHT RELATIONSHIPS from SVDBS",append=T)
  write_to_logfile(outputDir,logfile,data=pVal,label="pvalue: H0: single slope (beta) vs H1:seasonal (beta)",append=T)

  figText <- data.frame(SEASON = sort(unique(lwd$SEASON)),
                        x = c(rep(min(lwd$LENGTH),4)),
                        y = c(rep(max(lwd$INDWT),4)),
                        text = c(rep(paste0("Common slope: W = ",signif(exp(fit$coefficients[1]),6),"L^",signif(fit$coefficients[2],6)),4)),
                        textSeas = c(paste0("Seasonal slope: W = ",signif(exp(fit2$coefficients[1]),6),"L^",signif(fit2$coefficients[2:5],6))))

  figText <- figText %>% dplyr::mutate_if(is.factor, as.character)
  figText$SEASON <- as.factor(figText$SEASON)


  png(paste0(outputDir,"/length_weight_relationship.png"),width = 1000,height = 1000,units="px")

  p <- ggplot2::ggplot(data = lwd,ggplot2::aes(x=LENGTH, y = INDWT, color = SEX)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::facet_wrap(facets="SEASON") +
    ggplot2::geom_line(ggplot2::aes(y = predWt),color = "red") +
    ggplot2::geom_line(ggplot2::aes(y = predSeasWt),color = "blue") +
    ggplot2::xlab("Length (cm)") +
    ggplot2::ylab("Weight (kg)") +
    ggplot2::ggtitle(paste0("Length-weight relationship for ",speciesName)) +
    ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=y,label = text ),show.legend = F,size=3,color="black",hjust="inward") +
    ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=.9*y,label = textSeas ),show.legend = F,size=3,color="black",hjust="inward")

  print(p)
  dev.off()

  return(list(commonSlope=fit,SeasonSlope=fit2))

}
