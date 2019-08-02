#' plots Number of length samples
#'
#'A facet grid for each gear type by market code and quarter year
#'Eventually generalize to semester or annually
#'
#'@param data List. landings and length data aggregated to some level
#'@param plotID numeric scalar. number ID of plot
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F
#'
#'@return Nothing
#'
#'@export

plot_market_code_by_qtr <- function(data,plotID,outputDir,outputPlots=T) {

  if (outputPlots == F) return()


  # for each geartype
  for (gearType in unique(data$landings$NEGEAR)) {

    png(paste0(outputDir,"/",plotID,"_market_category_qtr_landings_",as.character(gearType),",.png"))

    p <- ggplot2::ggplot(data = data$landings %>% dplyr::filter(NEGEAR == gearType)) +
      ggplot2::geom_line(mapping = ggplot2::aes(x=as.numeric(YEAR), y=len_numLengthSamples)) +
      ggplot2::facet_wrap(facets= c("MARKET_CODE", "QTR")) +
      ggplot2::ggtitle(paste0("Market Code by Quarter (gear type = ",as.character(gearType),")")) +
      ggplot2::ylab("Number of Length Samples")
    print(p)

    dev.off()

    png(paste0(outputDir,"/",plotID+1,"_market_category_qtr_length_distribution_",as.character(gearType),",.png"))

    # Take a look at length distribution by QTR and MARKET CODE
    d <- data$lengthData %>% dplyr::filter(NEGEAR == gearType) %>% dplyr::group_by(MARKET_CODE,QTR,LENGTH) %>% summarise(numlens=sum(as.numeric(NUMLEN)))
    p <-   ggplot2::ggplot(data = d) +
      ggplot2::geom_bar(stat="identity",mapping= ggplot2::aes(x=LENGTH,y=numlens),na.rm=T) +
      ggplot2::facet_wrap(~QTR+MARKET_CODE) +
      ggplot2::ggtitle(paste0("By QRT and MARKET_CODE  (gear type = ",as.character(gearType),")")) +
      ggplot2::ylab("Distribution of all length samples")
    print(p)

    dev.off()

    png(paste0(outputDir,"/",plotID+2,"_market_category_qtr_number_samples_",as.character(gearType),",.png"))

    d <- data$landings %>% dplyr::filter(NEGEAR == gearType) %>% dplyr::group_by(MARKET_CODE,QTR) %>% summarise(numlens=sum(as.numeric(len_numLengthSamples)==0))
    p <-   ggplot2::ggplot(data = d) +
      ggplot2::geom_bar(stat="identity",mapping = ggplot2::aes(x=1,y=numlens),na.rm=T) +
      ggplot2::facet_wrap(~QTR+MARKET_CODE) +
      ggplot2::ggtitle(paste0("By QRT and MARKET_CODE  (gear type = ",as.character(gearType),")")) +
      ggplot2::ylab("Number of years with missing length samples")
    print(p)

    dev.off()

  }
}
