#' plots Number of length samples
#'
#'A facet grid for each gear type by market code and quarter year
#'Eventually generalize to semester or annually
#'
#'@param filteredLandings landings data aggregated to some level
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F
#'
#'@return NOthing
#'
#'@export

plot_market_code_by_qtr <- function(filteredLandings,outputDir,outputPlots=T) {

  if (outputPlots == F) return()


  # for each geartype
  for (gearType in unique(filteredLandings$NEGEAR)) {

    png(paste0(outputDir,"/8_market_category_qtr_landings_",as.character(gearType),",.png"))

    p <- ggplot2::ggplot(data = filteredLandings %>% dplyr::filter(NEGEAR == gearType)) +
      ggplot2::geom_line(mapping = ggplot2::aes(x=as.numeric(YEAR), y=len_numLengthSamples)) +
      ggplot2::facet_wrap(facets= c("MARKET_CODE", "QTR")) +
      ggplot2::ggtitle(paste0("Market Code by Quarter (gear type = ",as.character(gearType),")")) +
      ggplot2::ylab("Numbe of Length Samples")
    print(p)

    dev.off()
  }
}
