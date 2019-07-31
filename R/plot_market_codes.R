#' plots landings by market code
#'
#' Plots landings, number of samples for all market_codes found in landings data
#'
#'
#'@param marketData data frame.
#'@param plotID numeric scalar. number ID of plot
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F

#'
#'@importFrom ggplot2 "ggplot" "aes" "geom_bar" "geom_histogram" "facet_wrap" "element_text" "ylab" "xlab" "labs" "scale_x_discrete"
#'@importFrom dplyr "summarize" "summarise" "group_by"
#'@importFrom magrittr "%>%"
#'
#' @export

plot_market_codes <- function(marketData,plotID,outputDir,outputPlots){

  if (outputPlots == F) return()

  png(paste0(outputDir,"/",plotID,"_market_category_landings.png"))

  # landings grob
  pLand <- ggplot2::ggplot(data = marketData) +
    geom_bar(mapping = ggplot2::aes(x=MARKET_CODE,y=totalLandings),stat="identity")
  # num samples grob
  pSamp <- ggplot2::ggplot(data = marketData) +
    geom_bar(mapping = ggplot2::aes(x=MARKET_CODE,y=len_numLengthSamples),stat="identity")
  # data grob
  table <- gridExtra::tableGrob(marketData)
  # layout of plot
  lay <- rbind(c(1,2),
               c(3,3))
  gridExtra::grid.arrange(pLand,pSamp,table,layout_matrix=lay,as.table=T)

  dev.off()

}
