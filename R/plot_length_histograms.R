#' plots length distributions
#'
#' Creates a facet plot for each market category across all years for each gear type
#'
#'@param lengthData data frame.
#'@param species_itis Numeric scalar.  Species_itis code for species
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F

#'
#'@importFrom ggplot2 "ggplot" "aes" "geom_bar" "geom_histogram" "facet_wrap" "element_text" "ylab" "xlab" "labs" "scale_x_discrete"
#'@importFrom dplyr "summarize" "summarise" "group_by"
#'@importFrom magrittr "%>%"
#'
#' @export

plot_length_histogram <- function(lengthData,species_itis,outputDir,outputPlots){

  if (outputPlots == F) return()


  png(paste0(outputDir,"/5_market_category_lengths.png"))
  p <- ggplot(data = lengthData) +
    geom_bar(mapping = aes(x=LENGTH),na.rm=T) +
    facet_wrap(~MARKET_CODE,scales="free_y",nrow=length(unique(lengthData$MARKET_CODE)),ncol=1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_discrete(name="Length (cm)", breaks=seq(0, max(lengthData$LENGTH),10))
  print(p)
  dev.off()

}
