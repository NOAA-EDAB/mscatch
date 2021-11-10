#' plot landings by gear
#'
#' Select the variable to plot.
#' Eventually have the option to print to file as diagnostics
#'
#'
#'
#'
#'
#'
#' @noRd

plot_landings_by_gear <- function(species,data,threshold,outputPlots,outputDir,plotID) {


  if (outputPlots == F) return()

  png(paste0(outputDir,"/",plotID,"_market_category_lengths_by_gear.png"))

  newD <- data %>% dplyr::group_by(YEAR,NEGEAR) %>% dplyr::summarise(totLand=sum(landings_land,na.rm = TRUE))

  g1 <- ggplot2::ggplot(newD) +
    ggplot2::geom_col(mapping = ggplot2::aes(x=YEAR, y=totLand, fill = NEGEAR)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(title = paste0("Landings of ",species," by NEGEAR, using a threshold of ",threshold)) +
    ggplot2::ylab("Total Landings (lbs)")

  print(g1)

  dev.off()

}
