#' plot landings data as checks to aggregation rules
#'
#' Select the variable to plot
#'
#'
#'
#'
#'
#' @importFrom ggplot2 "aes" "geom_bar" "theme" "element_text" "ylab" "xlab"
#'
#' @export

plot_landings_data <- function(data,dataType,threshold) {

  g1 <- ggplot2::ggplot() +
    geom_bar(data, mapping = aes(x=YEAR,y=landings_land),stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  g1

}
