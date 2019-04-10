#' plot landings data as checks to aggregation rules
#'
#' Select the variable to plot
#'
#'
#'
#'
#'
#' @importFrom ggplot2 "aes" "geom_bar" "geom_col" "theme" "element_text" "ylab" "xlab"
#'
#' @export

plot_landings_data <- function(data,dataType,threshold) {

  # data <- sampleData_147
  # dataType <- "NEGEAR"
  # threshold <- 100

  newD <- data %>% group_by(YEAR,.dots=dataType)%>%summarise(totLand=sum(landings_land,na.rm = TRUE))

  g1 <- ggplot2::ggplot() +
    geom_col(newD, mapping = aes_string(x="YEAR", y="totLand", fill = dataType)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0("landings by ",dataType," using a threshold of ",threshold)) +
    ylab("Total Landings (lbs)")

  print(g1)

}
