#' Summary Stats
#'
#'summarize the data by frequency of gears, number of market categories etc. Create tables and plots
#'Eventually this needs to be embedded into an rmarkdown document with species title etc.
#'
#'@param data numeric scalar (proportion). Minimum proportion of cumulative landings to avoid aggregation of gear. Default = .9
#'
#' @importFrom ggplot2 "ggplot" "aes" "geom_bar" "geom_col" "theme" "element_text" "ylab" "xlab" "labs"
#' @importFrom dplyr "summarize" "summarise" "group_by"
#' @importFrom magrittr "%>%"
#'
#'@export

summary_stats <- function(data,species=147) {

  # market category by year
  market <- data %>% group_by(YEAR,MARKET_CODE) %>% summarize(n=n())
  g <- ggplot() +
    geom_col(market, mapping=aes(x=YEAR,y =n, fill = MARKET_CODE)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0("MARKET_CODE summed over NEGEAR and QTR")) +
    ylab("n")

  print(g)

  # total landings by gear over time
  gears <- data %>% group_by(YEAR,NEGEAR)%>%summarize(totLand=sum(landings_land,na.rm = TRUE))
  g <- ggplot() +
    geom_col(gears, mapping = aes(x=YEAR, y=totLand, fill = NEGEAR)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0("landings of ",species," by NEGEAR")) +
    ylab("Total Landings (lbs)")

  print(g)

  # length samples by quarter over time
  lengthSamples <- data %>% group_by(YEAR,QTR) %>% summarize(nSamples=sum(len_numLengthSamples,na.rm = TRUE))
  g <- ggplot() +
    geom_col(lengthSamples, mapping=aes(x=YEAR, y = nSamples, fill=QTR)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0("number of length samples")) +
    ylab("n")

  print(g)






}
