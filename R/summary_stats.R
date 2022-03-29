#' Summary Stats
#'
#'summarize the data by frequency of gears, number of market categories etc. Create tables and plots
#'Eventually this needs to be embedded into an rmarkdown document with species title etc.
#'
#'@param data numeric scalar (proportion). Minimum proportion of cumulative landings to avoid aggregation of gear. Default = .9
#'@param speciesName Character string. speciesName
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F

#'
#' @importFrom ggplot2 "ggplot" "aes" "geom_bar" "geom_col" "theme" "element_text" "ylab" "xlab" "labs"
#' @importFrom dplyr "summarize" "summarise" "group_by"
#' @importFrom magrittr "%>%"
#'
#'@noRd

summary_stats <- function(data,speciesName,outputDir,outputPlots) {

  if (outputPlots == F) return()

  # market category by year
  # market <- data %>% group_by(YEAR,MARKET_CODE) %>% summarize(n=n())
  # g <- ggplot() +
  #   geom_col(market, mapping=aes(x=YEAR,y =n, fill = MARKET_CODE)) +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #   labs(title = paste0("MARKET_CODE summed over NEGEAR and QTR")) +
  #   ylab("n")
  #
  # print(g)

  # total landings by gear over time
  # png(paste0(outputDir,"/1_landings_by_gear.png"))
  # gears <- data %>% group_by(YEAR,NEGEAR)%>%summarize(totLand=sum(landings_land,na.rm = TRUE))
  # g <- ggplot() +
  #   geom_col(gears, mapping = aes(x=YEAR, y=totLand, fill = NEGEAR)) +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #   labs(title = paste0("landings of ",speciesName," by NEGEAR")) +
  #   ylab("Total Landings (lbs)")
  #
  # print(g)
  # dev.off()

  # total landings by market category over time
  png(paste0(outputDir,"/2_landings_by_market_cat.png"))
  gears <- data %>% group_by(YEAR,MARKET_CODE)%>%summarize(totLand=sum(landings_land,na.rm = TRUE))
  g <- ggplot() +
    geom_col(gears, mapping = aes(x=YEAR, y=totLand, fill = MARKET_CODE)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0("landings of ",speciesName," by MARKET_CODE")) +
    ylab("Total Landings (lbs)")

  print(g)
  dev.off()


  # length samples by quarter over time
  png(paste0(outputDir,"/3_length_samples.png"))
  lengthSamples <- data %>% group_by(YEAR,QTR) %>% summarize(nSamples=sum(len_numLengthSamples,na.rm = TRUE))
  g <- ggplot() +
    geom_col(lengthSamples, mapping=aes(x=YEAR, y = nSamples, fill=as.factor(QTR))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0("number of length samples"),fill="QTR") +
    ylab("n")

  print(g)
  dev.off()

  # length samples by market_category over time
  png(paste0(outputDir,"/4_length_samples_by_market_cat.png"))
  lengthSamples <- data %>%
    group_by(YEAR,MARKET_CODE) %>%
    summarize(nSamples=sum(len_numLengthSamples,na.rm = TRUE), .groups = "drop")


  g <- ggplot(data=lengthSamples) +
    geom_col( mapping=aes(x=YEAR, y = nSamples, fill=MARKET_CODE)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0("number of length samples")) +
    ylab("n")

  print(g)
  dev.off()

  # length samples by gear over time
  png(paste0(outputDir,"/4b_length_samples_by_gear.png"))
  lengthSamples <- data %>%
    group_by(YEAR,NEGEAR) %>%
    summarize(nSamples=sum(len_numLengthSamples,na.rm = TRUE), .groups = "drop")

  g <- ggplot(data =lengthSamples) +
    geom_col(mapping=aes(x=YEAR, y = nSamples, fill=NEGEAR)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0("number of length samples")) +
    ylab("n")



  print(g)
  dev.off()






}
