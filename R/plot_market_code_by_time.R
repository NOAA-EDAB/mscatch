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
#'@noRd

plot_market_code_by_time <- function(data,plotID,outputDir,outputPlots=T,aggregate_to = "QTR") {

  if (outputPlots == F) return()

  if (aggregate_to == "YEAR") {
    aggregate_to <- "QTR"
  }

  # join landings and lengths to plot length distribution over time
  ddjoin <- data$landings %>% dplyr::left_join(.,data$lengthData,by = c("YEAR",aggregate_to,"NEGEAR","MARKET_CODE"))

  # for each geartype
  for (gearType in unique(data$landings$NEGEAR)) {

    png(paste0(outputDir,"/",plotID,"_market_category_",aggregate_to,"_lengths_",as.character(gearType),".png"))

    dd <- ddjoin %>% dplyr::filter(NEGEAR == gearType) %>% dplyr::filter(len_numLengthSamples != 0)

    p <- ggplot2::ggplot(data = dd) +
      ggplot2::geom_boxplot(mapping = ggplot2::aes(x=YEAR, y=LENGTH, group=YEAR)) +
      ggplot2::facet_grid(rows=ggplot2::vars(MARKET_CODE),cols=ggplot2::vars(.data[[aggregate_to]]))+
      ggplot2::ggtitle(paste0("Length distributions by Market Code and ",aggregate_to," (gear type = ",as.character(gearType),")")) +
      ggplot2::ylab("Length (cm)")
    print(p)
    dev.off()

#     png(paste0(outputDir,"/",plotID,"_market_category_",aggregate_to,"_landings_",as.character(gearType),".png"))
#     p <- ggplot2::ggplot(data = data$landings %>% dplyr::filter(NEGEAR == gearType)) +
#       ggplot2::geom_line(mapping = ggplot2::aes(x=as.numeric(YEAR), y=len_numLengthSamples)) +
# #      ggplot2::facet_grid(facets= c("MARKET_CODE",aggregate_to)) +
#       ggplot2::facet_grid(rows=ggplot2::vars(MARKET_CODE),cols=ggplot2::vars(.data[[aggregate_to]])) +
#       ggplot2::ggtitle(paste0("Market Code by ",aggregate_to," (gear type = ",as.character(gearType),")")) +
#       ggplot2::ylab("Number of Length Samples")
#     print(p)
#
#     dev.off()


    # Take a look at length distribution by QTR and MARKET CODE
    d <- data$lengthData %>% dplyr::filter(NEGEAR == gearType) %>%
      dplyr::group_by(MARKET_CODE,.data[[aggregate_to]],LENGTH) %>%
      dplyr::summarise(numlens=sum(as.numeric(NUMLEN)),.groups = "drop")

    # check to see if any lengths
    if(nrow(d) > 0) {
      png(paste0(outputDir,"/",plotID+1,"_market_category_",aggregate_to,"_length_distribution_",as.character(gearType),".png"))

      p <-   ggplot2::ggplot(data = d) +
        ggplot2::geom_bar(stat="identity",mapping= ggplot2::aes(x=LENGTH,y=numlens),na.rm=T) +
        ggplot2::facet_grid(rows=ggplot2::vars(.data[[aggregate_to]]),cols=ggplot2::vars(MARKET_CODE)) +
        ggplot2::ggtitle(paste0("By ",aggregate_to," and MARKET_CODE  (gear type = ",as.character(gearType),")")) +
        ggplot2::ylab("Number at length")
      print(p)

      dev.off()
    }


#     png(paste0(outputDir,"/",plotID+2,"_market_category_",aggregate_to,"_number_samples_",as.character(gearType),".png"))
#
#     d <- data$landings %>%
#       dplyr::filter(NEGEAR == gearType) %>%
#       dplyr::group_by(MARKET_CODE,.data[[aggregate_to]]) %>%
#       dplyr::summarise(numlens=sum(as.numeric(len_numLengthSamples)==0),
#                        .groups="drop")
#
#     p <-   ggplot2::ggplot(data = d) +
#       ggplot2::geom_bar(stat="identity",mapping = ggplot2::aes(x=1,y=numlens),na.rm=T) +
# #      ggplot2::facet_wrap(~.data[[aggregate_to]]+MARKET_CODE) +
#       ggplot2::facet_grid(rows=ggplot2::vars(.data[[aggregate_to]]),cols=ggplot2::vars(MARKET_CODE)) +
#       ggplot2::ggtitle(paste0("By ",aggregate_to," and MARKET_CODE  (gear type = ",as.character(gearType),")")) +
#       ggplot2::ylab("Number of years with missing length samples") +
#       ggplot2::theme(axis.title.x=ggplot2::element_blank(),
#             axis.text.x=ggplot2::element_blank(),
#             axis.ticks.x=ggplot2::element_blank())
#     print(p)
#
#     dev.off()

#     png(paste0(outputDir,"/",plotID+3,"_number_samples_over_time",as.character(gearType),".png"))
#
#     d <- data$landings %>%
#       dplyr::filter(NEGEAR == gearType) %>%
#       dplyr::group_by(YEAR,MARKET_CODE,.data[[aggregate_to]]) %>%
#       summarise(numlens=sum(as.numeric(len_numLengthSamples)>0))
#
#     p <-   ggplot2::ggplot(data = d, mapping = ggplot2::aes(x=as.numeric(YEAR),y=numlens)) +
#       ggplot2::geom_line() +
# #      ggplot2::facet_wrap(~.data[[aggregate_to]]+MARKET_CODE) +
#       ggplot2::facet_grid(rows=ggplot2::vars(.data[[aggregate_to]]),cols=ggplot2::vars(MARKET_CODE)) +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#       ggplot2::ggtitle(paste0("By ",aggregate_to," and MARKET_CODE  (gear type = ",as.character(gearType),")")) +
#       ggplot2::ylab("Presence of a length sample")
#     print(p)
#
#
#     dev.off()

  }
}
