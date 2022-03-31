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

plot_lengths_by_type <- function(species,data,threshold,outputPlots,outputDir,plotID,type) {


  if (outputPlots == F) return()

  if (type == "gear") {
    png(paste0(outputDir,"/",plotID,"_lengths_by_gear.png"))

    newD <- data %>%
      dplyr::group_by(YEAR,NEGEAR) %>%
      dplyr::summarize(nSamples=sum(len_numLengthSamples,na.rm = TRUE))

    g1 <- ggplot2::ggplot(newD) +
      ggplot2::geom_col(mapping = ggplot2::aes(x=YEAR, y=nSamples, fill = NEGEAR)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(title = paste0("Length samples for ",species," by NEGEAR")) +
      ggplot2::ylab("Number of samples")

    print(g1)

    dev.off()


  } else if (type == "market") {

    png(paste0(outputDir,"/",plotID,"_lengths_by_market_cat.png"))

    newD <- data %>%
      dplyr::group_by(YEAR,MARKET_CODE) %>%
      dplyr::summarize(nSamples=sum(len_numLengthSamples,na.rm = TRUE))

    g1 <- ggplot2::ggplot(newD) +
      ggplot2::geom_col(mapping = ggplot2::aes(x=YEAR, y=nSamples, fill = MARKET_CODE)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(title = paste0("Length samples for ",species," by MARKET_CODE")) +
      ggplot2::ylab("Number of samples")


    print(g1)

    dev.off()


  } else if (type == "QTR") {
    png(paste0(outputDir,"/",plotID,"_lengths_by_QTR.png"))

    newD <- data %>%
      dplyr::group_by(YEAR,QTR) %>%
      dplyr::summarize(nSamples=sum(len_numLengthSamples,na.rm = TRUE))

    g1 <- ggplot2::ggplot(newD) +
      ggplot2::geom_col(mapping = ggplot2::aes(x=YEAR, y=nSamples, fill = as.factor(QTR))) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(title = paste0("Length samples for ",species," by QTR"),fill="QTR") +
      ggplot2::ylab("Number of samples")


    print(g1)

    dev.off()

  } else if (type == "SEMESTER") {

    png(paste0(outputDir,"/",plotID,"_lengths_by_SEMESTER.png"))

    newD <- data %>%
      dplyr::group_by(YEAR,SEMESTER) %>%
      dplyr::summarize(nSamples=sum(len_numLengthSamples,na.rm = TRUE))

    g1 <- ggplot2::ggplot(newD) +
      ggplot2::geom_col(mapping = ggplot2::aes(x=YEAR, y=nSamples, fill = as.factor(SEMESTER))) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(title = paste0("Length samples for ",species," by SEMESTER"),fill="SEMESTER") +
      ggplot2::ylab("Number of samples")


    print(g1)

    dev.off()


  }

}
