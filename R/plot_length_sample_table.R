#' plots availability of length samples in a table
#'
#' A table displaying where length samples are available and how many.
#' To facilitate decisions on whether to impute missing lengths and/or define thresholds for
#' length sample use
#'
#'
#'@param data List. landings and length data aggregated to some level
#'@param plotID numeric scalar. number ID of plot
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F
#'
#'@return Nothing
#'
#'@noRd

plot_length_sample_table <- function(data,plotID,outputDir,outputPlots=T,aggregate_to,speciesName) {

  if (outputPlots == F) return()

  if(aggregate_to == "YEAR") {
    aggregate_to <- "QTR"
  }

  landings <- data$landings
  lengths <- data$lengthData
  names(landings)[names(landings) == aggregate_to] <- "TIME"
  names(lengths)[names(lengths) == aggregate_to] <- "TIME"

  # A table per gear type
  gears <- landings %>%
    dplyr::distinct(NEGEAR) %>%
    dplyr::pull()
  nGears <- length(gears)

  marketcats <- landings %>%
    dplyr::distinct(MARKET_CODE) %>%
    dplyr::pull()

  times <- landings %>%
    dplyr::distinct(TIME) %>%
    dplyr::pull()

  for (gear in gears) {

    # table showing length sample availability
    d <- landings %>%
      dplyr::filter(NEGEAR == gear) %>%
      dplyr::rename(landings = landings_land,
                    nfishSampled = len_totalNumLen,
                    nSamples = len_numLengthSamples) %>%
      dplyr::mutate(mtperfish = landings/nfishSampled) %>%
      dplyr::mutate(cell = dplyr::case_when(is.infinite(mtperfish) ~ "",
                                            is.na(mtperfish) ~ "",
                                            TRUE ~ paste0(round(mtperfish,digits = 2)," (",nfishSampled,"/",nSamples,")")) ) %>%
      dplyr::select(-landings_nn,-NEGEAR,-landings,-nfishSampled,-nSamples,-mtperfish)  %>%
      tidyr::pivot_wider(., names_from = c("MARKET_CODE", "TIME"),values_from = "cell") %>%
      dplyr::relocate(sort(colnames(.))) %>%
      dplyr::relocate(YEAR)

    # remove NAs
    d[is.na(d)] <- ""

    # parse variable names into season and market category
    seasons <- gsub("[A-Z+_]","",colnames(d))
    nseasons <- length(unique(seasons)) - 1
    header <- c(1,rep(nseasons,length(marketcats)))
    names(header) <- c(" ",marketcats)

    # create table
    d %>%
      kableExtra::kbl(.,
                      col.names=c(seasons),
                      caption=paste0("Gear Type: ",gear),
                      escape=F) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) %>%
      kableExtra::column_spec(1,border_right = T) %>%
      kableExtra::add_header_above(header) %>%
      kableExtra::save_kable(.,file=paste0(outputDir,"/",plotID,"_",gear,"_length_frequency_table1_",speciesName,".png"))

    # find all years market code, time combinations
    allYears <- d %>%
      dplyr::pull(YEAR)
    grid <- expand.grid(allYears,unique(tail(seasons,-1)),marketcats) %>%
      dplyr::rename(YEAR = Var1,
                    TIME = Var2,
                    MARKET_CODE = Var3) %>%
      dplyr::mutate(vars=paste0(MARKET_CODE,"-",as.character(TIME))) %>%
      dplyr::select(-MARKET_CODE,-TIME)

    # table showing length frequencies
    d2 <- lengths %>%
      dplyr::filter(NEGEAR == gear) %>%
      dplyr::group_by(YEAR,TIME,MARKET_CODE) %>%
      dplyr::summarise(vals = rep(LENGTH,NUMLEN),.groups = "drop") %>%
      dplyr::mutate(vars = paste0(MARKET_CODE,"-",as.character(TIME))) %>%
      dplyr::select(-TIME,-MARKET_CODE)

    # join with expanded grid
    d2 <- grid %>% dplyr::left_join(.,d2, by=c("YEAR","vars"))

    # plot faced grid with length frequencies
    ncols <- length(unique(d2$vars))
    nyrs <- length(unique(d2$YEAR))

    ptab2 <- ggplot2::ggplot(data = d2) +
      ggplot2::geom_histogram(mapping=ggplot2::aes(vals),binwidth = 1) +
      ggplot2::facet_grid(rows=dplyr::vars(YEAR),cols=dplyr::vars(vars),switch = "y") +
      ggplot2::theme(strip.background = ggplot2::element_blank()) +
      ggplot2::xlab("length (cm)") +
      ggplot2::ylab("Frequency") +
      ggplot2::ggtitle(paste0("Gear Type: ",gear))


    ggplot2::ggsave(ptab2,filename=paste0(plotID,"_",gear,"_length_frequency_table2_",speciesName,".png"),
                    path = outputDir,
                    height = nyrs*0.35,
                    width = ncols*1.25,
                    units="in")
    }

}
