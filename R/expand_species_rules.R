#' Aggregates data to Time interval and summarizes
#'
#' groups the data by time and summarizes, landings and length sample data after removing NA's
#'
#'@param data List. Landings and length data
#'@param speciesRules List. Containing species specific rules. Default = NULL (Fully automated).
#'Note:  Predefined \code{speciesRules} will be bundled with the package for select species
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param logfile Character string. Specify the name for the log file generated describing all decisions made.
#'
#'@return List
#'
#'\item{landings}{same as input}
#'\item{lengthData}{Same as input}
#'
#'@noRd

expand_species_rules <- function(speciesRules, outputDir,logfile) {

  gearNames <- NULL
  gearCodes <- NULL

  # speciesRules is NULL then just return
  if (!is.null(speciesRules)) {

    gearTypes <- unique(speciesRules$gearCodes$use)

    for (agear in gearTypes) {
      if (tolower(agear) == "other") {next}
      gears <- speciesRules$gearCodes %>%
        dplyr::filter(use == agear) %>%
        dplyr::pull(combine)
      allCodes <- NULL
      for (acode in gears) {
        allCodes <- c(allCodes,paste0(acode,0:9))
      }

      gearNames <- c(gearNames,rep(agear,length(allCodes)))
      gearCodes <- c(gearCodes,allCodes)

    }

    speciesRules$gearCodes <- data.frame(use = c(gearNames,"Other"),
                     combine = c(gearCodes,"all"))


    write_to_logfile(outputDir,logfile,"",label="All NEGEAR2 codes expanded to NEGEAR codes")
  }

  return(speciesRules)
}
