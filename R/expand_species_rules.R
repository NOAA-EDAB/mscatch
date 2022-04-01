#' Aggregates data to Time interval and summarizes
#'
#' groups the data by time and summarizes, landings and length sample data after removing NA's
#'
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

    # find all gear code names to be relabelled as
    gearTypes <- unique(speciesRules$gearCodes$use)

    # loop through all codes and pick out all 2 digit codes and expand to 3 digit
    for (agear in gearTypes) {
      if (tolower(agear) == "other") {next}
      gears <- speciesRules$gearCodes %>%
        dplyr::filter(use == agear) %>%
        dplyr::pull(combine)
      allCodes <- NULL
      allGears <- NULL
      # check each code to see if it is a 2 digit code or a 3 digit code
      for (acode in gears) {
        if (base::nchar(acode) == 2) {
          # expand to 3 digit and replicate gear name
          allCodes <- c(allCodes,paste0(acode,0:9))
          allGears <- c(allGears,rep(agear,10))
        } else { # just store 3 character gear code
          allCodes <- c(allCodes,acode)
          allGears <- c(allGears,agear)
        }
      }

#      gearNames <- c(gearNames,rep(agear,length(allCodes)))
      gearNames <- c(gearNames,allGears)
      gearCodes <- c(gearCodes,allCodes)

    }

    speciesRules$gearCodes <- data.frame(use = c(gearNames,"Other"),
                     combine = c(gearCodes,"all"))


    write_to_logfile(outputDir,logfile,"",label="All NEGEAR2 codes have been expanded to NEGEAR codes")
  }

  return(speciesRules)
}
