#' Format get_species_object for rmds
#'
#'
#'
#'
create_species_rules_table <- function(spitis) {

  speciesRules <- mscatch::get_species_object_mskeyrun(spitis)

  speciesRules$gearCodes <- NULL
  speciesRules$marketCodes <- NULL
  speciesRules$statStockArea <- NULL

  for (ifi in names(speciesRules)) {

    if (length(speciesRules[[ifi]]) > 1) {
      speciesRules[ifi] <- paste(speciesRules[[ifi]], collapse = ",")
    }

  }
  zz <- as.data.frame(speciesRules)


  tab <- zz  %>%
    dplyr::rename(species = speciesName) %>%
    dplyr::select(-c(species_itis,SVSPP)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(value = V1) %>%
    flextable::flextable() %>%
    flextable::theme_vanilla()

  return(tab)

}

