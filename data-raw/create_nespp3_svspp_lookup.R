# selects species from cfdbs and svdbs and joins them to crete a link between
# nespp3 and svspp
#
#
library(magrittr)


create_nespp3_svspp_lookup <- function(channel,overwrite=F){
  # Same order as Hydra
  # Spiny Dog, winter skate, herring , cod, haddock, yellowtail fl, winter fl, mackerel, silver hake, goosefish,pollock, acadian redfish
  speciesList <- c(160617,564145,161722,164712,164744,172909,172905,172414,164791,164499,164727,166774)
  speciesList <- sprintf(paste0("%06d"),speciesList)
  speciesList <-  paste0("'",speciesList,"'",collapse=",")

  # get species_itis from cfdbs data base
  sql <- paste0("select distinct COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4 from cfdbs.species_itis_ne where SPECIES_ITIS in (",speciesList,") order by SPECIES_ITIS, NESPP4;")

  cfdbsLookupTable <- DBI::dbGetQuery(channel,sql)
  cfdbsLookupTable$NESPP3 <- substr(cfdbsLookupTable$NESPP4,start=1,stop=3)

  cfdbsLookupTable <- cfdbsLookupTable %>% dplyr::select(COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP3) %>% dplyr::distinct()

  # now lookup svspp from svdbs database
  svdbsSpecies <- paste0("'",cfdbsLookupTable$SCIENTIFIC_NAME,"'",collapse=",")
  sql <- paste0("select distinct COMNAME, SCINAME, SVSPP from svdbs.svspecies_list where SCINAME in (",svdbsSpecies,")")
  svdbsLookupTable <- DBI::dbGetQuery(channel,sql)
  names(svdbsLookupTable) <- c("COMMON_NAME","SCIENTIFIC_NAME","SVSPP")

  # join tables
  speciesLookupTable <- dplyr::left_join(cfdbsLookupTable,svdbsLookupTable, by="SCIENTIFIC_NAME") %>%
    dplyr::select(SCIENTIFIC_NAME,SPECIES_ITIS,dplyr::everything()) %>% dplyr::as_tibble()

  usethis::use_data(speciesLookupTable,overwrite=overwrite)
  #save(speciesLookupTable,file=paste0(here::here("data"),"/speciesLookupTable.RData"))


  return(speciesLookupTable)
}

