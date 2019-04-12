# selects species name, code, market code etc so we can assign an ordinal scale to the market codes
#
#
library(magrittr)


create_market_code_species_itis <- function(channel){
  # Same order as Hydra
  # Spiny Dog, winter skate, herring , cod, haddock, yellowtail fl, winter fl, mackerel, silver hake, goosefish
  speciesList <- c(164744,564145,161722,164712,164744,172909,172905,172414,164791,164499)
  speciesList <- sprintf(paste0("%06d"),speciesList)
  speciesList <-  paste0("'",speciesList,"'",collapse=",")

  sql <- paste0("select COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4, MARKET_DESC, MARKET_CODE from cfdbs.species_itis_ne where SPECIES_ITIS in (",speciesList,") order by SPECIES_ITIS, NESPP4;")

  marketCodeLookupTable <- RODBC::sqlQuery(channel,sql,errors=TRUE,as.is=TRUE)

  save(marketCodeLookupTable,file=paste0(here::here("data"),"/marketCodeLookupTable.RData"))
#  return(query)

}


# Note, this may not result in accurate data pulls since some species have 2 NESPP3 codes (Haddock 147, 148)
# specieis_itis is better
create_market_code_lookup <- function(channel){
  # Same order as Hydra
  # Spiny Dog, winter skate, herring , cod, haddock, yellowtail fl, winter fl, mackerel, silver hake, goosefish
  speciesList <- c(352,367,168,081,147,123,120,212,509,013)
  speciesList <- sprintf(paste0("%03d"),speciesList)
  speciesList <-  paste0("'",speciesList,"'",collapse=",")

  sql <- paste0("select sppnm8, NEsPP3, MKTCAT, MKTNM from cfdbs.cfspp where nespp3 in (",speciesList,") order by mktcat;")

  query1 <- RODBC::sqlQuery(channel,sql,errors=TRUE,as.is=TRUE)

  sq2 <- paste0("select distinct NEsPP4, Market_code from stockeff.mv_cf_landings where nespp3 in (",speciesList,") order by nespp4;")

  query2 <- RODBC::sqlQuery(channel,sq2,errors=TRUE,as.is=TRUE)

  query2 <- query2 %>% dplyr::mutate(NESPP4,NESPP3=as.character(substr(NESPP4,1,3)),MKTCAT=as.character(substr(NESPP4,4,4)))

  query <- dplyr::full_join(query1,query2) %>% dplyr::arrange(SPPNM8,MKTCAT)

  return(query)
}


