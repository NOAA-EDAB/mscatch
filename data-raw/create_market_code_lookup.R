# selects
#
#

create_market_code_lookup <- function(channel){
  speciesList <- c(147,212,120,352,365,123,013,081,509)
  speciesList <- sprintf(paste0("%03d"),speciesList)
  speciesList <-  paste0("'",speciesList,"'",collapse=",")

  sql <- paste0("select sppnm8, NEsPP3, MKTCAT, MKTNM from cfdbs.cfspp where nespp3 in (",speciesList,") order by mktcat;")

  query1 <- RODBC::sqlQuery(channel,sql,,errors=TRUE,as.is=TRUE)

  sql <- paste0("select distinct NEsPP4, Market_code from stockeff.mv_cf_landings where nespp3 in (",speciesList,") order by nespp4;")

  query2 <- RODBC::sqlQuery(channel,sql,errors=TRUE,as.is=TRUE)

  query2 <- query2 %>% dplyr::mutate(NESPP4,NESPP3=as.character(substr(NESPP4,1,3)),MKTCAT=as.character(substr(NESPP4,4,4)))

  query <- dplyr::full_join(query1,query2)

  query <- query %>% dplyr::arrange(SPPNM8,MKTCAT)

  return(query)
}
