#'Basic stats from Trip AA tables and ADIOS
#'
#'Exploratory code to examine difference in number of unique trips between AA tables and ADIOS (mv_cf_landings).
#'
#'@param channel an RODBC object (see \code{\link{connect_to_database}})
#'@param yearRange a vector or years. Default =1964:2017
#'
#'@return A matrix 54 x 3
#'
#' \code{tableCount}{
#'
#' Column 1 - The year
#'
#' Column 2 - The number of Trips from the AA tables
#'
#' Column 3 - The number of trips from ADIOs (mv_cf_landings)
#'
#' Column 4 - Difference in number of Trips
#' }
#'
#'

aa_count_trips <- function(channel,yearRange=1964:2017) {

  tableCount <- NULL
  for (iyear in yearRange) {
    if (iyear <= 1981) {
      table <- paste0("wolandt",iyear %% 100)
    } else if (iyear <= 1993){
      table <- paste0("wodett",iyear %% 100)
    } else {
      table <- paste0("cfdett",iyear,"AA")
    }

   sqlAA <- paste0("select count(year) from cfdbs.",table,";")
   resAA <- as.numeric(unlist(DBI::dbGetQuery(channel,sqlAA)))

   sqlADIOS <- paste0("select count(distinct link) from stockeff.mv_cf_landings where year = ",iyear)
   resADIOS <- as.numeric(unlist(DBI::dbGetQuery(channel,sqlADIOS)))


   print(sqlAA)
   print(sqlADIOS)
   print(c(iyear,resAA,resADIOS,resAA-resADIOS))

   tableCount <- rbind(tableCount,c(iyear,resAA,resADIOS,resAA-resADIOS))


  }
  return(tableCount)
}
