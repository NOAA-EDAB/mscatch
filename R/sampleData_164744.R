#' A sample dataSet for species_itis 164744
#'
#'Highly aggregated data.
#'Spatially, the data is from entire extent of fishing reporting area.
#'This is the required format to undergo a laength expansion
#'
#' @format A dataframe (tibble) with 8 columns
#' \describe{
#'   \item{YEAR}{year}
#'   \item{QTR}{quarter year designation}
#'   \item{NEGEAR}{3 digit NE code to represent fishing gear}
#'   \item{MARKET_CODE}{2 digit market code}
#'   \item{landings_land}{Total landings (lbs). From mv_cf_landings}
#'   \item{landings_nn}{Number of landings (Trips, subtrips). From mv_cf_landings}
#'   \item{len_totalNumLen}{number of actual fish lengths taken. combined over numLengthSamples. From mv_cf_len}
#'   \item{len_numLengthSamples}{Number of length samples for YEAR,QTR,NEGEAR,MARKET_CODE. From mv_cf_len}
#'}
#'
#'@section Data Pull (SQL):
#'
#'The SQL call for the test data is:
#'
#'"select year, month, day, negear, negear2, toncl2, nespp3, nespp4, species_itis, market_code, area, ntrips, mesh,df, da , spplndlb,spplivlb, trplndlb,trplivlb,GIS_LAT,GIS_LON  from stockeff.mv_cf_landings where  (species_itis in ('164744'))"
#'
#'and is called by:
#'
#'cfdbs::get_landings(channel,year="all",areas="all",gear="all",tonnage="all",species=164744,species_itis=T)
#'
#'
#'
#'@source join between mv_cf_landings and mv_cf_len
"sampleData_164744"
