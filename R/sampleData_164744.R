#' A sample dataSet for species_itis 164744
#'
#'Highly aggregated data. In the desired format to undergo length expansion.
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
#'
#'@source join between mv_cf_landings and mv_cf_len
"sampleData_164744"
