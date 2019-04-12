#' A lookup table for species_itis and market code
#'
#'Market codes are not on an ordinal scale therefore it is difficult to programatically aggregate neighboring size classes.
#'Ordinal scale added manually for the included species
#'
#' @format A dataframe with 7 columns
#' \describe{
#'   \item{COMMON_NAME}{Common name for species}
#'   \item{SCIENTIFIC_NAME}{scientific name for species}
#'   \item{SPECIES_ITIS}{define this}
#'   \item{NESPP4}{US North east 4 digit species code}
#'   \item{MARKET_DESC}{Description of the fish part}
#'   \item{MARKET_CODE}{2 digit market code}
#'   \item{MARKET_SCALE}{Ordinal scale for market code within species}
#'}
#'
#'
#'@source SPECIES_ITIS_NE table
"marketCodeLookupTable"
