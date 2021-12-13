#'create an expression to use in dplyr::filter statement
#'
#'@param landingsRecord Tibble (1 x n). Single row from landingsData
#'@param categories Character vector. Names of Variables to filter over ("YEAR","QTR","NEGEAR", etc)
#'
#'@return An expression
#'
#'@section Notes:
#'
#'Values are access from the \code{landingsRecord} using the \code{categories} and strung together to make an expression in the form of
#'label == value & label = value etc.
#'
#'
#' @importFrom magrittr "%>%"
#'
#'@noRd

create_filter_expression <- function(landingsRecord,categories){

  dataF <- landingsRecord %>% dplyr::select(categories)

  filterExpression <- purrr::imap(dataF, ~ if(is.character(.x)) sprintf('%s == "%s"', .y, .x)
                           else sprintf('%s == %s', .y, .x)) %>%
    purrr::reduce(stringr::str_c,sep =" & ") %>%
    parse(text = .)

  return(filterExpression)
}
