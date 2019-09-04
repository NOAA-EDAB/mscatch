#'test function: devel mode
#'
#'
#'
#'
#'
#'
#'@export

test_expansion <- function() {
  # pull length-weight data for species

  # explore data and fit data
  fit_length_weight()
  # apply equations to lengthData and landingsData to expand
  expand_landings_to_lengths()

  # deal with Unclassified "UN" category. May need couple of options based on data availability
  expand_unclassified()


}
