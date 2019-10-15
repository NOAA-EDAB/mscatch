library(testthat)
library(mscatch)
#1

load(here::here("tests","haddockIntermediateData2.Rdata"))
# assume single length-weight key
nParams <- length(fits$commonSlope$coefficients)
lengthWeightParams <- list()
lengthWeightParams$alpha <- fits$commonSlope$coefficients[1]
lengthWeightParams$betas <- fits$commonSlope$coefficients[2:nParams]
lengthWeightParams$var <- sum(fits$commonSlope$residuals^2)/fits$commonSlope$df.residual

update <- mscatch::expand_landings_to_lengths(landings,lengthData,lengthWeightParams)

# deal with Unclassified "UN" category. May need couple of options based on data availability
dd <- expand_unclassified_landings(update$landings,update$lengthData,nLengthSamples)

# make sure landings are same in length and landings data
len <- dd$lengthData %>%
  dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
  dplyr::summarise(land=sum(weight)) %>%
  dplyr::filter(NEGEAR == "050")


land <- dd$landings %>%
  dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
  dplyr::filter(NEGEAR == "050")



