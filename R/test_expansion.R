#'test function: devel mode
#'
#'
#'
#'
#'
#'
#'@export

test_expansion <- function(channel,species_itis=164744) {
  # pull length-weight data for species

  # 1.  specify species and create directory to store output
  svspp <- speciesLookupTable %>% dplyr::filter(SPECIES_ITIS == species_itis) %>% dplyr::distinct(SVSPP)
  speciesName <- as.character(marketCodeLookupTable %>% dplyr::filter(SPECIES_ITIS == species_itis) %>% dplyr::select(COMMON_NAME)  %>% dplyr::distinct())
  outputDir <- here::here("output",speciesName)
  if (!dir.exists(outputDir)){dir.create(outputDir)} # create directory to store exploratory/informational plots

  # 2. CONNECT TO database
  #channel <- svdbs::connect_to_database("sole","abeet")

  # 3. aggregate landings
  aggData <- mscatch::aggregate_landings(outputDir=outputDir,species_itis = species_itis )
  # 4. pull length-weight data for species
  message("getting length weight data")
  # grab length-weight data from survey cruises
  lengthWeightData <- svdbs::get_length_weight(channel,year="all", species=as.numeric(svspp))
  # 5. create length weight
  fits <- fit_length_weight(lengthWeightData$data,speciesName,outputDir,logfile="logfile.txt")

  nParams <- length(fits$commonSlope$coefficients)
  lengthWeightParams <- list()
  lengthWeightParams$alpha <- fits$commonSlope$coefficients[1]
  lengthWeightParams$betas <- fits$commonSlope$coefficients[2:nParams]
  lengthWeightParams$var <- sum(fits$commonSlope$residuals^2)/fits$commonSlope$df.residual

  # 6. # apply equations to lengthData and landingsData to expand
  expand_landings_to_lengths(aggData$landings,aggData$lengthData,lengthWeightParams)




  return(lengthWeightData)


  # deal with Unclassified "UN" category. May need couple of options based on data availability
  #expand_unclassified()


}
