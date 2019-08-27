#' Test aggregation
#'
#'This will all change so no point documenting it too much.
#'Eventually split each section into its own function for easier future development
#'All plotting functions will need to be finined/generalized
#'Also need to adapt code to allow user to process on a year by year basis
#'
#'@param landingsData tidy data frame. Landings by YEAR,QTR,NEGEAR,MARKET_CODE,landings_land,landings_nn,len_totalNumLen,len_numLengthSampls
#'@param lengthData tidy data frame. Length data by YEAR,QTR,NEGEAR,MARKET_CODE, LENGTH, NUMLEN
#'@param species_itis numeric scalar. species_itis code relating to the landings and length data
#'@param landingsThresholdGear numeric scalar (proportion). Minimum proportion of cumulative landings to avoid aggregation of gear. Default = .9
#'@param nLengthSamples numeric scalar. The minimum number of length sample sizes required to avoid combination of data. Dfault = 1
#'@param pValue numeric scalar. Threshold pvalue for determining significance of ks test for length samples
#'@param proportionMissing numeric scalar. Proportion of missing samples allowed per YEAR for each MARKET_CODE/GEAR combination). Default = 0.2
#'@param outputDir Character string. Path to output directory (png files saved here)
#'@param outputPlots Boolean. Should plots be created. T or F (Default = F)
#'@param logFile character string. Specify the name for the log file generated describing all decisions made.
#'
#'@importFrom dplyr "summarize" "summarise" "group_by" "filter" "select" "arrange" "mutate"
#'@importFrom magrittr "%>%"
#'
#'@export

#channel <- cfdbs::connect_to_database("sole","abeet") #eventually remove this

test_aggregation <- function(landingsData=sampleData_164744,lengthData=sampleLengths_164744,species_itis=164744,
                              landingsThresholdGear = .90, nLengthSamples = 1, pValue = 0.05, proportionMissing= .2,
                              outputDir=here::here("output"), outputPlots=F, logfile="logFile.txt") {

  speciesName <- as.character(marketCodeLookupTable %>% dplyr::filter(SPECIES_ITIS == species_itis) %>% dplyr::select(COMMON_NAME)  %>% dplyr::distinct())
  outputDir <- paste0(outputDir,"/",speciesName)
  if (!dir.exists(outputDir)){dir.create(outputDir)} # create directory to store exploratory/informational plots
  write_to_logfile(outputDir,logfile,"",label="DECISIONS MADE DURING AGGREGATION OF DATA")
  write_to_logfile(outputDir,logfile,data=as.character(species_itis),label="Species_itis",append=T)



  otherGear <- "998"

  data <- list()
  data$landings <- landingsData
  data$lengthData <- lengthData
  sampleStartYear <- min(as.numeric(unique(data$lengthData$YEAR)))
  numYears <- length(unique(data$landings$YEAR))
  #######################################################
  ####### GEARs #########################################
  #######################################################
  # Now deal with Gary's schematic.
  # 1. aggregate the gears based on landings


  data <- aggregate_gear(data,otherGear,landingsThresholdGear)
  gearList <- unique(data$landings$NEGEAR)
  # look at the summary stats/plots after aggregation
  summary_stats(data$landings,species_itis,outputDir,outputPlots)
  # take a look at length distribution of size categories
  plot_length_histogram(data$lengthData,species_itis,outputDir,outputPlots)

  #######################################################
  ####### MARKET CODES ##################################
  #######################################################

  # 2 . combine market category (This will be difficult) market category description are unique to species and not ordinal.
  # Use distributions to aggregate instead of Market category
  # Need to keep Unclassified and Unknown categories separate from known market categories
  # if market category has landings but no length data at all. Then the landings need to be lumped into a
  # neighboring size class. Very subjective but dont lump into unclassified/ unknown


  data <- aggregate_market_codes(data,pValue,outputDir,outputPlots,logfile)
  marketCodeList <- unique(data$landings$MARKET_CODE)


  #######################################################
  ####### QTR, SEMESTER, ANNUAL #########################
  #######################################################

  # 3. look at QTR to see if need to lump quarters or borrow from other years
  # plot all diagnostics again with current aggregated data
  plot_market_code_by_qtr(data,9,outputDir,outputPlots)
  write_to_logfile(outputDir,logfile,paste0("Length samples started in ",as.character(sampleStartYear),". All landings prior to this year will use this years data"),label=NULL,append = T)
  #write_to_logfile(outputDir,logfile,"Other gear (code 998) will be aggregated similarly to other gears",label="market code by qrt",append = T)


  # find set of years where samples were not taken but landings were. Early years
  landYrs <- unique(data$landings$YEAR)
  missingEarlyYears <- seq(min(landYrs),sampleStartYear-1)

  # Need to start in latest year and work backward, filling in for each gear type
  write_to_logfile(outputDir,logfile,data="",label="Length samples by QTR.  YEAR-QRT missing: YEAR-QTR used",append=T)
  yrsList <- unique(data$landings$YEAR) # full list of years in landings data


  for (gearType in gearList[2]) { # loop over gear types
    print(gearType)
    for (marketCode in marketCodeList[2]) { # loop over market category
      if (marketCode == "UN") next
      print(marketCode)

      # filter data by gear, market code and years where samples were taken
      QTRData <- data$landings %>% dplyr::filter(YEAR >= sampleStartYear & NEGEAR == gearType & MARKET_CODE == marketCode)
      # find number of times no samples seen by QTR
      aggQTRData <- QTRData %>% dplyr::group_by(QTR) %>% dplyr::summarise(numSamples=sum(len_numLengthSamples < nLengthSamples))
      aggYEARData <- QTRData %>% dplyr::group_by(YEAR) %>% dplyr::summarize(totLand=sum(landings_land),numSamples=sum(len_numLengthSamples < nLengthSamples))
      # determines if number of years missing data is small enough to borrow data from other years
      write_to_logfile(outputDir,logfile,data=paste0("NEGEAR: ",gearType," - MARKET_CODE: ",marketCode," - Approx : ",mean(aggQTRData$numSamples)," years have NO length samples"),label=NULL,append=T)
      write_to_logfile(outputDir,logfile,data=paste0("NEGEAR: ",gearType," - MARKET_CODE: ",marketCode," - There are ",sum(aggYEARData$numSamples)," years out of ",length(aggYEARData$numSamples)," (in which there are landings) where no length samples exist"),label=NULL,append=T)

      if (mean(aggQTRData$numSamples) < proportionMissing*numYears) { # if mean number of missing years < specified tolerance
        # fill in missing QTRS using previous QTR(s): Borrow length sample data frm previous QTR

        # Deal with Early Years where we have landings data but no length samples
        # Repeat the length samples from the first year of sampling to all earlier years.
        # populate len_numLengthSamples in landings data

        for (iyear in missingEarlyYears) {
          for (iqtr in 1:4) {
            missingRow <- expand.grid(YEAR=iyear,QTR=iqtr)
            # check to see if any landings were recorded. if not then dont need lengths
            earlyData <- data$landings %>% dplyr::filter(YEAR == iyear & QTR == iqtr & NEGEAR == gearType & MARKET_CODE == marketCode )
            if (dim(earlyData)[1] == 0) { # not recorded landings
              next
            }
            numSamples <- update_early_years(QTRData,max(missingEarlyYears),iqtr)
            print(c(iyear,iqtr))
            print(numSamples)
            # numSamples <- QTRData %>% dplyr::filter(YEAR==(max(missingEarlyYears)+1) & QTR==iqtr) %>%
            #   dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)
            data <- update_length_samples(data,missingRow,gearType,marketCode,numSamples)

            write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",iyear,"-",iqtr," used length samples from ",numSamples$YEAR," - MARKET_CODE:",marketCode),label=NULL,append=T)
          }
#          write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",iyear," used length samples from ",max(missingEarlyYears)+1," for all QTRS   - MARKET_CODE:",marketCode),label=NULL,append=T)

        }
        return(data)
        # determin which QTRs have missing length samples
        missingQTRs <- QTRData %>% dplyr::group_by(YEAR,QTR) %>% dplyr::summarize(numSamples = sum(len_numLengthSamples < nLengthSamples)) %>% dplyr::filter(numSamples >= nLengthSamples)
        message("The following table shows YEAR/QTR that have missing samples:")
        message("We will borrow length data from the same QTR in the previous YEAR.")
        print(missingQTRs[,c(1:2)])
        # cycle through the table of YEAR/QTR combos
        for (iyear in 1:dim(missingQTRs)[1]) {
          # select same quarter in the previous year if not zero
          numSamples <- missing_length_by_qtr(QTRData,missingQTRs,iyear)
          if (numSamples$len_numLengthSamples == 0) {
            # still zero after going back many years!! This could be a problem.
            stop("PROBLEM!!!. Finding zero length samples in all previous years")
          }
          print("loops")
          # update year/qtr info with filled in data
          data <- update_length_samples(data,missingQTRs[iyear,],gearType,marketCode,numSamples)
          # write to logfile
          write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",missingQTRs$YEAR[iyear],"-",missingQTRs$QTR[iyear]," used length samples from ",numSamples$YEAR,"-",numSamples$QTR,"   - MARKET_CODE:",marketCode),label=NULL,append=T)
        }

      } else if (0) {
        # maybe add rules for semester aggregation if we can work out a plan

      } else if (0) {
        # maybe add a combo of QTR, SEMESTER, ANNUAL

      } else  { # too many QTRs missing. aggregate the entire MARKET_CODE over QTR to annual data
        # recode all QTRS to 0
        print(paste0("Aggregate over QTRS to YEARly-",marketCode))
        print(aggYEARData)
        if ((sum(aggYEARData$numSamples)/length(aggYEARData$numSamples)) > proportionMissing ){
          # Too many YEARs without length data for MARKET_CODE. Aggregate with another MARKET_CODE
          message("Annual data: There are ",sum(aggYEARData$numSamples)," years out of ",length(aggYEARData$numSamples)," (in which there are landings) where no length samples exist")
          message("Insufficient length samples at aggregate level: YEAR")
          summarizedData <- data$landings %>%
            dplyr::group_by(MARKET_CODE) %>%
            dplyr::filter(NEGEAR == gearType) %>%
            dplyr::summarise(totalLandings = sum(landings_land, na.rm = TRUE),len_numLengthSamples=sum(len_numLengthSamples,na.rm=T)) %>%
            dplyr::arrange(desc(totalLandings))
          print(summarizedData)

          # do ks test to help make a decision. compare all length distributions of this year
          landings <- data$landings %>% dplyr::filter(NEGEAR==gearType)
          lengthData <- data$lengthData %>% dplyr::filter(NEGEAR==gearType)
          codesToAggregate <- compare_length_distributions(landings,lengthData,variableToAggregate = "MARKET_CODE", groupBy=c("LENGTH","NUMLEN","MARKET_CODE"), pValue,outputDir,logfile)

          # plot number of samples by year
          if (!is.null(codesToAggregate)) {
            # we need to aggregate size classes
          } else {
            message("length distributions (by MARKET_CODE for gear = ",gearType,") are significantly different. Can not aggregate based on lengths.")
          }

          options <- readline(prompt=paste0("There are 2 options for gear = ",gearType,": 1. Grab missing lengths from previous years or 2. aggregate MARKET_CODE. Select 1 or 2: "))

          if (options == 2) {
            newCode <- readline(prompt=paste0("Gear = ",gearType,": Which Market category would you like to combine ",marketCode, " with: "))
            message(paste0("OK. We will combine ",sum(data$landings$MARKET_CODE == marketCode)," records for ",marketCode, " with ",newCode))
            # aggregate again. This takes care or early years also
            filteredLandings <- aggregate_data_by_class(data$landings,variable="MARKET_CODE",classes=c(marketCode,newCode),conditionalOn=c("NEGEAR",gearType),dataset="landings")
            data$landings <- filteredLandings
            lengthData <- aggregate_data_by_class(data$lengthData,variable="MARKET_CODE",classes=c(marketCode,newCode),conditionalOn=c("NEGEAR",gearType),dataset="lengths")
            data$lengthData <- lengthData
            write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",paste0(c(marketCode,newCode),collapse=" to ")),label="market code relabelling, (lack of length samples at QTR & YEAR level) from:to",append=T)

          } else if (options == 1) {
            # fill in using previous years lengths
            # aggregate over QTR to YEAR for NEGEAR,MARKET_CODE. plot/test length over time
            # code all QTRs to 0 . This is essentially Annual data
            write_to_logfile(outputDir,logfile,data=paste0("Aggregate over QRTs to Annual data"),label=NULL,append=T)

            conditionalOn <- rbind(c("NEGEAR",gearType),c("MARKET_CODE",marketCode))

            # aggregate QTR to annual. Code QTR = 0
            for (iq in 1:4) {
              filteredLandings <- aggregate_data_by_class(data$landings,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="landings")
              data$landings <- filteredLandings
              lengthData <- aggregate_data_by_class(data$lengthData,variable="QTR",classes=c(iq,0),conditionalOn=conditionalOn,dataset="lengths")
              data$lengthData <- lengthData
              write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - QTR ",iq," codes to 0 - MARKET_CODE:",marketCode),label=NULL,append=T)
            }

            YEARData <- data$landings %>%
              dplyr::filter(YEAR >= sampleStartYear & QTR == 0 & NEGEAR == gearType & MARKET_CODE == marketCode)

            # for each year with missing length find closest year with length samples
            missingYrs <- aggYEARData %>% dplyr::filter(numSamples == T)
            print(missingYrs)

            for (iyear in 1:dim(missingYrs)[1]) {
              targetYEAR <- missingYrs$YEAR[iyear]
              numSamples <- missing_length_by_year(YEARData,targetYEAR,nLengthSamples)
              print(numSamples)
              #duplicate length samples to missing year
              missingRow <- expand.grid(YEAR=targetYEAR,QTR=0)
              data <- update_length_samples(data,missingRow,gearType,marketCode,numSamples)

              write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",targetYEAR," used length samples from ",numSamples$YEAR," - MARKET_CODE:",marketCode),label=NULL,append=T)
            }
            # now deal with Early Years where we have landings data but no length samples were taken
            # repeat the length samples from the first year of sampling to all earlier years.
            # populate len_numLengthSamples in landings data
            for (iyear in missingEarlyYears) {
              missingRow <- expand.grid(YEAR=iyear,QTR=0)
              numSamples <- data$landings %>% dplyr::filter(YEAR==(max(missingEarlyYears)+1) & QTR==0) %>%
                dplyr::select(YEAR,QTR,len_totalNumLen,len_numLengthSamples)

              data <- update_length_samples(data,missingRow,gearType,marketCode,numSamples)
              write_to_logfile(outputDir,logfile,data=paste0("Gear: ",gearType," - ",iyear," used length samples from ",max(missingEarlyYears)+1," - MARKET_CODE:",marketCode),label=NULL,append=T)
            }

          }
        } else {
          # need to aggregate MARKET_CODE over QTRs to YEAR

        }

      }

    }
  }



  # produce report on decisions made and include figures

  #  write_to_logfile(outputDir,logfile,"Other gear (code 998) will be aggregated similarly to other gears\n",label=NULL,append = T)
  message(paste0("Check the logfile = ",outputDir,"/",logfile," for details regarding aggregation choices") )



  return(data)


}
