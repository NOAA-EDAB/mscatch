# Creates separate shape files for each EPU (GB,GOM,MAB,SS) which include all 10 min square info
#
# Reads in EPU shape files (@ 10 min square level) and reads in the grid of 10 minute squares
# then extracts 10 min square data that make up each EPU regiuon
#
# packages
# rgdal, rgeos

create_EPU_10minsq <- function() {
  ## 10min square EPUs
  path <- "data-raw/"
  options(warn=-1) # supress writeOGR  warning messages

  # reads in shape files for EPUS and 10 min square grid
  EPU10MinSq <-  rgdal::readOGR(dsn = paste0(path,'EPU_WITH_ESTUARIES'), layer = 'EPUS_FULL')
  Grid10MinSq <-  rgdal::readOGR(dsn = paste0(path,'Grid_10minSQ'), layer = 'TMSQ')

  EPUNames <- as.vector(EPU10MinSq@data$EPU)

  for (rname in 1:length(EPUNames)) {
    print(paste0("creating 10 min square EPU for ",EPUNames[rname]))

    # subsets the EPU polygon from the shapefile of all 4 polygons
    EPURegion <- raster::subset(EPU10MinSq,EPU == EPUNames[rname])
    # intersets EPU polygon with 10 min squares to select ALL 10 min squares
    EPU <- raster::intersect(EPURegion,Grid10MinSq)
    #raster::plot(EPU)

    # create a new shape file for shiny app
    rgdal::writeOGR(EPU,dsn=paste0(path,"EPU_10mins"),layer=paste0("10MinSq_",EPUNames[rname]),
                    driver="ESRI Shapefile",overwrite_layer=TRUE)
  }

  print("Warning: ABEET - > rgdal::writeOGR clips the names of some of the column names merged from Grid_10minSQ (from 10 characters to 7)")

}





