#' Records entries to a logfile
#'
#'@param outputDir Character string. directory to save output
#'@param logfile Character string. Name of logfile
#'@param data data to write to file
#'@param label Character string. Description of the data being written
#'@param append Boolean. append to current logfile (T) or overwrite (F)
#'
#'@return Nothing: outout written to log file
#'
#'@noRd



write_to_logfile <- function(outputDir,logfile,data,label,append = F) {

  if (append == F){ # open file
    fileConn<-file(paste0(outputDir,"/",logfile),open="w")
  } else (
    fileConn<-file(paste0(outputDir,"/",logfile),open="a")

  )
  if (!is.null(label)) {writeLines(label, fileConn)}

  if(is.vector(data)){
    cat(data, file=fileConn)
    cat("\n", file=fileConn, append=T)
  } else if (length(dim(data))==2){ # matrix or dataframe
    namesData <- sprintf("%15s ",names(data))
    cat(namesData, file=fileConn, append=T) # print field names
    cat("\n", file=fileConn, append=T)

    for (irow in 1:nrow(data)){
      printData <-  sprintf("%15s ",unlist(data[irow,]))
      cat(printData, file=fileConn, append=T)
      cat("\n", file=fileConn, append=T)
    }

  } else{
    stop("Can only pass vectors or matrices to logfile")
  }

  cat("---------------\n", file=fileConn, append=T)


  close(fileConn)

}
