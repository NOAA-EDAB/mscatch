#' Records entries to a logfile
#'
#'@param outputDir Character string. directory to save output
#'@param logfile Character string. Name of logfile
#'@param data data to write to file
#'@param label Character string. Description of the data being written
#'@param append Boolean. append to current logfile (T) or overwrite (F)
#'
#'@export


write_to_logfile <- function(outputDir,logfile,data,label,append = F) {

  if (append == F){ # open file
    fileConn<-file(paste0(outputDir,"/",logfile),open="w")
  } else (
    fileConn<-file(paste0(outputDir,"/",logfile),open="a")

  )
  if (!is.null(label)) {writeLines(label, fileConn)}

  if(is.vector(data)){
    cat(data, file=fileConn)
  } else if (length(dim(data))==2){ # matrix or dataframe

    for (irow in 1:nrow(data)){
      cat(data[irow,], file=fileConn, append=T)
      cat("\n", file=fileConn, append=T)
    }

  } else{
    stop("Can only pass vectors or matrices to logfile")
  }

  close(fileConn)

}
