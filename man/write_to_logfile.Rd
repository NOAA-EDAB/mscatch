% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_to_logfile.R
\name{write_to_logfile}
\alias{write_to_logfile}
\title{Records entries to a logfile}
\usage{
write_to_logfile(outputDir, logfile, data, label, append = F)
}
\arguments{
\item{outputDir}{Character string. directory to save output}

\item{logfile}{Character string. Name of logfile}

\item{data}{data to write to file}

\item{label}{Character string. Description of the data being written}

\item{append}{Boolean. append to current logfile (T) or overwrite (F)}
}
\value{
Nothing: outout written to log file
}
\description{
Records entries to a logfile
}
