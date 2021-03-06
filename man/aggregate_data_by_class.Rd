% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_data_by_class.R
\name{aggregate_data_by_class}
\alias{aggregate_data_by_class}
\title{Aggregates rows of data by a given variable}
\usage{
aggregate_data_by_class(data, variable, classes, conditionalOn = NULL, dataset)
}
\arguments{
\item{data}{tibble. Data set to aggreate}

\item{variable}{Character string. Name of the variable to aggregate over}

\item{classes}{Character vector. Two codes (from the variable) to aggregate (Assign 1st to 2nd)}

\item{conditionalOn}{Character Vector. Name and value of variable to condition the aggregation on eg. c("NEGEAR","050"). Default = NULL}

\item{dataset}{Characterstring. Denoting the type of data set. "landings" or "lengths}
}
\value{
tibble
\item{data}{The same form and content as the input tibble, just aggregated}
}
\description{
The landings or length data are aggregated over a variable (for example MARKET_CODE) conditional on another variable (eg NEGAR == "050")
}
