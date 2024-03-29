% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expand_landings_to_lengths.R
\name{expand_landings_to_lengths}
\alias{expand_landings_to_lengths}
\title{Proportions commercial landings based on length sample distribution}
\usage{
expand_landings_to_lengths(landingsData, lengthData, lengthWeightParams)
}
\arguments{
\item{landingsData}{Tibble. Aggregated landings data. YEAR, QTR, NEGEAR, MARKET_CODE,landings_land (metric tonnes), landings_nn (# trips), len_totalNumLen (# fish lengths), len_numLengthSamples (# independent samples).}

\item{lengthData}{Tibble. Aggregated length data. YEAR, QTR, NEGEAR, MARKET_CODE, LENGTH (length of fish), NUMLEN (# fish at LENGTH)}

\item{lengthWeightParams}{List. alpha = intercept, betas = slope(s), var = residual variance used to formulate the mean (?see Notes section below)}
}
\value{
A Tibble of expanded landings to represent weight of landings by length
\item{YEAR}{Year of landings}
\item{NEGEAR}{3 digit gear code as defined in cfdbs.gear}
\item{TIME}{Quarter/Half year of landings (The presence of this field depends on whether it was present in the \code{landingsData}}
\item{MARKET_CODE}{Market code assigned to landed fish (The presence of this field depends on whether it was present in the \code{landingsData}}
\item{LENGTH}{Length of sampled fish}
\item{NUMLEN}{number of sampled for fish stated LENGTH}
\item{weight}{expanded weight (mt) of all fish of given LENGTH in YEAR, NEGEAR etc..}
}
\description{
LandingsData and data used to estimate lengthWeightParams must be in the same units. Lbs or MT
}
\section{Notes}{


The length weight relationship (see \code{\link{fit_length_weight}}) is fit assuming log normal errors (normal on the log scale).
Therefore when exponentiating a correction for the estimate is required:

E(W) = \eqn{\alpha  L^\beta exp(\sigma^2 / 2)}
}

\section{Expansion calculations}{


For each unique category (YEAR, TIME, NEGEAR, MARKET_CODE) weights (mean weights, metric tons)
are attributed to the sampled individuals lengths using the weight-length relationship above.
This distribution of weights by length is then scaled such that
the sum of weights (over lengths) = the total landed weight from the landingsData.
This scaling assumes that the landed (commercial) fish have the same length distribution as the sampled fish.
}

