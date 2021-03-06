% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_clv_time_datetime.R
\docType{class}
\name{clv.time.datetime-class}
\alias{clv.time.datetime-class}
\title{POSIXct based time-units}
\description{
Virtual base class for time units that define periods smaller than a single day.
This class processes time and dates at the single seconds level and
stores all timepoints using data type \code{POSIXct}.
}
\section{Slots}{

\describe{
\item{\code{timepoint.estimation.start}}{Single \code{POSIXct} that stores the start of the estimation period.}

\item{\code{timepoint.estimation.end}}{Single \code{POSIXct} that stores the end of the estimation period.}

\item{\code{timepoint.holdout.start}}{Single \code{POSIXct} that stores the start of the holdout period.}

\item{\code{timepoint.holdout.end}}{Single \code{POSIXct} that stores the end of the holdout period.}

\item{\code{timezone}}{Single character vector indicating the enforced timezone when parsing inputs to timepoints.
Defaults to UTC, but may be overwritten by a subclass to enforce a different timezone than UTC.}
}}

\seealso{
For time unit implementations based on this class: \linkS4class{clv.time.hours}
}
\keyword{internal}
