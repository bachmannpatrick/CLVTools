% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_clv_time_date.R
\docType{class}
\name{clv.time.date-class}
\alias{clv.time.date-class}
\title{Date based time-units}
\description{
Virtual base class for time units that need only whole day granularity.
This class processes time and dates at day level ignoring the time of day
and stores all timepoints using data type \code{Date}.
}
\section{Slots}{

\describe{
\item{\code{timepoint.estimation.start}}{Single \code{Date} that stores the start of the estimation period.}

\item{\code{timepoint.estimation.end}}{Single \code{Date} that stores the end of the estimation period.}

\item{\code{timepoint.holdout.start}}{Single \code{Date} that stores the start of the holdout period.}

\item{\code{timepoint.holdout.end}}{Single \code{Date} that stores the end of the holdout period.}
}}

\seealso{
For time unit implementations based on this class: \linkS4class{clv.time.days}, \linkS4class{clv.time.weeks}, \linkS4class{clv.time.years}
}
\keyword{internal}
