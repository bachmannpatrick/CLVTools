#' @title Summarizing a CLV time object
#' @param object A CLV time object
# These are parameters for the print functions whose documentation link to the summary docu
#' @param x An object of class \code{"summary.clv.time"}, usually, a result of a call to \code{summary.clv.time}.
#' @param digits The number of significant digits to use when printing.
#' @param ... Ignored.
#' @description
#'
#' Summary method for CLV time objects that provides information about the start and end date of
#' the estimation and possible holdout sample.
#'
#' @return This function computes and returns summary statistics of the
#' time related information stored in \code{object}. It returns a list of
#' class \code{summary.clv.time} that contains the elements:
#' \item{time.unit}{What time unit defines a single period.}
#' \item{estimation.period.in.tu}{Length of estimation period in time units.}
#' \item{has.holdout}{Whether the object has a holdout data sample.}
#' \item{holdout.period.in.tu}{Length of holdout period in time units, if any.}
#'
#' @seealso \code{\link[CLVTools:summary.clv.data]{summary.clv.data}} for how to summarize a clv data object
#'
