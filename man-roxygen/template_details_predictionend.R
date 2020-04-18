#' @details \code{prediction.end} indicates until when to predict or plot and can be given as either
#' a point in time (of class \code{Date}, \code{POSIXct}, or \code{character}) or the number of periods.
#' If \code{prediction.end} is of class character, the date/time format set when creating the data object is used for parsing.
#' If \code{prediction.end} is the number of periods, the end of the fitting period serves as the reference point
#' from which periods are counted. Only full periods may be specified.
#   Due to limitations in package lubridate and the Date class which can represent only full days, only whole periods
#   may be specified (because what is 0.2345 weeks even?).
#' If \code{prediction.end} is omitted or NULL, it defaults to the end of the holdout period if present and to the
#' end of the estimation period otherwise.
#'
#' The first prediction period is defined to start right after the end of the estimation period.
#' If for example weekly time units are used and the estimation period ends on Sunday 2019-01-01, then the first day
#' of the first prediction period is Monday 2019-01-02. Each prediction period includes a total of 7 days and
#' the first prediction period therefore will end on, and include, Sunday 2019-01-08. Subsequent prediction periods
#' again start on Mondays and end on Sundays.
#' If \code{prediction.end} indicates a timepoint on which to end, this timepoint is included in the prediction period.
#'
