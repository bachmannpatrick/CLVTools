#' @details \code{prediction.end} is either a point in time (of class \code{Date}, \code{POSIXct}, or \code{character}) or the number of periods
#' that indicates until when to predict or plot.
#' If \code{prediction.end} is of class character, the date/time format set when creating the data object is used for parsing.
#' If \code{prediction.end} is the number of periods, the end of the fitting period serves as the reference point from which periods are counted.
#' Only full periods may be specified.
#' If \code{prediction.end} is omitted or NULL, it defaults to the end of the holdout period if present and to the
#' end of the estimation period otherwise.
