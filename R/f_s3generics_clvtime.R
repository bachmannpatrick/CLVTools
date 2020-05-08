#' @include class_clv_time.R
#' @importFrom methods show
#' @export
#' @rdname clv.time-class
setMethod(f = "show", signature = signature(object="clv.time"), definition = function(object){
  print(x=object)})

#' @rdname summary.clv.time
#' @include class_clv_time.R
#' @keywords internal
#' @export
print.clv.time <- function(x, digits=max(3L, getOption("digits")-3L), ...){

  nsmall <- 4 # dont leave to user, hardcode

  has.holdout <- clv.time.has.holdout(clv.time=x)

  .print.list(list("Time unit"         = x@name.time.unit,
                   "   " ="",
                   "Estimation start"  = clv.time.format.timepoint(clv.time=x, timepoint=x@timepoint.estimation.start),
                   "Estimation end"    = clv.time.format.timepoint(clv.time=x, timepoint=x@timepoint.estimation.end),
                   "Estimation length" = paste0(format(x@estimation.period.in.tu, digits=digits,nsmall=nsmall), " ", x@name.time.unit),
                   "  " ="",
                   "Holdout start"     = ifelse(has.holdout, clv.time.format.timepoint(clv.time=x, timepoint=x@timepoint.holdout.start), "-"),
                   "Holdout end"       = ifelse(has.holdout, clv.time.format.timepoint(clv.time=x, timepoint=x@timepoint.holdout.end), "-"),
                   "Holdout length"    = ifelse(has.holdout, paste0(format(x@holdout.period.in.tu, nsmall=nsmall), " ", x@name.time.unit), "-")),
              nsmall=nsmall)

  cat("\n")

  invisible(x)
}


#' @template template_summary_clvtime
#' @include class_clv_time.R
#' @export
summary.clv.time <- function(object, ...){
  res <- structure(list(), class="summary.clv.time")

  res$name.time.unit          <- object@name.time.unit
  res$estimation.period.in.tu <- object@estimation.period.in.tu
  res$has.holdout             <- clv.time.has.holdout(clv.time=object)
  if(res$has.holdout)
    res$holdout.period.in.tu <- object@holdout.period.in.tu

  return(res)
}

#' @keywords internal
#' @export
print.summary.clv.time <-function(x, digits=max(3L, getOption("digits")-3L), ...){
  nsmall <- 4

  .print.list(list("Time unit"         = x$name.time.unit,
                   "Estimation length" = paste0(format(x$estimation.period.in.tu, digits=digits,nsmall=nsmall), " ", x$name.time.unit),
                   "Holdout length"    = ifelse(x$has.holdout, paste0(format(x$holdout.period.in.tu, nsmall=nsmall), " ", x$name.time.unit), "-"))
              , nsmall=nsmall)

  return(invisible(x))
}
