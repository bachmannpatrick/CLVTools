#' @include all_generics.R
#' @include class_clv_model_pnbd_nocov.R

setMethod("clv.model.expectation", signature(clv.model="clv.model.pnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  r <- s <- alpha_i <- beta_i <- date.first.repeat.trans<- date.first.actual.trans <- T.cal <- t_i<- period.first.trans<-NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  # all params exactly the same for all customers as there are no covariates
  params_i[, r       := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha_i := clv.fitted@prediction.params.model[["alpha"]]]
  params_i[, s       := clv.fitted@prediction.params.model[["s"]]]
  params_i[, beta_i  := clv.fitted@prediction.params.model[["beta"]]]

  # To caluclate expectation at point t for customers alive in t, given in params_i.t
  fct.expectation <- function(params_i.t) {return( params_i.t[, (r * beta_i)/(alpha_i * (s - 1)) * (1 - (beta_i/(beta_i + t_i))^(s - 1))] )}

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})


#' @title Extract Unconditional Expectation
#' @param object A fitted clv model for which the unconditional expectation is desired.
#' @template template_param_predictionend
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#' Extract the unconditional expectation (future transactions unconditional on beein "alive") from a fitted clv model.
#'
#' @details
#' \code{prediction.end} is either a point in time (of class \code{Date}, \code{POSIXct}, or \code{character}) or the number of periods
#' that indicates until when to calculate the unconditional expectation.
#' If \code{prediction.end} is of class character, the date/time format set when creating the data object is used for parsing.
#' If \code{prediction.end} is the number of periods, the end of the fitting period serves as the reference point from which periods are counted. Only full periods may be specified.
#' If \code{prediction.end} is omitted or NULL, it defaults to the end of the holdout period.
#'
#' @export
fitted.clv.fitted <- function(object, prediction.end=NULL, verbose=FALSE, ...){

  dt.expectation.seq <- clv.time.expectation.periods(clv.time = object@clv.data@clv.time,
                                                     user.tp.end = prediction.end)
  object <- clv.controlflow.predict.set.prediction.params(object)

  dt.model.expectation <- clv.model.expectation(clv.model=object@clv.model, clv.fitted=object,
                                                dt.expectation.seq=dt.expectation.seq, verbose=verbose)

  # data.table does not print when returned because it is returned directly after last [:=]
  # " if a := is used inside a function with no DT[] before the end of the function, then the next
  #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
  #   will print. To avoid this: include a DT[] after the last := in your function."
  dt.model.expectation[]
  return(dt.model.expectation)
}
