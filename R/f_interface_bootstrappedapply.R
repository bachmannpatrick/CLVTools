#'
#' @title Bootstrapping: Fit a model again on sampled data and apply method
#'
#' @description
#' Given a fitted model, sample new data from the \code{clv.data} stored in it and re-fit the model on it.
#' Which customers are selected into the new data is determined by \code{fn.sample}.
#' The model is fit on the new data with the same options with which it was originally fit (excluding \code{optimx.args} and \code{verbose}).
#' After it is fit, \code{fn.boot.apply} is applied to it and
#' the value it returns is collected in a list which is eventually returned.
#'
#' @param object Fitted model
#' @param num.boot number of times to sample data and re-fit the model
#' @param fn.sample Method sampling customer Ids for creating the bootstrapped data. Receives and returns
#'  a vector of ids (string). If \code{NULL}, 80 percent of customers are sampled without replacement. See examples.
#' @param fn.boot.apply Method to apply on each model estimated on the sampled data. See examples.
#' @param ... Passed to the model estimation method. See examples.
#'
#' @returns
#' Returns a list containing the results of \code{fn.boot.apply}
#'
#' @seealso Models for possible inputs to \code{...}: \link{pnbd}, \link{ggomnbd}, \link{bgnbd}.
#'
#' @examples
#' \donttest{
#' data("cdnow")
#'
#' clv.cdnow <- clvdata(data.transactions = cdnow, date.format="ymd",
#'                      time.unit = "weeks")
#'
#' pnbd.cdnow <- pnbd(clv.cdnow)
#'
#' # bootstrapped model coefs while sampling 50 percent
#' # of customers without replacement
#' bootstrapped.apply(pnbd.cdnow, num.boot=5, fn.boot.apply=coef,
#' fn.sample=function(x){
#' sample(x, size = as.integer(0.5*length(x)), replace = FALSE)})
#'
#' # sample customers with built-in standard logic and
#' # return predictions 10 periods ahead.
#' # prediction.end is required because the bootstrapped
#' # data contains no holdout period
#' bootstrapped.apply(pnbd.cdnow, num.boot=5, fn.sample=NULL,
#' fn.boot.apply=function(x){predict(x, prediction.end=10)})
#'
#' # return the fitted models
#' # forward additional arguments to the model fitting method
#' bootstrapped.apply(pnbd.cdnow, num.boot=5, fn.sample=NULL,
#' fn.boot.apply=return,
#' # args for ..., forwarded to pnbd()
#' verbose=FALSE, optimx.args=list(method="Nelder-Mead"),
#' start.params.model=coef(pnbd.cdnow))
#' }
#'
#' @export
bootstrapped.apply <- function(object, num.boot, fn.boot.apply, fn.sample=NULL, ...){

  # TODO [test]: Test that works for transaction and spending model
  # TODO [test]: Test that works for static and dynamic covariate models

  if(is.null(fn.sample)){
    fn.sample <- function(x){
      return(sample(x, size = as.integer(0.8*length(x)), replace = FALSE))
    }
  }

  ids <- object@cbs[, unique(Id)]

  l.boots <- lapply(seq(num.boot), function(i){

    clv.data.boot <- clv.data.create.bootstrapping.data(
      clv.data = object@clv.data,
      ids=fn.sample(ids))

    clv.fitted.boot <- clv.fitted.estimate.same.specification.on.new.data(
      clv.fitted = object,
      newdata = clv.data.boot,
      ...)

    return(fn.boot.apply(clv.fitted.boot))
  })

  return(l.boots)
}
