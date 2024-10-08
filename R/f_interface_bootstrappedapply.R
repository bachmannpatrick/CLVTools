#'
#' @title Bootstrapping: Fit a model again on sampled data and apply method
#'
#' @description
#' Given a fitted model, sample new data from the \code{clv.data} stored in it and re-fit the model on it.
#' Which customers are selected into the new data is determined by \code{fn.sample}.
#' The model is fit on the new data with the same options with which it was originally fit,
#' including \code{optimx.args}, \code{verbose} and start parameters. If required,
#' any option can be changed by passing it as \code{...}.
#' After the model is fit, \code{fn.boot.apply} is applied to it and
#' the value it returns is collected in a list which is eventually returned.
#'
#' The estimation and holdout periods are preserved exactly as in the original data.
#' This is regardless of how the actually sampled transactions would define these periods.
#' This way, each customer's model summary data (\code{cbs}) generated from the
#' sampled data remains the same as on the original data.
#' This makes sampling from the \code{clv.data} object equivalent to sampling
#' directly from the model summary data.
#'
#' Note that the Id of customers which are sampled more than once gains a suffix "_BOOTSTRAP_ID_<number>".
#'
#'
#' @param object Fitted model
#' @param num.boots number of times to sample data and re-fit the model
#' @param fn.sample Method sampling customer ids for creating the bootstrapped data. Receives and returns
#'  a vector of ids (string). If \code{NULL}, ids are sampled with replacement until reaching original length. See examples.
#' @param fn.boot.apply Method to apply on each model estimated on the sampled data. See examples.
#' @param ... Passed to the model estimation method. See examples.
#'
#' @returns
#' Returns a list containing the results of \code{fn.boot.apply}
#'
#' @seealso For possible inputs to \code{...} see \link{pnbd}, \link{ggomnbd}, \link{bgnbd}.
#' @seealso Internal methods \code{clv.data.create.bootstrapping.data} to create a \code{clv.data}
#' object of given customer ids and \code{clv.fitted.estimate.same.specification.on.new.data} to
#' estimate a model again on new data with its original specification.
#'
#' @examples
#' \donttest{
#' data("cdnow")
#'
#' clv.cdnow <- clvdata(data.transactions = cdnow, date.format="ymd",
#'                      time.unit = "weeks", estimation.split=37)
#'
#' pnbd.cdnow <- pnbd(clv.cdnow)
#'
#' # bootstrapped model coefs while sampling 50 percent
#' # of customers without replacement
#' clv.bootstrapped.apply(pnbd.cdnow, num.boots=5, fn.boot.apply=coef,
#' fn.sample=function(x){
#' sample(x, size = as.integer(0.5*length(x)), replace = FALSE)})
#'
#' # sample customers with built-in standard logic and
#' # return predictions until end of holdout period in original
#' # data.
#' # prediction.end is not required because the bootstrapped
#' # data contains the same estimation and holdout periods
#' # as the original data, even if the transactions of the sampled
#' # customers .
#' clv.bootstrapped.apply(pnbd.cdnow, num.boots=5, fn.sample=NULL,
#' fn.boot.apply=function(x){predict(x)})
#'
#' # return the fitted models
#' # forward additional arguments to the model fitting method
#' clv.bootstrapped.apply(pnbd.cdnow, num.boots=5, fn.sample=NULL,
#' fn.boot.apply=return,
#' # args for ..., forwarded to pnbd()
#' verbose=FALSE, optimx.args=list(method="Nelder-Mead"),
#' start.params.model=coef(pnbd.cdnow))
#' }
#'
#' @export
clv.bootstrapped.apply <- function(object, num.boots, fn.boot.apply, fn.sample=NULL, ...){
  # cran silence
  Id <- NULL

  check_err_msg(check_user_data_numboots(num.boots=num.boots))
  check_err_msg(check_user_data_fnbootapply(fn.boot.apply=fn.boot.apply))
  check_err_msg(check_user_data_fnsample(fn.sample=fn.sample))

  if(is.null(fn.sample)){
    fn.sample <- function(x){
      return(sample(x, size = length(x), replace = TRUE))
    }
  }

  ids <- object@cbs[, unique(Id)]

  l.boots <- lapply(seq_len(num.boots), function(i){

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
