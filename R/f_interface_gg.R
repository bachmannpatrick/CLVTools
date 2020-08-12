#' @name gg
#'
#' @title Gamma/Gamma Spending model
#'
#' @template template_params_estimate
#' @template template_param_verbose
#' @template template_param_dots
#' @param remove.first.transaction Whether customer's first transaction are removed. If \code{TRUE} all zero-repeaters are excluded from model fitting.
#'
#' @description
#' Fits the Gamma-Gamma model on a given object of class \code{clv.data} to predict customers' mean
#' spending per transaction.
#'
#'
#' @details Model parameters for the G/G model are \code{p, q, and gamma}. \cr
#' \code{p}: shape parameter of the Gamma distribution of the spending process. \cr
#' \code{q}: shape parameter of the Gamma distribution to account for customer heterogeneity. \cr
#' \code{gamma}: scale parameter of the Gamma distribution to account for customer heterogeneity.\cr
#' If no start parameters are given, 1.0 is used for all model parameters. All parameters are required
#' to be > 0.
#'
#' The Gamma-Gamma model cannot be estimated for data that contains negative prices.
#' Customers with a mean spending of zero or a transaction count of zero are ignored during model fitting.
#'
#' \subsection{The G/G model}{
#' The G/G model allows to predict a value for future customer transactions. Usually, the G/G model is used
#' in combination with a probabilistic model predicting customer transaction such as the Pareto/NBD or the BG/NBD model.
#' }
#'
#' @return
#' An object of class \linkS4class{clv.gg} is returned.
#'
#' @template template_clvfitted_returnvalue
#'
#' @seealso \code{\link[CLVTools:clvdata]{clvdata}} to create a clv data object.
#'
#' @seealso \code{\link[CLVTools:predict.clv.fitted.spending]{predict}} to predict expected mean spending for every customer.
#' @seealso \code{\link[CLVTools:plot.clv.fitted.spending]{plot}} to plot the density of customer's mean transaction value compared to the model's prediction.
#'
#' @template template_references_gg
#'
#' @templateVar name_model_short gg
#' @templateVar vec_startparams_model c(p=0.5, q=15, gamma=2)
#' @template template_examples_spendingmodelinterface
#'
NULL



#' @exportMethod gg
setGeneric("gg", def = function(clv.data, start.params.model=c(), optimx.args=list(), remove.first.transaction = TRUE, verbose=TRUE, ...)
  standardGeneric("gg"))



#' @include class_clv_data.R
#' @rdname gg
setMethod("gg", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                  start.params.model=c(),
                                                                                  optimx.args=list(),
                                                                                  remove.first.transaction = TRUE,
                                                                                  verbose=TRUE,
                                                                                  ...){

  err.msg <- c()
  err.msg <- c(err.msg, check_user_data_emptyellipsis(...))

  # Check here already because inputs are already needed to build the cbs (remove.first.transaction and data)
  err.msg <- c(err.msg, .check_user_data_single_boolean(remove.first.transaction, var.name = "remove.first.transaction"))
  err.msg <- c(err.msg, check_user_data_containsspendingdata(clv.data = clv.data))
  check_err_msg(err.msg)

  if(clv.data.has.negative.spending(clv.data)){
    check_err_msg("The Gamma-Gamma spending model cannot be fit on data that contains negative prices!")
  }

  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)
  obj <- clv.gg(cl=cl, clv.data=clv.data, remove.first.transaction = remove.first.transaction)

  return(clv.template.controlflow.estimate(clv.fitted=obj, start.params.model = start.params.model,
                                           optimx.args = optimx.args, verbose = verbose,
                                           remove.first.transaction = remove.first.transaction))
})
