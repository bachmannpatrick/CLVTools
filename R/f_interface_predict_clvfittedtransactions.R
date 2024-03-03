
# S3 predict for clv.fitted.transactions ----------------------------------------------------------------------------------

#' @title Predict CLV from a fitted transaction model
#' @param object A fitted clv transaction model for which prediction is desired.
#' @param newdata A clv data object or data for the new customer prediction (see \link{newcustomer}) for which predictions should
#' be made with the fitted model. If none or NULL is given, predictions are made for the data on which the model was fit.
#' @param predict.spending Whether and how to predict spending and based on it also CLV, if possible. See details.
#' @param continuous.discount.factor continuous discount factor to use to calculate \code{DERT/DECT}. Defaults to a 10\% continuous annual rate. See details.
#' @template template_params_uncertainty
#' @templateVar prefix {}
#' @templateVar plot_or_predict predict
#' @template template_param_predictionend
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#'
#' Probabilistic customer attrition models predict in general three expected characteristics for every customer:
#' \itemize{
#' \item "conditional expected transactions" (\code{CET}), which is the number of transactions to expect from a customer
#' during the prediction period,
#' \item "probability of a customer being alive" (\code{PAlive}) at the end of the estimation period and
#' \item "discounted expected residual transactions" (\code{DERT}) for every customer, which is the total number of
#' transactions for the residual lifetime of a customer discounted to the end of the estimation period.
#' In the case of time-varying covariates, instead of \code{DERT}, "discounted expected conditional transactions" (\code{DECT})
#' is predicted. \code{DECT} does only cover a finite time horizon in contrast to \code{DERT}.
#' For \code{continuous.discount.factor=0}, \code{DECT} corresponds to \code{CET}.
#'}
#'
#' In order to derive a monetary value such as CLV, customer spending has to be considered.
#' If the \code{clv.data} object contains spending information, customer spending can be predicted using a Gamma/Gamma spending model for
#' parameter \code{predict.spending} and the predicted CLV is be calculated (if the transaction model supports \code{DERT/DECT}).
#' In this case, the prediction additionally contains the following two columns:
#' \itemize{
#' \item "predicted.mean.spending", the mean spending per transactions as predicted by the spending model.
#' \item "CLV", the customer lifetime value. CLV is the product of DERT/DECT and predicted spending.
#'}
#'
#' Uncertainty estimates are available for all predicted quantities using bootstrapping.
#'
#' \subsection{New customer prediction}{
#' The fitted model can also be used to predict the number of transactions a single, average
#' newly alive customer is expected to make at the moment of the first transaction ("coming alive").
#' For covariate models, the prediction is for an average customer with the given covariates.
#'
#' The individual-level unconditional expectation that is also used for the
#' \link[CLVTools:plot.clv.fitted.transactions]{tracking plot} is used to obtain this prediction.
#' For models without covariates, the prediction hence is the same for all customers
#' and independent of when a customer comes alive.
#' For models with covariates, the prediction is the same for all customers
#' with the same covariates.
#'
#' The data on which the model was fit and which is stored in it is NOT used for this prediction.
#' See examples and \link{newcustomer} for more details.
#' }
#'
#'
#' @details \code{predict.spending} indicates whether to predict customers' spending and if so, the spending model to use.
#' Accepted inputs are either a logical (\code{TRUE/FALSE}), a method to fit a spending model (i.e. \code{\link{gg}}), or
#' an already fitted spending model. If provided \code{TRUE}, a Gamma-Gamma model is fit with default options. If argument
#' \code{newdata} is provided, the spending model is fit on \code{newdata}. Predicting spending is only possible if
#' the transaction data contains spending information. See examples for illustrations of valid inputs.
#'
#' @template template_details_newdata
#'
#' @details
#' To predict for new customers, the output of \link{newcustomer} has to be given to \code{newdata}. See examples.
#'
#' @template template_details_predictionend
#'
#'
#' @details \code{continuous.discount.factor} is the continuous rate used to discount the expected residual
#' transactions (\code{DERT/DECT}). An annual rate of (100 x d)\% equals a continuous rate delta = ln(1+d).
#' To account for time units which are not annual, the continuous rate has to be further adjusted
#' to delta=ln(1+d)/k, where k are the number of time units in a year.
#'
#' @section Uncertainty Estimates:
#' Bootstrapping is used to provide confidence intervals of all predicted metrics.
#' These provide an estimate of parameter uncertainty.
#' To create bootstrapped data, customer ids are sampled with replacement until reaching original
#' length and all transactions of the sampled customers are used to create a new \code{clv.data} object.
#' A new model is fit on the bootstrapped data which is then used to predict on this data.
#' All prediction parameters, incl \code{prediction.end} and \code{continuous.discount.factor}, are forwarded
#' to the prediction on the bootstrapped data.
#' Per customer, confidence intervals of each predicted metric are created using a "reversed quantile" approach.
#' See \link{clv.bootstrapped.apply} to create a custom bootstrapping procedure.
#'
#'
#' @seealso models to predict transactions: \link{pnbd}, \link{bgnbd}, \link{ggomnbd}.
#' @seealso models to predict spending: \link{gg}.
#' @seealso \code{\link[CLVTools:predict.clv.fitted.spending]{predict}} for spending models
#' @seealso \link{clv.bootstrapped.apply} for bootstrapped model estimation
#' @seealso \code{\link{newcustomer}} to create data to predict for newly alive customers.
#'
#'
#' @return
#' An object of class \code{data.table} with columns:
#' \item{Id}{The respective customer identifier}
#' \item{period.first}{First timepoint of prediction period}
#' \item{period.last}{Last timepoint of prediction period}
#' \item{period.length}{Number of time units covered by the period indicated by \code{period.first} and \code{period.last} (including both ends).}
#' \item{PAlive}{Probability to be alive at the end of the estimation period}
#' \item{CET}{The Conditional Expected Transactions: The number of transactions expected until prediction.end.}
#' \item{DERT or DECT}{Discounted Expected Residual Transactions or Discounted Expected Conditional Transactions for dynamic covariates models}
#' \item{actual.x}{Actual number of transactions until prediction.end. Only if there is a holdout period and the prediction ends in it, otherwise not reported.}
#' \item{actual.total.spending}{Actual total spending until prediction.end. Only if there is a holdout period and the prediction ends in it, otherwise not reported.}
#' \item{predicted.mean.spending}{The mean spending per transactions as predicted by the spending model.}
#' \item{predicted.total.spending}{The predicted total spending until prediction.end (\code{CET*predicted.mean.spending}).}
#' \item{predicted.CLV}{Customer Lifetime Value based on \code{DERT/DECT} and \code{predicted.mean.spending}.}
#'
#' If predicting for new customers (using \code{newcustomer()}), a numeric scalar indicating the expected
#' number of transactions is returned instead.
#'
#' @examples
#'
#' \donttest{
#'
#' data("apparelTrans")
#' # Fit pnbd standard model on data, WITH holdout
#' apparel.holdout <- clvdata(apparelTrans, time.unit="w",
#'                            estimation.split=52, date.format="ymd")
#' apparel.pnbd <- pnbd(apparel.holdout)
#'
#' # Predict until the end of the holdout period
#' predict(apparel.pnbd)
#'
#' # Predict until 10 periods (weeks in this case) after
#' #   the end of the 37 weeks fitting period
#' predict(apparel.pnbd, prediction.end = 10) # ends on 2010-11-28
#'
#' # Predict until 31th Dec 2016 with the timepoint as a character
#' predict(apparel.pnbd, prediction.end = "2016-12-31")
#'
#' # Predict until 31th Dec 2016 with the timepoint as a Date
#' predict(apparel.pnbd, prediction.end = lubridate::ymd("2016-12-31"))
#'
#'
#' # Predict future transactions but not spending and CLV
#' predict(apparel.pnbd, predict.spending = FALSE)
#'
#' # Predict spending by fitting a Gamma-Gamma model
#' predict(apparel.pnbd, predict.spending = gg)
#'
#' # Fit a spending model separately and use it to predict spending
#' apparel.gg <- gg(apparel.holdout, remove.first.transaction = FALSE)
#' predict(apparel.pnbd, predict.spending = apparel.gg)
#'
#'
#' # Fit pnbd standard model WITHOUT holdout
#' pnc <- pnbd(clvdata(apparelTrans, time.unit="w", date.format="ymd"))
#'
#' # This fails, because without holdout, a prediction.end is required
#' \dontrun{
#' predict(pnc)
#' }
#'
#' # But it works if providing a prediction.end
#' predict(pnc, prediction.end = 10) # ends on 2016-12-17
#'
#'
#' # Predict num transactions for a newly alive customer
#' # in the next 3.45 weeks
#' # See ?newcustomer() for more examples
#' predict(apparel.pnbd, newdata = newcustomer(num.periods=3.45))
#'
#' }
#'
#'
#'
#' @importFrom stats predict
#' @method predict clv.fitted.transactions
#' @aliases predict
#' @export
predict.clv.fitted.transactions <- function(object, newdata=NULL, prediction.end=NULL, predict.spending=gg,
                                            continuous.discount.factor=log(1+0.1), uncertainty=c("none", "boots"), level=0.9, num.boots=100, verbose=TRUE, ...){

  check_err_msg(check_user_data_emptyellipsis(...))
  check_err_msg(.check_userinput_matcharg(char=tolower(uncertainty), choices=c("none", "boots"), var.name="uncertainty"))
  # match uncertainty to one of the allowed values
  uncertainty <- match.arg(tolower(uncertainty), choices = c("none", "boots"), several.ok = FALSE)

  # The usual prediction unless newdata indicates a new customer prediction (ie newdata=newcustomer())
  if(is(newdata, "clv.newcustomer.no.cov")){
    # not other parameters except object and newdata may be given (all others must be missing)
    if(!all(missing(prediction.end), missing(predict.spending), missing(continuous.discount.factor))){
      check_err_msg("Parameters prediction.end, predict.spending and continuous.discount.factor may not be specified when predicting for new customers.")
    }


    return(clv.predict.new.customer(clv.fitted = object, clv.newcustomer = newdata))
  }

  # If it was not explicitly passed in the call, the spending model should only be applied
  #   it there is spending data. Otherwise, predict does not work out-of-the-box for
  #   data object w/o spending
  if(missing(predict.spending)){
    # Only need to disable if has no spending, default argument is a spending model
    #   (ie predict.spending = gg already)
    if(!clv.data.has.spending(object@clv.data)){
      predict.spending <- FALSE
    }
  }

  return(clv.template.controlflow.predict(clv.fitted=object, prediction.end=prediction.end, predict.spending=predict.spending,
                                   continuous.discount.factor=continuous.discount.factor, verbose=verbose, user.newdata=newdata,
                                   uncertainty=uncertainty, num.boots=num.boots, level=level))
}


# . S4 definition ----------------------------------------------------------------------------------------
# S4 method to forward to S3 method
#' @include all_generics.R class_clv_fitted_transactions.R
#' @exportMethod predict
#' @rdname predict.clv.fitted.transactions
setMethod(f = "predict", signature = signature(object="clv.fitted.transactions"), predict.clv.fitted.transactions)
