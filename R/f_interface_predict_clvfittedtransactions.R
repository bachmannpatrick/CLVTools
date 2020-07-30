
# S3 predict for clv.fitted.transactions ----------------------------------------------------------------------------------

#' @title Predict CLV from a fitted transaction model
#' @param object A fitted clv transaction model for which prediction is desired.
#' @param newdata A clv data object for which predictions should be made with the fitted model. If none or NULL is given, predictions are made for the data on which the model was fit.
#' @param predict.spending Whether and how to predict spending and based on it also CLV, if possible. See details.
#' @param continuous.discount.factor continuous discount factor to use to calculate \code{DERT/DECT}
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
#' is predicted. \code{DECT} does only cover a finite time horizon in contrast to \code{DERT}. For \code{continuous.discount.factor=0}, \code{DECT} corresponds to \code{CET}.
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
#' @details \code{predict.spending} indicates whether to predict customers' spending and if so, the spending model to use.
#' Accepted inputs are either a logical (\code{TRUE/FALSE}), a method to fit a spending model (i.e. \code{\link{gg}}), or
#' an already fitted spending model. If provided \code{TRUE}, a Gamma-Gamma model is fit with default options. If argument
#' \code{newdata} is provided, the spending model is fit on \code{newdata}. Predicting spending is only possible if
#' the transaction data contains spending information. See examples for illustrations of valid inputs.
#'
#' @template template_details_newdata
#'
#' @template template_details_predictionend
#'
#' @details \code{continuous.discount.factor} allows to adjust the discount rate used to estimated the discounted expected
#' transactions (\code{DERT/DECT}).
#' The default value is \code{0.1} (=10\%). Note that a continuous rate needs to be provided.
#'
#'
#' @seealso models to predict transactions: \link{pnbd}, \link{bgnbd}, \link{ggomnbd}.
#' @seealso models to predict spending: \link{gg}.
#' @seealso \code{\link[CLVTools:predict.clv.fitted.spending]{predict}} for spending models
#'
#'
#' @return
#' An object of class \code{data.table} with columns:
#' \item{Id}{The respective customer identifier}
#' \item{period.first}{First timepoint of prediction period}
#' \item{period.last}{Last timepoint of prediction period}
#' \item{period.length}{Number of time units covered by the period indicated by \code{period.first} and \code{period.last} (including both ends).}
#' \item{PAlive}{Probability to be alive at the end of the estimation period}
#' \item{CET}{The Conditional Expected Transactions}
#' \item{DERT or DECT}{Discounted Expected Residual Transactions or Discounted Expected Conditional Transactions for dynamic covariates models}
#' \item{actual.x}{Actual number of transactions until prediction.end. Only if there is a holdout period and the prediction ends in it, otherwise it is not reported.}
#' \item{actual.total.spending}{Actual total spending until prediction.end. Only if there is a holdout period and the prediction ends in it, otherwise it is not reported.}
#' \item{predicted.mean.spending}{The mean spending per transactions as predicted by the spending model.}
#' \item{predicted.CLV}{Customer Lifetime Value based on \code{DERT/DECT} and \code{predicted.mean.spending}.}
#'
#' @examples
#'
#' \donttest{
#'
#' data("apparelTrans")
#' # Fit pnbd standard model on data, WITH holdout
#' apparel.holdout <- clvdata(apparelTrans, time.unit="w",
#'                            estimation.split=37, date.format="ymd")
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
#' }
#'
#' @importFrom stats predict
#' @method predict clv.fitted.transactions
#' @aliases predict
#' @export
predict.clv.fitted.transactions <- function(object, newdata=NULL, prediction.end=NULL, predict.spending=gg,
                                            continuous.discount.factor=0.1, verbose=TRUE, ...){

  check_err_msg(check_user_data_emptyellipsis(...))

  # If it was not explicitly passed in the call, the spending model should only be applied
  #   it there is spending data. Otherwise, predict does not work out-of-the-box for
  #   data object w/o spending
  if(missing(predict.spending)){
    # Only need to disable if has no spending, default argument is a spending model
    #   (ie prediction.end = gg already)
    if(!clv.data.has.spending(object@clv.data)){
      predict.spending <- FALSE
    }
  }

  return(clv.template.controlflow.predict(clv.fitted=object, prediction.end=prediction.end, predict.spending=predict.spending,
                                   continuous.discount.factor=continuous.discount.factor, verbose=verbose, user.newdata=newdata))
}


# . S4 definition ----------------------------------------------------------------------------------------
# S4 method to forward to S3 method
#' @include all_generics.R class_clv_fitted_transactions.R
#' @exportMethod predict
#' @rdname predict.clv.fitted.transactions
setMethod(f = "predict", signature = signature(object="clv.fitted.transactions"), predict.clv.fitted.transactions)
