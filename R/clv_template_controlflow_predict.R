clv.template.controlflow.predict <- function(clv.fitted, verbose, user.newdata, ...){

  # ***TODO: For development purposes only: All additional args have to be named!
  l.elipsis <- list(...)
  if(length(l.elipsis)>0){
    names.ellipsis.args <- names(list(...))
    stopifnot(!is.null(names.ellipsis.args))
    stopifnot(all(nchar(names(list(...)))>0))
  }

  # Process Newdata -------------------------------------------------------------------------------------
  # Because many of the following steps refer to the data stored in the fitted model,
  #   it first is replaced with newdata before any other steps are done
  if(!is.null(user.newdata)){
    # check newdata
    clv.controlflow.check.newdata(clv.fitted = clv.fitted, user.newdata = user.newdata, ...)

    # Replace data in model with newdata
    #   Deep copy to not change user input
    clv.fitted@clv.data <- copy(user.newdata)

    # Do model dependent steps of adding newdata
    clv.fitted <- clv.model.process.newdata(clv.model = clv.fitted@clv.model, clv.fitted=clv.fitted, verbose=verbose)
  }


  # Input checks ----------------------------------------------------------------------------------------
  #   Only after newdata replaced clv.data stored in clv.fitted because inputchecks use clv.fitted@clv.data
  clv.controlflow.predict.check.inputs(clv.fitted=clv.fitted, verbose=verbose, ...)

  # Prediction result table -----------------------------------------------------------------------------
  dt.predictions <- clv.controlflow.predict.build.result.table(clv.fitted=clv.fitted, verbose=verbose, ...)

  # Model prediction ------------------------------------------------------------------------------------
  dt.predictions <- clv.model.predict(clv.model = clv.fitted@clv.model, clv.fitted = clv.fitted,
                                      dt.predictions = dt.predictions, verbose = verbose, ...)
  setkeyv(dt.predictions, "Id")

  # Actuals ---------------------------------------------------------------------------------------------¨
  has.actuals    <- clv.controlflow.predict.get.has.actuals(clv.fitted, dt.predictions = dt.predictions)
  dt.predictions <- clv.controlflow.predict.add.actuals(clv.fitted = clv.fitted, dt.predictions = dt.predictions,
                                                        has.actuals = has.actuals, verbose = verbose, ...)

  # post.process / add any additional steps -------------------------------------------------------------
  # set col order etc
  dt.predictions <- clv.controlflow.predict.post.process.prediction.table(clv.fitted = clv.fitted,
                                                                          has.actuals = has.actuals,
                                                                          dt.predictions = dt.predictions,
                                                                          verbose = verbose, ...)


  # data.table does not print when returned because it is returned directly after last [:=]
  # " if a := is used inside a function with no DT[] before the end of the function, then the next
  #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
  #   will print. To avoid this: include a DT[] after the last := in your function."
  dt.predictions[]


  return(dt.predictions)
}




# S3 predict for clv.fitted ----------------------------------------------------------------------------------


#' @title Predict CLV from a fitted model
#' @param object A fitted clv model for which prediction is desired.
#' @param newdata A clv data object for which predictions should be made with the fitted model. If none or NULL is given, predictions are made for the data on which the model was fit.
#' @param predict.spending Whether the spending and CLV should be calculated and reported additionally. Only possible if the transaction data contains spending information.
#' @param continuous.discount.factor continuous discount factor to use
#' @template template_param_predictionend
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#' Probabilistic customer attrition models predict in general three expected characteristics for every customer:
#' \itemize{
#' \item "conditional expected transactions" (\code{CET}), which is the number of transactions to expect form a customer
#' during the prediction period,
#' \item "probability of a customer being alive" (\code{PAlive}) at the end of the estimation period and
#' \item "discounted expected residual transactions" (\code{DERT}) for every customer, which is the total number of
#' transactions for the residual lifetime of a customer discounted to the end of the estimation period.
#' In the case of time-varying covariates, instead of \code{DERT}, "discounted expected conditional transactions" (\code{DECT})
#' is predicted. \code{DECT} does only cover a finite time horizon in contrast to \code{DERT}. For \code{continuous.discount.factor=0}, \code{DECT} corresponds to \code{CET}.
#'}
#' If spending information was provided in the \code{clvdata} object, by default a Gamma/Gamma model is fitted
#' to predict spending and calculate the predicted CLV. In this case, the prediction additionally contains the following two columns:
#' \itemize{
#' \item predicted spending estimated by a Gamma/Gamma model
#' \item the customer lifetime value (CLV).
#'}
#'
#'
#' @template template_details_newdata
#'
#' @template template_details_predictionend
#'
#' @details \code{predict.spending} uses a Gamma/Gamma model to predict customer spending. This option is only available
#' if customer spending information was provided when the data object was created.
#'
#' @details \code{continuous.discount.factor} allows to adjust the discount rate used to estimated the discounted expected
#' transactions (\code{DERT}).
#' The default value is \code{0.1} (=10\%). Note that a continuous rate needs to be provided.
#'
#'
#' \subsection{The Gamma-Gamma model to Predict Spending}{
#' Most probabilistic latent customer attrition model capture future customer
#' behavior as a combination of the customer's purchase and attrition process.
#' However, in order to derive a monetary value such as CLV, customer spending
#' also has to be considered. To model customer spending the Gamma/Gamma is a
#' popular choice.
#' }
#'
#' @references
#' Schmittlein DC, Morrison DG, Colombo R (1987). \dQuote{Counting Your Customers:
#' Who-Are They and What Will They Do Next?} Management Science, 33(1), 1–24.
#'
#' Fader PS, Hardie BGS (2005). \dQuote{A Note on Deriving the Pareto/NBD Model and
#' Related Expressions.}
#' URL \url{http://www.brucehardie.com/notes/009/pareto_nbd_derivations_2005-11-05.pdf}.
#'
#' Fader PS, Hardie BG (2007). "Incorporating time-invariant covariates into the
#' Pareto/NBD and BG/NBD models."
#' URL \url{http://www.brucehardie.com/notes/019/time_invariant_covariates.pdf}.
#'
#' Colombo R, Jiang W (1999). "A stochastic RFM model."
#' Journal of Interactive Marketing, 13(3), 2–12.
#'
#' Fader PS, Hardie BG, Lee K (2005). "RFM and CLV: Using Iso-Value Curves for Customer Base Analysis."
#' Journal of Marketing Research, 42(4), 415–430.
#'
#' Fader PS, Hardie BG (2013). "The Gamma-Gamma Model of Monetary Value."
#' URL \url{http://www.brucehardie.com/notes/025/gamma_gamma.pdf}.
#'
#'
#' @return
#' An object of class \code{data.table} with each columns containing the predictions:
#' \item{Id}{The respective customer identifier}
#' \item{period.first}{First timepoint of prediction period}
#' \item{period.last}{Last timepoint of prediction period}
#' \item{period.length}{Number of time units covered by the period indicated by \code{period.first} and \code{period.last} (including both ends).}
#' \item{PAlive}{Probability to be alive at the end of the estimation period}
#' \item{CET}{The Conditional Expected Transactions}
#' \item{DERT or DECT}{Discounted Expected Residual Transactions or Discounted Expected Conditional Transactions for dynamic covariates models}
#' \item{actual.x}{Actual number of transactions until prediction.end. Only if there is a holdout period and the prediction ends in it.}
#' \item{actual.Spending}{Actual spending until prediction.end. Only if there is a holdout period and the prediction ends in it, 0 otherwise.}
#' \item{predicted.Spending}{The spending as predicted by the Gamma-Gamma model.}
#' \item{predicted.CLV}{Customer Lifetime Value based on DERT and predicted spending.}
#'
#' @examples
#'
#' \donttest{
#'
#' data("apparelTrans")
#' # Fit pnbd standard model on data, WITH holdout
#' pnc <- pnbd(clvdata(apparelTrans, time.unit="w",
#'                     estimation.split=37, date.format="ymd"))
#'
#' # Predict until the end of the holdout period
#' predict(pnc)
#'
#' # Predict until 10 periods (weeks in this case) after
#' #   the end of the 37 weeks fitting period
#' predict(pnc, prediction.end = 10) # ends on 2010-11-28
#'
#' # Predict until 31th Dec 2016 with the timepoint as a character
#' predict(pnc, prediction.end = "2016-12-31")
#'
#' # Predict until 31th Dec 2016 with the timepoint as a Date
#' predict(pnc, prediction.end = lubridate::ymd("2016-12-31"))
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
#' # Now, predict 10 periods from the end of the last transaction
#' #   (end of estimation period)
#' predict(pnc, prediction.end = 10) # ends on 2016-12-17
#'
#' }
#'
#' @importFrom stats predict
#' @method predict clv.fitted
#' @export
predict.clv.fitted <- function(object, newdata=NULL, prediction.end=NULL, predict.spending=gg,
                               continuous.discount.factor=0.1, verbose=TRUE, ...){
  check_err_msg(check_user_data_emptyellipsis())

  clv.template.controlflow.predict(clv.fitted=object, prediction.end=prediction.end, predict.spending=predict.spending,
                                   continuous.discount.factor=continuous.discount.factor, verbose=verbose, user.newdata=newdata)
}


# S4 predict definitions --------------------------------------------------------------------------------
# S4 method to forward to S3 method
#' @include all_generics.R class_clv_fitted.R
#' @exportMethod predict
#' @rdname predict.clv.fitted
setMethod(f = "predict", signature = signature(object="clv.fitted"), predict.clv.fitted)
