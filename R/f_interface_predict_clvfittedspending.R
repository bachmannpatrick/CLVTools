# S3 predict for clv.fitted.spending ------------------------------------------------------------------------------
#' @title Infer customers' spending
#'
#' @param object A fitted spending model for which prediction is desired.
#' @param newdata A \code{clv.data} object or data for the new customer prediction (see \link[CLVTools:newcustomer]{newcustomer.spending}).
#' If none or NULL is given, predictions are made for the data on which the model was fit.
#' @template template_params_uncertainty
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#' Infer customer's mean spending per transaction and compare it to the actual mean spending in the holdout period.
#'
#' \subsection{New customer prediction}{
#' The fitted model can also be used to estimate the spending that a single, (fictional), average
#' newly alive customer is expected to make at the moment of the first transaction.
#' This is, for a customer which has no existing order history and that just "came alive".
#'
#' The data on which the model was fit and which is stored in it is NOT used for this prediction.
#' See examples and \link[CLVTools:newcustomer]{newcustomer.spending} for more details.
#' }
#'
#' @details
#' If \code{newdata} is provided, the individual customer statistics underlying the model are calculated
#' the same way as when the model was fit initially. Hence, if \code{remove.first.transaction} was \code{TRUE},
#' this will be applied to \code{newdata} as well.
#'
#' To predict for new customers, the output of \link[CLVTools:newcustomer]{newcustomer.spending} has to be given to \code{newdata}. See examples.
#'
#' @seealso models to predict spending: \link{gg}.
#' @seealso models to predict transactions: \link{pnbd}, \link{bgnbd}, \link{ggomnbd}.
#' @seealso \code{\link[CLVTools:predict.clv.fitted.transactions]{predict}} for transaction models
#' @seealso \code{\link[CLVTools:newcustomer]{newdata.spending}} to create data to predict for customers without order history
#'
#'
#' @return An object of class \code{data.table} with columns:
#' \item{Id}{The respective customer identifier}
#' \item{actual.mean.spending}{Actual mean spending per transaction in the holdout period. Only if there is a holdout period otherwise it is not reported.}
#' \item{predicted.mean.spending}{The mean spending per transaction as predicted by the fitted spending model.}
#'
#' If predicting for new customers (using \code{newcustomer.spending()}), a numeric scalar
#' indicating the expected spending is returned instead.
#'
#' @examples
#' \donttest{
#' data("apparelTrans")
#'
#' # Fit gg model on data
#' apparel.holdout <- clvdata(apparelTrans, time.unit="w",
#'                            estimation.split = 52, date.format = "ymd")
#' apparel.gg <- gg(apparel.holdout)
#'
#' # Estimate customers' mean spending per transaction
#' predict(apparel.gg)
#'
#' # Estimate the mean spending per transaction a single,
#' # fictional, average new customer is expected to make
#' # See ?newcustomer.spending() for more examples
#' predict(apparel.gg, newdata=newcustomer.spending())
#'
#' }
#'
#' @importFrom stats predict
#' @method predict clv.fitted.spending
#' @export
predict.clv.fitted.spending <- function(object, newdata=NULL, uncertainty=c("none", "boots"), level=0.9, num.boots=100, verbose=TRUE, ...){

  check_err_msg(check_user_data_emptyellipsis(...))
  check_err_msg(check_user_data_uncertainty(uncertainty = uncertainty))
  # match uncertainty to one of the allowed values
  uncertainty <- match.arg(tolower(uncertainty), choices=c("none", "boots"), several.ok=FALSE)



  # The usual prediction unless newdata indicates a new customer prediction (ie newdata=newcustomer.spending())
  if(is(newdata, "clv.newcustomer.spending")){

    # TODO: Implement parameter checks
    # not other parameters except object and newdata may be given (all others must be missing)
    # if(!all(missing(prediction.end), missing(predict.spending), missing(continuous.discount.factor))){
    #   check_err_msg("Parameters prediction.end, predict.spending and continuous.discount.factor may not be specified when predicting for new customers.")
    # }
    return(clv.controlflow.predict.new.customer(clv.fitted = object, clv.newcustomer = newdata))
  }


  return(clv.template.controlflow.predict(clv.fitted=object, verbose=verbose, user.newdata=newdata, uncertainty=uncertainty, num.boots=num.boots, level=level))
}



# . S4 definition ----------------------------------------------------------------------------------------
# S4 method to forward to S3 method
#' @include all_generics.R class_clv_fitted_spending.R
#' @exportMethod predict
#' @rdname predict.clv.fitted.spending
setMethod(f = "predict", signature = signature(object="clv.fitted.spending"), predict.clv.fitted.spending)
