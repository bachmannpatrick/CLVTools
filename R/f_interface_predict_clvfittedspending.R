# S3 predict for clv.fitted.spending ------------------------------------------------------------------------------
#' @title Predict customers' future spending
#'
#' @param object A fitted spending model for which prediction is desired.
#' @param newdata A clv data object for which predictions should be made with the fitted model. If none or NULL is given, predictions are made for the data on which the model was fit.
#'
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#' Predict customer's future mean spending per transaction and compare it to the actual mean spending in the holdout period.
#'
#' @details
#' If \code{newdata} is provided, the individual customer statistics underlying the model are calculated
#' the same way as when the model was fit initially. Hence, if \code{remove.first.transaction} was \code{TRUE},
#' this will be applied to \code{newdata} as well.
#'
#' @seealso models to predict spending: \link{gg}.
#' @seealso models to predict transactions: \link{pnbd}, \link{bgnbd}, \link{ggomnbd}.
#' @seealso \code{\link[CLVTools:predict.clv.fitted.transactions]{predict}} for transaction models
#'
#'
#' @return An object of class \code{data.table} with columns:
#' \item{Id}{The respective customer identifier}
#' \item{actual.mean.spending}{Actual mean spending per transaction in the holdout period. Only if there is a holdout period otherwise it is not reported.}
#' \item{predicted.mean.spending}{The mean spending per transaction as predicted by the fitted spending model.}
#'
#'
#' @examples
#' \donttest{
#' data("apparelTrans")
#'
#' # Fit gg model on data
#' apparel.holdout <- clvdata(apparelTrans, time.unit="w",
#'                            estimation.split=37, date.format="ymd")
#' apparel.gg <- gg(apparel.holdout)
#'
#' # Predict customers' future mean spending per transaction
#' predict(apparel.gg)
#'
#' }
#'
#' @importFrom stats predict
#' @method predict clv.fitted.spending
#' @export
predict.clv.fitted.spending <- function(object, newdata=NULL, verbose=TRUE, ...){

  check_err_msg(check_user_data_emptyellipsis(...))

  return(clv.template.controlflow.predict(clv.fitted=object, verbose=verbose, user.newdata=newdata))
}



# . S4 definition ----------------------------------------------------------------------------------------
# S4 method to forward to S3 method
#' @include all_generics.R class_clv_fitted_spending.R
#' @exportMethod predict
#' @rdname predict.clv.fitted.spending
setMethod(f = "predict", signature = signature(object="clv.fitted.spending"), predict.clv.fitted.spending)
