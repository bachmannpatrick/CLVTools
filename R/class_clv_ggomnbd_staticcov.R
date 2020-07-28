#' @templateVar name_model_full GGompertz/NBD
#' @templateVar name_class_clvmodel clv.model.ggomnbd.static.cov
#' @template template_class_clvfittedtransactionmodels_staticcov
#'
#' @template template_slot_ggomnbdcbs
#'
#' @seealso \linkS4class{clv.fitted.transactions.static.cov}, \linkS4class{clv.model.ggomnbd.static.cov}, \linkS4class{clv.ggomnbd}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_ggomnbd_staticcov.R class_clv_data_staticcovariates.R class_clv_fitted_transactions_staticcov.R
setClass(Class = "clv.ggomnbd.static.cov", contains = "clv.fitted.transactions.static.cov",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


#' @importFrom methods new
clv.ggomnbd.static <- function(cl, clv.data){

  dt.cbs.ggomnbd <- ggomnbd_cbs(clv.data = clv.data)
  clv.model <- clv.model.ggomnbd.static.cov()

  # Reuse clv.fitted constructor to ensure proper object creation
  #   a recommended pattern by Martin Morgan on SO
  return(new("clv.ggomnbd.static.cov",
             clv.fitted.transactions.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.ggomnbd))
}
