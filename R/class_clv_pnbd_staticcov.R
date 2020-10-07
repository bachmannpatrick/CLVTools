#' @templateVar name_model_full Pareto/NBD
#' @templateVar name_class_clvmodel clv.model.pnbd.static.cov
#' @template template_class_clvfittedtransactionmodels_staticcov
#'
#' @template template_slot_pnbdcbs
#'
#' @seealso \linkS4class{clv.fitted.transactions.static.cov}, \linkS4class{clv.model.pnbd.static.cov}, \linkS4class{clv.pnbd}, \linkS4class{clv.pnbd.dynamic.cov}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_pnbd_staticcov.R class_clv_data_staticcovariates.R class_clv_fitted_transactions_staticcov.R
setClass(Class = "clv.pnbd.static.cov", contains = "clv.fitted.transactions.static.cov",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


#' @importFrom methods new
clv.pnbd.static.cov <- function(cl, clv.data){

  dt.cbs.pnbd <- pnbd_cbs(clv.data = clv.data)
  clv.model   <- clv.model.pnbd.static.cov()

  return(new("clv.pnbd.static.cov",
             clv.fitted.transactions.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.pnbd))
}
