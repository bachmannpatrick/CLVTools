#' @templateVar name_model_full BG/NBD
#' @templateVar name_class_clvmodel clv.model.bgnbd.static.cov
#' @template template_class_clvfittedtransactionmodels_staticcov
#'
#' @template template_slot_bgnbdcbs
#'
#' @seealso \linkS4class{clv.fitted.transactions.static.cov}, \linkS4class{clv.model.bgnbd.static.cov}, \linkS4class{clv.bgnbd}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_bgnbd_staticcov.R class_clv_data_staticcovariates.R class_clv_fitted_transactions_staticcov.R
setClass(Class = "clv.bgnbd.static.cov", contains = "clv.fitted.transactions.static.cov",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


#' @importFrom methods new
clv.bgnbd.static.cov <- function(cl, clv.data){

  dt.cbs.bgnbd <- bgnbd_cbs(clv.data = clv.data)
  clv.model    <- clv.model.bgnbd.static.cov()

  return(new("clv.bgnbd.static.cov",
             clv.fitted.transactions.static.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.bgnbd))
}
