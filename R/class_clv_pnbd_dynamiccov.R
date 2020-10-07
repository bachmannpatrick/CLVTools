#' Result of fitting the Pareto/NBD model with dynamic covariates
#'
#' @description
#' Output from fitting the Pareto/NBD model on data with dynamic covariates. It constitutes the
#' estimation result and is returned to the user to use it as input to other methods such as
#' to make predictions or plot the unconditional expectation.
#'
#' Inherits from \code{clv.fitted.transactions.dynamic.cov} in order to execute all steps required for fitting a model
#' with dynamic covariates and it contains an instance of class \code{clv.model.pnbd.dynamic.cov} which
#' provides the required Pareto/NBD (dynamic covariates) specific functionalities.
#'
#' @template template_slot_pnbdcbs
#'
#' @seealso \linkS4class{clv.fitted.transactions.dynamic.cov}, \linkS4class{clv.model.pnbd.dynamic.cov}, \linkS4class{clv.pnbd}, \linkS4class{clv.pnbd.static.cov}
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_pnbd_dynamiccov.R class_clv_data_dynamiccovariates.R
setClass(Class = "clv.pnbd.dynamic.cov", contains = "clv.fitted.transactions.dynamic.cov",
         slots = c(
           cbs = "data.table",

           data.walks.life  = "list",
           data.walks.trans = "list",

           LL.data          = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table(),

           data.walks.life  = list(),
           data.walks.trans = list(),

           LL.data          = data.table()))


#' @importFrom methods new
clv.pnbd.dynamic.cov <- function(cl, clv.data){

  dt.cbs.pnbd <- pnbd_dyncov_cbs(clv.data = clv.data)
  clv.model   <- clv.model.pnbd.dynamic.cov()
  # Create walks only after inputchecks
  #   (walks are done in clv.model.put.estimation.input)

  return(new("clv.pnbd.dynamic.cov",
             clv.fitted.transactions.dynamic.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.pnbd))
}

# Dyncov cbs also has d_omega for every customer
pnbd_dyncov_cbs <- function(clv.data){
  d_omega <- date.first.actual.trans <- NULL

  dt.cbs <- pnbd_cbs(clv.data = clv.data)

  # The CBS for pnbd dyncov additinoally contains d_omega for every customer
  # d_omega: "= Time difference between the very first purchase (start of the observation period)
  #   and the end of the interval the first purchase is contained in."
  dt.cbs[, d_omega := clv.time.interval.in.number.tu(clv.time=clv.data@clv.time,
                                                     interv = interval(start = date.first.actual.trans,
                                                                       end = clv.time.ceiling.date(clv.time=clv.data@clv.time,
                                                                                                   date.first.actual.trans)))]

  return(dt.cbs)
}
