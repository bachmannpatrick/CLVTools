#' @importFrom methods setClass
#' @include class_clv_model_pnbd_dynamiccov.R class_clv_data_dynamiccovariates.R class_clv_fitted_staticcov.R
setClass(Class = "clv.pnbd.dynamic.cov", contains = "clv.fitted.dynamic.cov",
         slots = c(
           cbs = "data.table",

           data.walks.life  = "list",
           data.walks.trans = "list",

           LL.data          = "data.table"),

         # Prototype is labeled not useful anymore,
         # but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table(),
           data.walks.life  = list(),
           data.walks.trans = list(),

           LL.data          = data.table()))


# Convenience constructor to encapsulate all steps for object creation
#' @include class_clv_data.R
clv.pnbd.dynamic.cov <- function(cl, clv.data){

  dt.cbs.pnbd <- pnbd_dyncov_cbs(clv.data = clv.data)

  clv.model <- new("clv.model.pnbd.dynamic.cov")

  # Do walks only after inputchecks. (in clv.model.put.estimation.input)

  # Reuse clv.fitted constructor to ensure proper object creation
  #   a recommended pattern by Martin Morgan on SO
  return(new("clv.pnbd.dynamic.cov",
             clv.fitted.dynamic.cov(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.pnbd))
}

# Dyncov cbs also has d_omega for every customer
pnbd_dyncov_cbs <- function(clv.data){
  dt.cbs <- pnbd_cbs(clv.data = clv.data)

  # The CBS for pnbd dyncov also contains d_omega for every customer
  # d_omega: "= Time difference between the very first purchase (start of the observation period)
  #   and the end of the interval the first purchase is contained in."
  dt.cbs[, d_omega := clv.time.interval.in.number.tu(clv.time=clv.data@clv.time,
                                                     interv = interval(start = date.first.actual.trans,
                                                                       end = clv.time.ceiling.date(clv.time=clv.data@clv.time,
                                                                                                   date.first.actual.trans)))]

  return(dt.cbs)
}
