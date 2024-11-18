#' @templateVar name_model_full GGompertz/NBD
#' @template template_class_clvmodelstaticcov
#'
#' @seealso Other clv model classes \linkS4class{clv.model}, \linkS4class{clv.model.ggomnbd.no.cov}
#' @seealso Classes using its instance: \linkS4class{clv.fitted.transactions.static.cov}
#'
#' @include all_generics.R class_clv_model_ggomnbd_nocov.R
setClass(Class = "clv.model.ggomnbd.static.cov", contains = "clv.model.ggomnbd.no.cov",
         slots = list(start.param.cov = "numeric"),
         prototype = list(
           start.param.cov = numeric(0)
         ))


#' @importFrom methods new
clv.model.ggomnbd.static.cov <- function(){
  return(new("clv.model.ggomnbd.static.cov",
             clv.model.ggomnbd.no.cov(),
             name.model = "GGompertz/NBD with Static Covariates",
             start.param.cov = 0.1))
}

# Methods --------------------------------------------------------------------------------------------------------------------------------
# . clv.model.check.input.args ------------------------------------------------------------------------------------------------------------
# Use nocov

# . clv.model.put.estimation.input ------------------------------------------------------------------------------------------------------------
# Nothing specific required, use nocov

# . clv.model.transform.start.params.cov ------------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.transform.start.params.cov", signature = signature(clv.model="clv.model.ggomnbd.static.cov"), definition = function(clv.model, start.params.cov){
  # no transformation needed
  return(start.params.cov)
})

# . clv.model.backtransform.estimated.params.cov -----------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.backtransform.estimated.params.cov", signature = signature(clv.model="clv.model.ggomnbd.static.cov"), definition = function(clv.model, prefixed.params.cov){
  # no transformation needed
  return(prefixed.params.cov)
})

# . clv.model.process.newdata -----------------------------------------------------------------------------------------------------
#   Use ggomnbd.no.cov methods, dont need to overwrite


# . clv.model.prepare.optimx.args -----------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.ggomnbd.static.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args){

  l.LL.call.data <- clv.fitted.transactions.static.cov.compressed.ll.data(clv.fitted)

  # Everything to call the LL function
  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = ggomnbd_staticcov_LL_sum,
                                 LL.function.ind = ggomnbd_staticcov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX      = l.LL.call.data$cbs$x,
                                 vT_x    = l.LL.call.data$cbs$t.x,
                                 vT_cal  = l.LL.call.data$cbs$T.cal,
                                 vN  = l.LL.call.data$cbs$n,

                                 mCov_life  = l.LL.call.data$m.cov.life,
                                 mCov_trans = l.LL.call.data$m.cov.trans,

                                 # parameter ordering for the callLL interlayer
                                 LL.params.names.ordered = c(c(log.r = "log.r",log.alpha =  "log.alpha", log.b = "log.b", log.s = "log.s", log.beta = "log.beta"),
                                                             clv.fitted@names.prefixed.params.after.constr.life,
                                                             clv.fitted@names.prefixed.params.after.constr.trans)),
                            keep.null = TRUE)
  return(optimx.args)
})

#' @include all_generics.R
#' @importFrom stats integrate
setMethod("clv.model.expectation", signature(clv.model="clv.model.ggomnbd.static.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  r <- alpha_i <- beta_i <- b <- s <- t_i <- tau <- NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]
  m.cov.data.life  <- clv.data.get.matrix.data.cov.life(clv.data=clv.fitted@clv.data, correct.row.names=params_i$Id,
                                                        correct.col.names=names(clv.fitted@prediction.params.life))
  m.cov.data.trans <- clv.data.get.matrix.data.cov.trans(clv.data=clv.fitted@clv.data, correct.row.names=params_i$Id,
                                                         correct.col.names=names(clv.fitted@prediction.params.trans))

  params_i[, alpha_i := ggomnbd_staticcov_alpha_i(alpha_0          = clv.fitted@prediction.params.model[["alpha"]],
                                                  vCovParams_trans = clv.fitted@prediction.params.trans,
                                                  mCov_trans       = m.cov.data.trans)]
  params_i[, beta_i := ggomnbd_staticcov_beta_i(beta_0          = clv.fitted@prediction.params.model[["beta"]],
                                                vCovParams_life = clv.fitted@prediction.params.life,
                                                mCov_life       = m.cov.data.life)]

  fct.expectation <- function(params_i.t){
    return(drop(ggomnbd_staticcov_expectation(r       = clv.fitted@prediction.params.model[["r"]],
                                              b       = clv.fitted@prediction.params.model[["b"]],
                                              s       = clv.fitted@prediction.params.model[["s"]],
                                              vAlpha_i= params_i.t$alpha_i,
                                              vBeta_i = params_i.t$beta_i,
                                              vT_i    = params_i.t$t_i)))
  }

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

# . clv.model.predict.new.customer -----------------------------------------------------------------------------------------------------
setMethod("clv.model.predict.new.customer", signature = signature(clv.model="clv.model.ggomnbd.static.cov"), definition = function(clv.model, clv.fitted, clv.newcustomer){

  m.cov.trans <- clv.newcustomer.static.get.matrix.cov.trans(clv.newcustomer=clv.newcustomer, clv.fitted=clv.fitted)
  m.cov.life <- clv.newcustomer.static.get.matrix.cov.life(clv.newcustomer=clv.newcustomer, clv.fitted=clv.fitted)

  alpha_i <- ggomnbd_staticcov_alpha_i(
    alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
    vCovParams_trans = clv.fitted@prediction.params.trans,
    mCov_trans       = m.cov.trans)

  beta_i <- ggomnbd_staticcov_beta_i(
    beta_0          = clv.fitted@prediction.params.model[["beta"]],
    vCovParams_life = clv.fitted@prediction.params.life,
    mCov_life       = m.cov.life)

  return(ggomnbd_staticcov_expectation(
    r       = clv.fitted@prediction.params.model[["r"]],
    b       = clv.fitted@prediction.params.model[["b"]],
    s       = clv.fitted@prediction.params.model[["s"]],
    vAlpha_i= alpha_i,
    vBeta_i = beta_i,
    vT_i    = clv.newcustomer@num.periods))
})


# . clv.model.pmf --------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod("clv.model.pmf", signature=(clv.model="clv.model.ggomnbd.static.cov"), function(clv.model, clv.fitted, x){
  Id <- T.cal <- pmf.x <- NULL

  dt.res <- clv.fitted@cbs[, c("Id", "T.cal")]
  data.cov.mat.life  <- clv.data.get.matrix.data.cov.life(clv.data = clv.fitted@clv.data, correct.row.names=dt.res$Id,
                                                          correct.col.names=names(clv.fitted@prediction.params.life))
  data.cov.mat.trans <- clv.data.get.matrix.data.cov.trans(clv.data = clv.fitted@clv.data, correct.row.names=dt.res$Id,
                                                           correct.col.names=names(clv.fitted@prediction.params.trans))

  dt.res[, pmf.x :=  ggomnbd_staticcov_PMF(
            r       = clv.fitted@prediction.params.model[["r"]],
            alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
            b       = clv.fitted@prediction.params.model[["b"]],
            s       = clv.fitted@prediction.params.model[["s"]],
            beta_0  = clv.fitted@prediction.params.model[["beta"]],
            x       = x,
            vT_i    = T.cal,
            vCovParams_trans = clv.fitted@prediction.params.trans,
            vCovParams_life  = clv.fitted@prediction.params.life,
            mCov_life  = data.cov.mat.life,
            mCov_trans = data.cov.mat.trans)]

  dt.res <- dt.res[, list(Id, pmf.x)]
  setnames(dt.res, "pmf.x", paste0("pmf.x.", x))
  return(dt.res)
})

# . clv.model.predict --------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod("clv.model.predict", signature(clv.model="clv.model.ggomnbd.static.cov"), function(clv.model, clv.fitted, dt.predictions, verbose, continuous.discount.factor, ...){
  r <- alpha <- beta <- b <- s <- x <- t.x <- T.cal <- CET <- PAlive <- i.CET <- i.PAlive <- period.length <- NULL

  predict.number.of.periods <- dt.predictions[1, period.length]

  # To ensure sorting, do everything in a single table
  dt.result <- copy(clv.fitted@cbs[, c("Id", "x", "t.x", "T.cal")])
  data.cov.mat.life  <- clv.data.get.matrix.data.cov.life(clv.data = clv.fitted@clv.data, correct.row.names=dt.result$Id,
                                                          correct.col.names=names(clv.fitted@prediction.params.life))
  data.cov.mat.trans <- clv.data.get.matrix.data.cov.trans(clv.data = clv.fitted@clv.data, correct.row.names=dt.result$Id,
                                                           correct.col.names=names(clv.fitted@prediction.params.trans))

  # Add CET
  dt.result[, CET :=  ggomnbd_staticcov_CET(r       = clv.fitted@prediction.params.model[["r"]],
                                            alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                            b       = clv.fitted@prediction.params.model[["b"]],
                                            s       = clv.fitted@prediction.params.model[["s"]],
                                            beta_0  = clv.fitted@prediction.params.model[["beta"]],
                                            dPeriods = predict.number.of.periods,
                                            vX      = x,
                                            vT_x    = t.x,
                                            vT_cal  = T.cal,
                                            vCovParams_trans = clv.fitted@prediction.params.trans,
                                            vCovParams_life  = clv.fitted@prediction.params.life,
                                            mCov_life  = data.cov.mat.life,
                                            mCov_trans = data.cov.mat.trans)]

  # Add PAlive
  dt.result[, PAlive := ggomnbd_staticcov_PAlive(r       = clv.fitted@prediction.params.model[["r"]],
                                                 alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                                 b       = clv.fitted@prediction.params.model[["b"]],
                                                 s       = clv.fitted@prediction.params.model[["s"]],
                                                 beta_0  = clv.fitted@prediction.params.model[["beta"]],
                                                 vX      = x,
                                                 vT_x    = t.x,
                                                 vT_cal  = T.cal,
                                                 vCovParams_trans = clv.fitted@prediction.params.trans,
                                                 vCovParams_life  = clv.fitted@prediction.params.life,
                                                 mCov_life  = data.cov.mat.life,
                                                 mCov_trans = data.cov.mat.trans)]

  # Add results to prediction table, by matching Id
  dt.predictions[dt.result, CET    := i.CET,    on = "Id"]
  dt.predictions[dt.result, PAlive := i.PAlive, on = "Id"]

  return(dt.predictions)
})

# .clv.model.vcov.jacobi.diag --------------------------------------------------------------------------------------------------------

setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.ggomnbd.static.cov"), definition = function(clv.model, clv.fitted, prefixed.params){
  # Get corrections from nocov model
  m.diag.model <- callNextMethod()

  # No transformations for static covs: Set diag to 1 for all static cov params

  # Gather names of cov param
  names.cov.prefixed.params  <- c(clv.fitted@names.prefixed.params.free.life,
                                  clv.fitted@names.prefixed.params.free.trans)
  if(clv.fitted@estimation.used.constraints)
    names.cov.prefixed.params <- c(names.cov.prefixed.params, clv.fitted@names.prefixed.params.constr)

  # Set to 1
  m.diag.model[names.cov.prefixed.params,
               names.cov.prefixed.params] <- diag(x = 1,
                                                  nrow = length(names.cov.prefixed.params),
                                                  ncol = length(names.cov.prefixed.params))
  return(m.diag.model)
})
