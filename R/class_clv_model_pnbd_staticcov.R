#' @templateVar name_model_full Pareto/NBD
#' @template template_class_clvmodelstaticcov
#'
#' @seealso Other clv model classes \linkS4class{clv.model}, \linkS4class{clv.model.pnbd.no.cov}, \linkS4class{clv.model.pnbd.dynamic.cov}
#' @seealso Classes using its instance: \linkS4class{clv.fitted.transactions.static.cov}
#'
#' @include all_generics.R class_clv_model_pnbd.R
setClass(Class = "clv.model.pnbd.static.cov", contains = "clv.model.pnbd.no.cov",
         slots = list(
           start.param.cov = "numeric"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           start.param.cov = numeric(0)))


#' @importFrom methods new
clv.model.pnbd.static.cov <- function(){

  # optimx.args stay the same as for nocov

  return(new("clv.model.pnbd.static.cov",
             clv.model.pnbd.no.cov(),
             # Overwrite nocov name
             name.model      = "Pareto NBD with Static Covariates",
             start.param.cov = 0.1))
}


# Methods --------------------------------------------------------------------------------------------------------------------------------

# .clv.model.check.input.args ------------------------------------------------------------------------------------------------------------
# use nocov, no static cov checks

# . clv.model.put.estimation.input ------------------------------------------------------------------------------------------------------------
# Nothing specific required, use nocov

# . clv.model.transform.start.params.cov ------------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.transform.start.params.cov", signature = signature(clv.model="clv.model.pnbd.static.cov"), definition = function(clv.model, start.params.cov){
  # no transformation needed
  return(start.params.cov)
})

# . clv.model.backtransform.estimated.params.cov -----------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.backtransform.estimated.params.cov", signature = signature(clv.model="clv.model.pnbd.static.cov"), definition = function(clv.model, prefixed.params.cov){
  # no transformation needed
  return(prefixed.params.cov)
})

# . clv.model.prepare.optimx.args -----------------------------------------------------------------------------------------------------
#' @importFrom utils modifyList
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.pnbd.static.cov"),
          definition = function(clv.model, clv.fitted, prepared.optimx.args){

            # Do not call the no.cov function as the LL is different

            # Everything to call the LL function
            optimx.args <- modifyList(prepared.optimx.args,
                                      list(
                                        obj = clv.fitted,
                                        LL.function.sum = pnbd_staticcov_LL_sum,
                                        LL.function.ind = pnbd_staticcov_LL_ind, # if doing correlation

                                        # For cpp static cov the param order is: model, life, trans
                                        LL.params.names.ordered = c(clv.model@names.prefixed.params.model,
                                                                    clv.fitted@names.prefixed.params.after.constr.life,
                                                                    clv.fitted@names.prefixed.params.after.constr.trans),
                                        vX      = clv.fitted@cbs$x,
                                        vT_x    = clv.fitted@cbs$t.x,
                                        vT_cal  = clv.fitted@cbs$T.cal,

                                        mCov_life  = clv.data.get.matrix.data.cov.life(clv.data = clv.fitted@clv.data, correct.row.names=clv.fitted@cbs$Id,
                                                                                       correct.col.names=clv.data.get.names.cov.life(clv.fitted@clv.data)),
                                        mCov_trans = clv.data.get.matrix.data.cov.trans(clv.data = clv.fitted@clv.data, correct.row.names=clv.fitted@cbs$Id,
                                                                                        correct.col.names=clv.data.get.names.cov.trans(clv.fitted@clv.data))),
                                      keep.null = TRUE)
            return(optimx.args)
          })

# . clv.model.vcov.jacobi.diag -----------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.pnbd.static.cov"),
          definition = function(clv.model, clv.fitted, prefixed.params){
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

# . clv.model.predict -----------------------------------------------------------------------------------------------------
setMethod("clv.model.predict", signature(clv.model="clv.model.pnbd.static.cov"), definition = function(clv.model, clv.fitted, dt.predictions, verbose, continuous.discount.factor, ...){
  # cran silence
  period.length <- CET <- i.CET <- x <- t.x <- T.cal <- PAlive <- i.PAlive <- i.DERT <- DERT <- NULL

  predict.number.of.periods <- dt.predictions[1, period.length]

  # To ensure sorting, do everything in a single table
  dt.result <- copy(clv.fitted@cbs[, c("Id", "x", "t.x", "T.cal")])
  data.cov.mat.life  <- clv.data.get.matrix.data.cov.life(clv.data = clv.fitted@clv.data, correct.row.names=dt.result$Id,
                                                          correct.col.names=names(clv.fitted@prediction.params.life))
  data.cov.mat.trans <- clv.data.get.matrix.data.cov.trans(clv.data = clv.fitted@clv.data, correct.row.names=dt.result$Id,
                                                           correct.col.names=names(clv.fitted@prediction.params.trans))

  # Add CET
  dt.result[, CET :=  pnbd_staticcov_CET(r       = clv.fitted@prediction.params.model[["r"]],
                                         alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                         s       = clv.fitted@prediction.params.model[["s"]],
                                         beta_0  = clv.fitted@prediction.params.model[["beta"]],
                                         dPeriods = predict.number.of.periods,
                                         vX     = x,
                                         vT_x   = t.x,
                                         vT_cal = T.cal,
                                         vCovParams_trans = clv.fitted@prediction.params.trans,
                                         vCovParams_life  = clv.fitted@prediction.params.life,
                                         mCov_trans  = data.cov.mat.trans,
                                         mCov_life   = data.cov.mat.life)]

  # Add PAlive
  dt.result[, PAlive := pnbd_staticcov_PAlive(r       = clv.fitted@prediction.params.model[["r"]],
                                              alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                              s       = clv.fitted@prediction.params.model[["s"]],
                                              beta_0  = clv.fitted@prediction.params.model[["beta"]],
                                              vX     = x,
                                              vT_x   = t.x,
                                              vT_cal = T.cal,
                                              vCovParams_trans = clv.fitted@prediction.params.trans,
                                              vCovParams_life  = clv.fitted@prediction.params.life,
                                              mCov_trans = data.cov.mat.trans,
                                              mCov_life  = data.cov.mat.life)]

  # Add DERT
  dt.result[, DERT := pnbd_staticcov_DERT(r       = clv.fitted@prediction.params.model[["r"]],
                                          alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                          s       = clv.fitted@prediction.params.model[["s"]],
                                          beta_0  = clv.fitted@prediction.params.model[["beta"]],
                                          continuous_discount_factor = continuous.discount.factor,
                                          vX     = x,
                                          vT_x   = t.x,
                                          vT_cal = T.cal,
                                          mCov_life     = data.cov.mat.life,
                                          mCov_trans    = data.cov.mat.trans,
                                          vCovParams_life  = clv.fitted@prediction.params.life,
                                          vCovParams_trans = clv.fitted@prediction.params.trans)]


  # Add results to prediction table, by matching Id
  dt.predictions[dt.result, CET    := i.CET,    on = "Id"]
  dt.predictions[dt.result, PAlive := i.PAlive, on = "Id"]
  dt.predictions[dt.result, DERT   := i.DERT,   on = "Id"]

  return(dt.predictions)
})

# . clv.model.expectation -----------------------------------------------------------------------------------------------------
setMethod("clv.model.expectation", signature(clv.model="clv.model.pnbd.static.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){

  r<-s<-alpha_i<-beta_i <- T.cal <- t_i <- NULL

  #calculate alpha_i, beta_i
  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  m.cov.data.life  <- clv.data.get.matrix.data.cov.life(clv.data=clv.fitted@clv.data, correct.row.names=params_i$Id,
                                                        correct.col.names=names(clv.fitted@prediction.params.life))
  m.cov.data.trans <- clv.data.get.matrix.data.cov.trans(clv.data=clv.fitted@clv.data, correct.row.names=params_i$Id,
                                                         correct.col.names=names(clv.fitted@prediction.params.trans))
  params_i[, alpha_i := pnbd_staticcov_alpha_i(alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                               vCovParams_trans = clv.fitted@prediction.params.trans,
                                               mCov_trans = m.cov.data.trans)]
  params_i[, beta_i  := pnbd_staticcov_beta_i(beta_0 = clv.fitted@prediction.params.model[["beta"]],
                                              vCovParams_life = clv.fitted@prediction.params.life,
                                              mCov_life = m.cov.data.life)]

  # To caluclate expectation at point t for customers alive in t, given in params_i.t
  fct.expectation <- function(params_i.t) {
    return(drop(pnbd_staticcov_expectation(r        = clv.fitted@prediction.params.model[["r"]],
                                           s        = clv.fitted@prediction.params.model[["s"]],
                                           vAlpha_i = params_i.t$alpha_i,
                                           vBeta_i  = params_i.t$beta_i,
                                           vT_i     = params_i.t$t_i)))}

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})



