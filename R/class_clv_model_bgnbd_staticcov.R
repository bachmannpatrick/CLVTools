#' @templateVar name_model_full BG/NBD
#' @template template_class_clvmodelstaticcov
#'
#' @seealso Other clv model classes \linkS4class{clv.model}, \linkS4class{clv.model.bgnbd.no.cov}
#' @seealso Classes using its instance: \linkS4class{clv.fitted.transactions.static.cov}
#'
#' @include all_generics.R class_clv_model_bgnbd.R
setClass(Class = "clv.model.bgnbd.static.cov", contains = "clv.model.bgnbd.no.cov",
         slots = list(start.param.cov = "numeric"),
         prototype = list(
           start.param.cov = numeric(0)
         ))

#' @importFrom methods new
clv.model.bgnbd.static.cov <- function(){
  return(new("clv.model.bgnbd.static.cov",
             clv.model.bgnbd.no.cov(),
             name.model = "BG/NBD with Static Covariates",
             start.param.cov = 0.1))
}

# Methods --------------------------------------------------------------------------------------------------------------------------------
# . clv.model.check.input.args ----------------------------------------------------------------------------------------------------------
# Use nocov

# . clv.model.put.estimation.input ------------------------------------------------------------------------------------------------------------
# Nothing specific required, use nocov

# . clv.model.transform.start.params.cov ------------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.transform.start.params.cov", signature = signature(clv.model="clv.model.bgnbd.static.cov"), definition = function(clv.model, start.params.cov){
  # no transformation needed
  return(start.params.cov)
})

# . clv.model.backtransform.estimated.params.cov -----------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.backtransform.estimated.params.cov", signature = signature(clv.model="clv.model.bgnbd.static.cov"), definition = function(clv.model, prefixed.params.cov){
  # no transformation needed
  return(prefixed.params.cov)
})

# . clv.model.prepare.optimx.args -----------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.bgnbd.static.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args){
  # Do not call the no.cov function as the LL is different

  # Everything to call the LL function
  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = bgnbd_staticcov_LL_sum,
                                 LL.function.ind = bgnbd_staticcov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,
                                 mCov_life  = clv.data.get.matrix.data.cov.life(clv.data = clv.fitted@clv.data, correct.row.names=clv.fitted@cbs$Id,
                                                                                correct.col.names=clv.data.get.names.cov.life(clv.fitted@clv.data)),
                                 mCov_trans = clv.data.get.matrix.data.cov.trans(clv.data = clv.fitted@clv.data, correct.row.names=clv.fitted@cbs$Id,
                                                                                 correct.col.names=clv.data.get.names.cov.trans(clv.fitted@clv.data)),
                                 # parameter ordering for the callLL interlayer
                                 LL.params.names.ordered = c(clv.model@names.prefixed.params.model,
                                                             clv.fitted@names.prefixed.params.after.constr.life,
                                                             clv.fitted@names.prefixed.params.after.constr.trans),
                                 keep.null = TRUE))
  return(optimx.args)
})



# . clv.model.expectation -----------------------------------------------------------------------------------------------------
setMethod("clv.model.expectation", signature(clv.model="clv.model.bgnbd.static.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  r <- alpha_i <- a_i <- b_i <- date.first.repeat.trans<- date.first.actual.trans <- T.cal <- t_i<- period.first.trans<-NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]
  m.cov.data.life  <- clv.data.get.matrix.data.cov.life(clv.data=clv.fitted@clv.data, correct.row.names=params_i$Id,
                                                        correct.col.names=names(clv.fitted@prediction.params.life))
  m.cov.data.trans <- clv.data.get.matrix.data.cov.trans(clv.data=clv.fitted@clv.data, correct.row.names=params_i$Id,
                                                         correct.col.names=names(clv.fitted@prediction.params.trans))

  params_i[, alpha_i := bgnbd_staticcov_alpha_i(alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                                vCovParams_trans = clv.fitted@prediction.params.trans,
                                                mCov_trans = m.cov.data.trans)]
  params_i[, a_i := bgnbd_staticcov_a_i(a_0 = clv.fitted@prediction.params.model[["a"]],
                                        vCovParams_life = clv.fitted@prediction.params.life,
                                        mCov_life = m.cov.data.life)]
  params_i[, b_i := bgnbd_staticcov_b_i(b_0 = clv.fitted@prediction.params.model[["b"]],
                                        vCovParams_life = clv.fitted@prediction.params.life,
                                        mCov_life = m.cov.data.life)]

  # Alpha is for trans, a and b for live!
  fct.bgnbd.expectation <- function(params_i.t){
    return(drop(bgnbd_staticcov_expectation(r        = clv.fitted@prediction.params.model[["r"]],
                                            vAlpha_i = params_i.t$alpha_i,
                                            vA_i     = params_i.t$a_i,
                                            vB_i     = params_i.t$b_i,
                                            vT_i     = params_i.t$t_i)))}

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.bgnbd.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

# . clv.model.predict -----------------------------------------------------------------------------------------------------
setMethod("clv.model.predict", signature(clv.model="clv.model.bgnbd.static.cov"), function(clv.model, clv.fitted, dt.predictions, verbose, continuous.discount.factor, ...){
  r <- alpha <- a <- b <- period.length <- CET <- PAlive <- i.CET <- i.PAlive <- x <- t.x <- T.cal <- NULL

  predict.number.of.periods <- dt.predictions[1, period.length]

  # To ensure sorting, do everything in a single table
  dt.result <- copy(clv.fitted@cbs[, c("Id", "x", "t.x", "T.cal")])
  data.cov.mat.life  <- clv.data.get.matrix.data.cov.life(clv.data = clv.fitted@clv.data, correct.row.names=dt.result$Id,
                                                          correct.col.names=names(clv.fitted@prediction.params.life))
  data.cov.mat.trans <- clv.data.get.matrix.data.cov.trans(clv.data = clv.fitted@clv.data, correct.row.names=dt.result$Id,
                                                           correct.col.names=names(clv.fitted@prediction.params.trans))

  # Add CET
  dt.result[, CET := bgnbd_staticcov_CET(r     = clv.fitted@prediction.params.model[["r"]],
                                         alpha = clv.fitted@prediction.params.model[["alpha"]],
                                         a     = clv.fitted@prediction.params.model[["a"]],
                                         b     = clv.fitted@prediction.params.model[["b"]],
                                         dPeriods = predict.number.of.periods,
                                         vX     = x,
                                         vT_x   = t.x,
                                         vT_cal = T.cal,
                                         vCovParams_trans = clv.fitted@prediction.params.trans,
                                         vCovParams_life  = clv.fitted@prediction.params.life,
                                         mCov_trans  = data.cov.mat.trans,
                                         mCov_life   = data.cov.mat.life)]


  # Add PAlive
  dt.result[, PAlive := bgnbd_staticcov_PAlive(r     = clv.fitted@prediction.params.model[["r"]],
                                               alpha = clv.fitted@prediction.params.model[["alpha"]],
                                               a     = clv.fitted@prediction.params.model[["a"]],
                                               b     = clv.fitted@prediction.params.model[["b"]],
                                               vX     = x,
                                               vT_x   = t.x,
                                               vT_cal = T.cal,
                                               vCovParams_trans = clv.fitted@prediction.params.trans,
                                               vCovParams_life  = clv.fitted@prediction.params.life,
                                               mCov_trans = data.cov.mat.trans,
                                               mCov_life  = data.cov.mat.life)]

  # Add results to prediction table, by matching Id
  dt.predictions[dt.result, CET    := i.CET,    on = "Id"]
  dt.predictions[dt.result, PAlive := i.PAlive, on = "Id"]

  return(dt.predictions)
})


# .clv.model.vcov.jacobi.diag --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.bgnbd.static.cov"), definition = function(clv.model, clv.fitted, prefixed.params){
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

