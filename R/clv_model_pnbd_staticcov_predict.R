#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.pnbd.static.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor,verbose){

  CET <- x <- t.x <- T.cal <- PAlive <- DERT <- NULL

  # Covariates as matrix, if there is a covariate
  data.cov.mat.life  <- clv.data.get.matrix.data.cov.life(clv.fitted@clv.data)
  data.cov.mat.trans <- clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data)

  # **TODO:Check that matrices have same order as cbs?? (stopifnot(all(rownames() == cbs$Id))?)

  # Read out from table
  predict.number.of.periods <- dt.prediction[1, period.length]

  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  # Put params together in single vec
  estimated.params <- c(r = clv.fitted@prediction.params.model[["r"]], alpha = clv.fitted@prediction.params.model[["alpha"]],
                        s = clv.fitted@prediction.params.model[["s"]], beta  = clv.fitted@prediction.params.model[["beta"]])


  # Add CET
  dt.prediction[, CET :=  pnbd_staticcov_CET(vEstimated_params  = estimated.params,
                                             dPrediction_period = predict.number.of.periods,
                                             vX     = clv.fitted@cbs[, x],
                                             vT_x   = clv.fitted@cbs[, t.x],
                                             vT_cal = clv.fitted@cbs[, T.cal],
                                             vCovParams_trans = clv.fitted@prediction.params.trans,
                                             vCovParams_life  = clv.fitted@prediction.params.life,
                                             mCov_trans  = data.cov.mat.trans,
                                             mCov_life   = data.cov.mat.life)]

  # Add PAlive
  dt.prediction[, PAlive := pnbd_staticcov_PAlive(vEstimated_params = estimated.params,
                                                  vX     = clv.fitted@cbs[, x],
                                                  vT_x   = clv.fitted@cbs[, t.x],
                                                  vT_cal = clv.fitted@cbs[, T.cal],
                                                  vCovParams_trans = clv.fitted@prediction.params.trans,
                                                  vCovParams_life  = clv.fitted@prediction.params.life,
                                                  mCov_trans = data.cov.mat.trans,
                                                  mCov_life  = data.cov.mat.life)]

  # Add DERT
  dt.prediction[, DERT := pnbd_staticcov_DERT(vEstimated_params = estimated.params,
                                              continuous_discount_factor = continuous.discount.factor,
                                              vX     = clv.fitted@cbs[, x],
                                              vT_x   = clv.fitted@cbs[, t.x],
                                              vT_cal = clv.fitted@cbs[, T.cal],
                                              mCov_life     = data.cov.mat.life,
                                              mCov_trans    = data.cov.mat.trans,
                                              vCovParams_life  = clv.fitted@prediction.params.life,
                                              vCovParams_trans = clv.fitted@prediction.params.trans)]

  return(dt.prediction)
})

