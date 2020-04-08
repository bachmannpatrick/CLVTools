# Class --------------------------------------------------------------------------------------------------------------------------------
#' @include class_clv_model_basestrategy.R class_clv_model_pnbd_nocov.R
setClass(Class = "clv.model.pnbd.static.cov", contains = "clv.model.pnbd.no.cov",
         slots = list(start.param.cov = "numeric"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         #  init with model defaults
         prototype = list(start.param.cov = 1,
                          # New model defaults
                          optimx.defaults  = list(method = "L-BFGS-B",
                                                  itnmax = 3000),
                          name.model       = "Pareto NBD with Static Covariates"))


# Methods --------------------------------------------------------------------------------------------------------------------------------

# .clv.model.check.input.args ------------------------------------------------------------------------------------------------------------
#' @include all_generics.R
#' @importFrom methods callNextMethod
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.pnbd.static.cov"), definition =
            function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose,
                                names.cov.life, names.cov.trans,
                                start.params.life, start.params.trans,
                                names.cov.constr,start.params.constr,
                                reg.lambdas, ...){

  # Check start.params.model in pnbd.no.cov function
  #   but with no cov specific inputs only
  callNextMethod(clv.model=clv.model, clv.fitted=clv.fitted, start.params.model=start.params.model, use.cor=use.cor,
                 start.param.cor=start.param.cor, optimx.args=optimx.args, verbose=verbose)

  if(length(list(...)) > 0)
    warning("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)

  # Nothing model-specific to check about all other user inputs
  # Nothing to return
})

# . clv.model.put.estimation.input ------------------------------------------------------------------------------------------------------------
#   Use pnbd.no.cov methods, dont need to overwrite
# setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.pnbd.static.cov"), definition = function(clv.model, clv.fitted, ...){
#   return(callNextMethod())
# })

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
  definition = function(clv.model, clv.fitted, prepared.optimx.args,...){

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
                              # Covariate data, as matrix!
                              mCov_life  = clv.data.get.matrix.data.cov.life(clv.fitted@clv.data),
                              mCov_trans = clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data)))
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

# . clv.model.predict.clv -----------------------------------------------------------------------------------------------------
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.pnbd.static.cov"), definition = function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor,verbose){
  # cran silence
  period.length <- CET <- x <- t.x <- T.cal <- PAlive <- DERT <- NULL

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
                                             vX     = clv.fitted@cbs$x,
                                             vT_x   = clv.fitted@cbs$t.x,
                                             vT_cal = clv.fitted@cbs$T.cal,
                                             vCovParams_trans = clv.fitted@prediction.params.trans,
                                             vCovParams_life  = clv.fitted@prediction.params.life,
                                             mCov_trans  = data.cov.mat.trans,
                                             mCov_life   = data.cov.mat.life)]

  # Add PAlive
  dt.prediction[, PAlive := pnbd_staticcov_PAlive(vEstimated_params = estimated.params,
                                                  vX     = clv.fitted@cbs$x,
                                                  vT_x   = clv.fitted@cbs$t.x,
                                                  vT_cal = clv.fitted@cbs$T.cal,
                                                  vCovParams_trans = clv.fitted@prediction.params.trans,
                                                  vCovParams_life  = clv.fitted@prediction.params.life,
                                                  mCov_trans = data.cov.mat.trans,
                                                  mCov_life  = data.cov.mat.life)]

  # Add DERT
  dt.prediction[, DERT := pnbd_staticcov_DERT(vEstimated_params = estimated.params,
                                              continuous_discount_factor = continuous.discount.factor,
                                              vX     = clv.fitted@cbs$x,
                                              vT_x   = clv.fitted@cbs$t.x,
                                              vT_cal = clv.fitted@cbs$T.cal,
                                              mCov_life     = data.cov.mat.life,
                                              mCov_trans    = data.cov.mat.trans,
                                              vCovParams_life  = clv.fitted@prediction.params.life,
                                              vCovParams_trans = clv.fitted@prediction.params.trans)]

  return(dt.prediction)
})

# . clv.model.expectation -----------------------------------------------------------------------------------------------------
setMethod("clv.model.expectation", signature(clv.model="clv.model.pnbd.static.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){

  r<-s<-alpha_i<-beta_i <- T.cal <- t_i<- period.first.trans <- NULL

  #calculate alpha_i, beta_i
  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  # all params exactly the same for all customers as there are no covariates
  params_i[, r       := clv.fitted@prediction.params.model[["r"]]]
  params_i[, s       := clv.fitted@prediction.params.model[["s"]]]

  # Alpha is for trans, beta for live!
  params_i[, alpha_i := clv.fitted@prediction.params.model[["alpha"]] * exp( -clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data) %*% clv.fitted@prediction.params.trans)]
  params_i[, beta_i  := clv.fitted@prediction.params.model[["beta"]]  * exp( -clv.data.get.matrix.data.cov.life(clv.fitted@clv.data)  %*% clv.fitted@prediction.params.life)]


  # To caluclate expectation at point t for customers alive in t, given in params_i.t
  fct.expectation <- function(params_i.t) {return( params_i.t[, (r * beta_i)/(alpha_i * (s - 1)) * (1 - (beta_i/(beta_i + t_i))^(s - 1))] )}

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})



