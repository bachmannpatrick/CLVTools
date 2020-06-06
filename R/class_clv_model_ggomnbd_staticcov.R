#' CLV Model functionality for GGompertz/NBD with static covariates
#'
#' This class implements the functionalities and model-specific steps which are required
#' to fit the GGompertz/NBD model with static covariates.
#'
#' @keywords internal
#' @seealso Other clv model classes \link{clv.model-class}, \link{clv.model.ggomnbd.no.cov-class}
#' @seealso Classes using its instance: \link{clv.fitted.static.cov-class},
#'
#' @include all_generics.R class_clv_model.R class_clv_model_ggomnbd_nocov.R
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
             start.param.cov = 1,
             optimx.defaults = list(method = "L-BFGS-B",
                                    itnmax = 3000)
             ))
}

# Methods --------------------------------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.ggomnbd.static.cov"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose,
                                                                                                                                 names.cov.life, names.cov.trans,
                                                                                                                                 start.params.life, start.params.trans,
                                                                                                                                 names.cov.constr,start.params.constr,
                                                                                                                                 reg.lambdas, ...){

  # Check start.params.model in ggomnbd.no.cov function
  #   but with no cov specific inputs only
  callNextMethod(clv.model=clv.model, clv.fitted=clv.fitted, start.params.model=start.params.model, use.cor=use.cor,
                 start.param.cor=start.param.cor, optimx.args=optimx.args, verbose=verbose)

  if(length(list(...)) > 0)
    check_err_msg("Any further parameters passed in ... are ignored because they are not needed by this model.")
})

# . clv.model.put.estimation.input ------------------------------------------------------------------------------------------------------------
#   Use ggomnbd.no.cov methods, dont need to overwrite
# setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.ggomnbd.static.cov"), definition = function(clv.model, clv.fitted, ...){
#   return(callNextMethod())
# })

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

setMethod(f = "clv.model.put.newdata", signature = signature(clv.model = "clv.model.ggomnbd.static.cov"), definition = function(clv.model, clv.fitted, verbose){
  # clv.data in clv.fitted is already replaced with newdata here
  # Need to only redo cbs if given new data
  clv.fitted@cbs <- ggomnbd_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})

setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.ggomnbd.static.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){
  # Do not call the no.cov function as the LL is different

  # Everything to call the LL function
  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = ggomnbd_staticcov_LL_sum,
                                 LL.function.ind = ggomnbd_staticcov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,
                                 # Covariate data, as matrix!
                                 mCov_life  = clv.data.get.matrix.data.cov.life(clv.fitted@clv.data),
                                 mCov_trans = clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data),
                                 # parameter ordering for the callLL interlayer
                                 #** TODO: Hardcode from cpp interface
                                 LL.params.names.ordered = c(clv.model@names.prefixed.params.model,
                                                             clv.fitted@names.prefixed.params.after.constr.life,
                                                             clv.fitted@names.prefixed.params.after.constr.trans),
                                 keep.null = TRUE))
  return(optimx.args)
})

#' @include all_generics.R
#' @importFrom stats integrate
setMethod("clv.model.expectation", signature(clv.model="clv.model.ggomnbd.static.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  r <- alpha_i <- beta_i <- b <- s <- t_i <- tau <- NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]
  params_i[, r       := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha_i := clv.fitted@prediction.params.model[["alpha"]] * exp( -clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data) %*% clv.fitted@prediction.params.trans)]
  params_i[, beta_i  := clv.fitted@prediction.params.model[["beta"]]  * exp( -clv.data.get.matrix.data.cov.life(clv.fitted@clv.data)  %*% clv.fitted@prediction.params.life)]
  params_i[, b       := clv.fitted@prediction.params.model[["b"]]]
  params_i[, s       := clv.fitted@prediction.params.model[["s"]]]

  fct.ggomnbd.expectation <- function(r, alpha_i, beta_i, b, s, t_i){

    term1 <- (r/alpha_i)
    term2 <- (beta_i/(beta_i+exp(b*t_i)-1))^s*t_i
    term3 <- b*s*beta_i^s
    term4 <- integrate(f = function(tau){tau*exp(b*tau)*(beta_i + exp(b*tau)-1)^(-(s+1))}, lower = 0, upper = t_i)$value

    return(term1 * (term2 + (term3 * term4)))
  }

  fct.expectation <- function(params_i.t){
    return(params_i.t[,.(res = fct.ggomnbd.expectation(r = r, alpha_i = alpha_i, beta_i = beta_i, b = b, s = s, t_i = t_i)), by="Id"]$res)
  }

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.ggomnbd.static.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  r <- alpha <- beta <- b <- s <- CET <- PAlive <- DERT <- period.length <- NULL
  # Covariates as matrix, if there is a covariate
  data.cov.mat.life  <- clv.data.get.matrix.data.cov.life(clv.fitted@clv.data)
  data.cov.mat.trans <- clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data)


  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  predict.number.of.periods <- dt.prediction[1, period.length]

  # Put params together in single vec
  estimated.params <- c(r = clv.fitted@prediction.params.model[["r"]], alpha = clv.fitted@prediction.params.model[["alpha"]],
                        b = clv.fitted@prediction.params.model[["b"]],
                        s = clv.fitted@prediction.params.model[["s"]], beta  = clv.fitted@prediction.params.model[["beta"]])


  # Add CET
  dt.prediction[, CET :=  ggomnbd_staticcov_CET(vEstimated_params  = estimated.params,
                                             dPrediction_period = predict.number.of.periods,
                                             vX     = clv.fitted@cbs$x,
                                             vT_x   = clv.fitted@cbs$t.x,
                                             vT_cal = clv.fitted@cbs$T.cal,
                                             vCovParams_trans = clv.fitted@prediction.params.trans,
                                             vCovParams_life  = clv.fitted@prediction.params.life,
                                             mCov_life   = data.cov.mat.life,
                                             mCov_trans  = data.cov.mat.trans
  )]

  # Add PAlive
  dt.prediction[, PAlive := ggomnbd_staticcov_PAlive(vEstimated_params = estimated.params,
                                                  vX     = clv.fitted@cbs$x,
                                                  vT_x   = clv.fitted@cbs$t.x,
                                                  vT_cal = clv.fitted@cbs$T.cal,
                                                  vCovParams_trans = clv.fitted@prediction.params.trans,
                                                  vCovParams_life  = clv.fitted@prediction.params.life,
                                                  mCov_life  = data.cov.mat.life,
                                                  mCov_trans = data.cov.mat.trans)]

  # Add DERT
  dt.prediction[, DERT := 0]
  # dt.prediction[, DERT := ggomnbd_staticcov_DERT(vEstimated_params = estimated.params,
  #                                             continuous_discount_factor = continuous.discount.factor,
  #                                             vX     = clv.fitted@cbs[, x],
  #                                             vT_x   = clv.fitted@cbs[, t.x],
  #                                             vT_cal = clv.fitted@cbs[, T.cal],
  #                                             mCov_life     = data.cov.mat.life,
  #                                             mCov_trans    = data.cov.mat.trans,
  #                                             vCovParams_life  = clv.fitted@prediction.params.life,
  #                                             vCovParams_trans = clv.fitted@prediction.params.trans)]

  return(dt.prediction)
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
