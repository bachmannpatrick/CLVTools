#' CLV Model functionality for BG/NBD with static covariates
#'
#' This class implements the functionalities and model-specific steps which are required
#' to fit the BG/NBD model with static covariates.
#'
#' @keywords internal
#' @seealso Other clv model classes \link{clv.model-class}, \link{clv.model.bgnbd.no.cov-class}
#' @seealso Classes using its instance: \link{clv.fitted.static.cov-class},
#'
#' @include all_generics.R class_clv_model.R class_clv_model_bgnbd.R
setClass(Class = "clv.model.bgnbd.static.cov", contains = "clv.model.bgnbd.no.cov",
         slots = list(start.param.cov = "numeric"),
         prototype = list(
           start.param.cov = numeric()
         ))

#' @importFrom methods new
clv.model.bgnbd.static.cov <- function(){
  return(new("clv.model.bgnbd.static.cov",
             clv.model.bgnbd.no.cov(),
             name.model = "BG/NBD with Static Covariates",
             start.param.cov = 1,
             optimx.defaults = list(method="L-BFGS-B",
                  itnmax = 3000)
             ))
}

# Methods --------------------------------------------------------------------------------------------------------------------------------
#' @importFrom methods callNextMethod
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.bgnbd.static.cov"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose,
                                                                                                                                 names.cov.life, names.cov.trans,
                                                                                                                                 start.params.life, start.params.trans,
                                                                                                                                 names.cov.constr,start.params.constr,
                                                                                                                                 reg.lambdas, ...){

  # Check start.params.model in bgnbd.no.cov function
  #   but with no cov specific inputs only
  callNextMethod(clv.model=clv.model, clv.fitted=clv.fitted, start.params.model=start.params.model, use.cor=use.cor,
                 start.param.cor=start.param.cor, optimx.args=optimx.args, verbose=verbose)

  if(length(list(...)) > 0)
    stop("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)
})

# . clv.model.put.estimation.input ------------------------------------------------------------------------------------------------------------
#   Use bgnbd.no.cov methods, dont need to overwrite
# setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.bgnbd.static.cov"), definition = function(clv.model, clv.fitted, ...){
#   return(callNextMethod())
# })

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
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.bgnbd.static.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){
  # Do not call the no.cov function as the LL is different

  # Everything to call the LL function
  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = bgnbd_staticcov_LL_sum,
                                 LL.function.ind = bgnbd_staticcov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,
                                 # Covariate data, as matrix!
                                 mCov_life  = clv.data.get.matrix.data.cov.life(clv.fitted@clv.data),
                                 mCov_trans = clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data),
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

  # Alpha is for trans, a and b for live!
  params_i[, r       := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha_i := clv.fitted@prediction.params.model[["alpha"]] * exp( -clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data) %*% clv.fitted@prediction.params.trans)]
  params_i[, a_i     := clv.fitted@prediction.params.model[["a"]]     * exp(  clv.data.get.matrix.data.cov.life(clv.fitted@clv.data)  %*% clv.fitted@prediction.params.life)]
  params_i[, b_i     := clv.fitted@prediction.params.model[["b"]]     * exp(  clv.data.get.matrix.data.cov.life(clv.fitted@clv.data)  %*% clv.fitted@prediction.params.life)]

  fct.bgnbd.expectation <- function(params_i.t){
    term1 <- params_i.t[,(a_i + b_i - 1)/(a_i - 1)]
    term2 <- params_i.t[,(alpha_i/(alpha_i + t_i))^r]
    term3 <- params_i.t[, vec_gsl_hyp2f1_e(r, b_i, a_i+b_i-1, t_i/(alpha_i+t_i))$value]

    return(term1 * (1 - term2 * term3))
  }

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.bgnbd.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

# . clv.model.predict.clv -----------------------------------------------------------------------------------------------------
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.bgnbd.static.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  r <- alpha <- a <- b <- period.length <- CET <- PAlive <- x <- t.x <- T.cal <- DERT <- NULL

  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  # Covariates as matrix, if there is a covariate
  data.cov.mat.life  <- clv.data.get.matrix.data.cov.life(clv.fitted@clv.data)
  data.cov.mat.trans <- clv.data.get.matrix.data.cov.trans(clv.fitted@clv.data)

  predict.number.of.periods <- dt.prediction[1, period.length]

  # Add CET
  dt.prediction[, CET := bgnbd_staticcov_CET(r     = clv.fitted@prediction.params.model[["r"]],
                                             alpha = clv.fitted@prediction.params.model[["alpha"]],
                                             a     = clv.fitted@prediction.params.model[["a"]],
                                             b     = clv.fitted@prediction.params.model[["b"]],
                                             nPeriods = predict.number.of.periods,
                                             vX     = clv.fitted@cbs$x,
                                             vT_x   = clv.fitted@cbs$t.x,
                                             vT_cal = clv.fitted@cbs$T.cal,
                                             vCovParams_trans = clv.fitted@prediction.params.trans,
                                             vCovParams_life  = clv.fitted@prediction.params.life,
                                             mCov_trans  = data.cov.mat.trans,
                                             mCov_life   = data.cov.mat.life)]


  # Add PAlive
  dt.prediction[, PAlive := bgnbd_staticcov_PAlive(r     = clv.fitted@prediction.params.model[["r"]],
                                                   alpha = clv.fitted@prediction.params.model[["alpha"]],
                                                   a     = clv.fitted@prediction.params.model[["a"]],
                                                   b     = clv.fitted@prediction.params.model[["b"]],
                                                   vX     = clv.fitted@cbs[, x],
                                                   vT_x   = clv.fitted@cbs[, t.x],
                                                   vT_cal = clv.fitted@cbs[, T.cal],
                                                   vCovParams_trans = clv.fitted@prediction.params.trans,
                                                   vCovParams_life  = clv.fitted@prediction.params.life,
                                                   mCov_trans = data.cov.mat.trans,
                                                   mCov_life  = data.cov.mat.life)]
  # Add DERT
  dt.prediction[, DERT := 0]

  return(dt.prediction)
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

