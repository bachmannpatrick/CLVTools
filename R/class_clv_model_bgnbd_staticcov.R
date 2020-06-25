#' @templateVar name_model_full BG/NBD
#' @template template_class_clvmodelstaticcov
#'
#' @seealso Other clv model classes \link{clv.model-class}, \link{clv.model.bgnbd.no.cov-class}
#' @seealso Classes using its instance: \link{clv.fitted.static.cov-class},
#'
#' @include all_generics.R class_clv_model.R class_clv_model_bgnbd.R
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
    stop("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE)
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

  # Alpha is for trans, a and b for live!
  params_i[, r       := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha_i := clv.fitted@prediction.params.model[["alpha"]] * exp( -m.cov.data.trans  %*% clv.fitted@prediction.params.trans)]
  params_i[, a_i     := clv.fitted@prediction.params.model[["a"]]     * exp(  m.cov.data.life   %*% clv.fitted@prediction.params.life)]
  params_i[, b_i     := clv.fitted@prediction.params.model[["b"]]     * exp(  m.cov.data.life   %*% clv.fitted@prediction.params.life)]

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
  r <- alpha <- a <- b <- period.length <- CET <- PAlive <- DERT <- i.CET <- i.PAlive <- i.DERT <- x <- t.x <- T.cal <- NULL

  predict.number.of.periods <- dt.prediction[1, period.length]

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
  # Add DERT
  dt.result[, DERT := 0]

  # Add results to prediction table, by matching Id
  dt.prediction[dt.result, CET    := i.CET,    on = "Id"]
  dt.prediction[dt.result, PAlive := i.PAlive, on = "Id"]
  dt.prediction[dt.result, DERT   := i.DERT,   on = "Id"]

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

