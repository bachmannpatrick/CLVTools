# Class --------------------------------------------------------------------------------------------------------------------------------
#' @importFrom methods setClass
#' @include all_generics.R
setClass(Class = "clv.model.bgbb.no.cov", contains = "clv.model",
         slots = list(),
         prototype = list(
           name.model = "BG/BB Standard",
           names.original.params.model = c(alpha="alpha", beta="beta", gamma="gamma", delta="delta"),
           names.prefixed.params.model = c("log.alpha", "log.beta", "log.gamma", "log.delta"),
           start.params.model = c(alpha = 0.5, beta = 1, gamma = 0.5, delta = 1)
         ))


# Methods --------------------------------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.bgbb.no.cov"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){

  err.msg <- c()

  # Have to be > 0 as will be logged
  if(any(start.params.model <= 0)){
    err.msg <- c(err.msg, "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")
  }


  if(use.cor){
    err.msg <- c(err.msg, "Correlation is not supported for the BG/BB model")
  }

  if(length(list(...)) > 0){
    warning("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)
  }

  check_err_msg(err.msg)

})

setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.bgbb.no.cov"), definition = function(clv.model, clv.fitted, verbose, ...){
  # nothing to put specifically for this model
  return(clv.fitted)
})

#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.bgbb.no.cov"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})

#' @importFrom stats setNames
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.bgbb.no.cov"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})

setMethod(f = "clv.model.put.newdata", signature = signature(clv.model = "clv.model.bgbb.no.cov"), definition = function(clv.model, clv.fitted, verbose){
  # clv.data in clv.fitted is already replaced with newdata here
  # Need to only redo cbs if given new data
  clv.fitted@cbs <- bgbb_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})

setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.bgbb.no.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){
  # Also model optimization settings should go here

  # Only add LL function args, everything else is prepared already, incl. start parameters

  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = bgbb_nocov_LL_sum,
                                 LL.function.ind = bgbb_nocov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,
                                 vN_cal = clv.fitted@cbs$n.cal,
                                 # parameter ordering for the callLL interlayer
                                 LL.params.names.ordered = c(log.alpha = "log.alpha", log.beta =  "log.beta", log.gamma = "log.gamma", log.delta = "log.delta")),
                            keep.null = TRUE)
  return(optimx.args)
})

#' @include all_generics.R
setMethod("clv.model.expectation", signature(clv.model="clv.model.bgbb.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  alpha <- beta <- gamma <- delta <- date.first.repeat.trans<- date.first.actual.trans <- T.cal <- n.cal <- period.first.trans<-NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "n.cal", "date.first.actual.trans")]

  params_i[, alpha := clv.fitted@prediction.params.model[["alpha"]]]
  params_i[, beta := clv.fitted@prediction.params.model[["beta"]]]
  params_i[, gamma := clv.fitted@prediction.params.model[["gamma"]]]
  params_i[, delta := clv.fitted@prediction.params.model[["delta"]]]

  fct.bgbb.expectation <- function(params_i.t){
    term1 <- params_i.t[,alpha/(alpha + beta) * delta/(gamma - 1)]
    term2 <- params_i.t[,exp(lgamma(gamma + delta) - lgamma(gamma + delta + n.cal) + lgamma(1 + delta + n.cal) - lgamma(1 + delta))]

    return(term1 * (1 - term2))
  }

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.bgnbd.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.bgbb.no.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  predict.number.of.periods <- dt.prediction[1, period.length]

  # Add CET
  dt.prediction[, CET := bgbb_nocov_CET(alpha = clv.fitted@prediction.params.model[["alpha"]],
                                         beta = clv.fitted@prediction.params.model[["beta"]],
                                         gamma = clv.fitted@prediction.params.model[["gamma"]],
                                         delta = clv.fitted@prediction.params.model[["delta"]],
                                         nPeriods = predict.number.of.periods,
                                         vX = clv.fitted@cbs[, x],
                                         vT_x = clv.fitted@cbs[, t.x],
                                         vT_cal = clv.fitted@cbs[, T.cal],
                                         vN_cal = clv.fitted@cbs[, n.cal])]


  # Add PAlive
  dt.prediction[, PAlive := bgbb_nocov_PAlive(alpha = clv.fitted@prediction.params.model[["alpha"]],
                                               beta = clv.fitted@prediction.params.model[["beta"]],
                                               gamma = clv.fitted@prediction.params.model[["gamma"]],
                                               delta = clv.fitted@prediction.params.model[["delta"]],
                                               vX = clv.fitted@cbs[, x],
                                               vT_x = clv.fitted@cbs[, t.x],
                                               vT_cal = clv.fitted@cbs[, T.cal],
                                               vN_cal = clv.fitted@cbs[, n.cal])]
  # Add DERT
  dt.prediction[, DERT := bgbb_nocov_DERT(alpha = clv.fitted@prediction.params.model[["alpha"]],
                                          beta = clv.fitted@prediction.params.model[["beta"]],
                                          gamma = clv.fitted@prediction.params.model[["gamma"]],
                                          delta = clv.fitted@prediction.params.model[["delta"]],
                                          continuous_discount_factor = continuous.discount.factor,
                                          vX     = clv.fitted@cbs[, x],
                                          vT_x   = clv.fitted@cbs[, t.x],
                                          vT_cal = clv.fitted@cbs[, T.cal],
                                          vN_cal = clv.fitted@cbs[, n.cal])]

  return(dt.prediction)
})

# .clv.model.vcov.jacobi.diag --------------------------------------------------------------------------------------------------------

setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.bgbb.no.cov"), definition = function(clv.model, clv.fitted, prefixed.params){
  # Create matrix with the full required size
  m.diag <- diag(x = 0, ncol = length(prefixed.params), nrow=length(prefixed.params))
  rownames(m.diag) <- colnames(m.diag) <- names(prefixed.params)

  # Add the transformations for the model to the matrix
  #   All model params need to be exp()
  m.diag[clv.model@names.prefixed.params.model,
         clv.model@names.prefixed.params.model] <- diag(x = exp(prefixed.params[clv.model@names.prefixed.params.model]),
                                                        nrow = length(clv.model@names.prefixed.params.model),
                                                        ncol = length(clv.model@names.prefixed.params.model))
  return(m.diag)
})
