#' CLV Model functionality for GGompertz/NBD without covariates
#'
#' This class implements the functionalities and model-specific steps which are required
#' to fit the GGompertz/NBD model without covariates.
#'
#' @keywords internal
#' @importFrom methods setClass
#' @seealso Other clv model classes \link{clv.model-class}, \link{clv.model.ggomnbd.static.cov-class}
#' @seealso Classes using its instance: \link{clv.fitted-class}
#' @include all_generics.R class_clv_model.R
setClass(Class = "clv.model.ggomnbd.no.cov", contains = "clv.model",
         # no additional slots required
         slots = list(),
         prototype = list(
           name.model                  = character(0),
           names.original.params.model = character(0),
           names.prefixed.params.model = character(0),
           start.params.model          = numeric(0),
           optimx.defaults = list()
         ))

clv.model.ggomnbd.no.cov <- function(){
  return(new("clv.model.ggomnbd.no.cov",
             name.model = "GGompertz/NBD Standard",
             names.original.params.model = c(r="r", alpha="alpha", b="b", s="s", beta="beta"),
             names.prefixed.params.model = c("log.r","log.alpha", "log.b", "log.s", "log.beta"),
             start.params.model          = c(r=1, alpha=1, b=1, s=1, beta=1),
             optimx.defaults = list(method = "L-BFGS-B",
                                    itnmax  = 5000,
                                    control = list(
                                      kkt = TRUE,
                                      all.methods = FALSE,
                                      save.failures = TRUE,
                                      # Do not perform starttests because it checks the scales with max(logpar)-min(logpar)
                                      #   but all standard start parameters are <= 0, hence there are no logpars what
                                      #   produces a warning
                                      starttests = FALSE))))
}

# Methods --------------------------------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.ggomnbd.no.cov"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){

  err.msg <- c()

  # Have to be > 0 as will be logged
  if(any(start.params.model <= 0)){
    err.msg <- c(err.msg, "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")
  }

  if(length(list(...)) > 0){
    err.msg <- c(err.msg, "Any further parameters passed in ... are ignored because they are not needed by this model.")
  }

  check_err_msg(err.msg)

})

setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.ggomnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose, ...){
  # nothing to put specifically for this model
  return(clv.fitted)
})

#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.ggomnbd.no.cov"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})

#' @importFrom stats setNames
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.ggomnbd.no.cov"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})

# . clv.model.process.post.estimation -----------------------------------------------------------------------------------------
setMethod("clv.model.process.post.estimation", signature = signature(clv.model="clv.model.ggomnbd.no.cov"), definition = function(clv.model, clv.fitted, res.optimx){
  # No additional step needed (ie store model specific stuff, extra process)
  return(clv.fitted)
})

setMethod(f = "clv.model.put.newdata", signature = signature(clv.model = "clv.model.ggomnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose){
  # clv.data in clv.fitted is already replaced with newdata here
  # Need to only redo cbs if given new data
  clv.fitted@cbs <- ggomnbd_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})

setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.ggomnbd.no.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){
  # Also model optimization settings should go here

  # Only add LL function args, everything else is prepared already, incl. start parameters

  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = ggomnbd_nocov_LL_sum,
                                 LL.function.ind = ggomnbd_nocov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,

                                 # parameter ordering for the callLL interlayer
                                 LL.params.names.ordered = c(log.r = "log.r",log.alpha =  "log.alpha", log.b = "log.b", log.s = "log.s", log.beta = "log.beta")),
                            keep.null = TRUE)
  return(optimx.args)
})

#' @include all_generics.R
#' @importFrom stats integrate
setMethod("clv.model.expectation", signature(clv.model="clv.model.ggomnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  r <- alpha <- beta <- b <- s <- t_i <- tau <- NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]
  params_i[, r     := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha := clv.fitted@prediction.params.model[["alpha"]]]
  params_i[, beta  := clv.fitted@prediction.params.model[["beta"]]]
  params_i[, b     := clv.fitted@prediction.params.model[["b"]]]
  params_i[, s     := clv.fitted@prediction.params.model[["s"]]]

  fct.ggomnbd.expectation <- function(r, alpha, beta, b, s, t_i){

    term1 <- (r/alpha)
    term2 <- (beta/(beta+exp(b*t_i)-1))^s*t_i
    term3 <- b*s*beta^s
    term4 <- integrate(f = function(tau){tau*exp(b*tau)*(beta + exp(b*tau)-1)^(-(s+1))}, lower = 0, upper = t_i)$value

    return(term1 * (term2 + (term3 * term4)))
  }

  fct.expectation <- function(params_i.t){
    return(params_i.t[,.(res = fct.ggomnbd.expectation(r = r, alpha = alpha, beta = beta, b = b, s = s, t_i = t_i)), by="Id"]$res)
  }

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.ggomnbd.no.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  r <- alpha <- b <- s <- beta <- x <- t.x <- T.cal <- PAlive <- DERT <- CET <- period.length <- NULL

  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  predict.number.of.periods <- dt.prediction[1, period.length]

  # Add CET
  dt.prediction[, CET := ggomnbd_nocov_CET(vEstimated_params = clv.fitted@prediction.params.model,
                                           dPrediction_period = predict.number.of.periods,
                                           vX = clv.fitted@cbs[, x],
                                           vT_x = clv.fitted@cbs[, t.x],
                                           vT_cal = clv.fitted@cbs[, T.cal])]


  # Add PAlive
  dt.prediction[, PAlive := ggomnbd_nocov_PAlive(vEstimated_params = clv.fitted@prediction.params.model,
                                                 vX = clv.fitted@cbs[, x],
                                                 vT_x = clv.fitted@cbs[, t.x],
                                                 vT_cal = clv.fitted@cbs[, T.cal])]
  # Add DERT
  dt.prediction[, DERT := 0]

  return(dt.prediction)
})

# .clv.model.vcov.jacobi.diag --------------------------------------------------------------------------------------------------------

setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.ggomnbd.no.cov"), definition = function(clv.model, clv.fitted, prefixed.params){
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