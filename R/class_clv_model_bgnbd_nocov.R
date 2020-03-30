# Class --------------------------------------------------------------------------------------------------------------------------------
#' @importFrom methods setClass
#' @include all_generics.R class_clv_model_basestrategy.R
setClass(Class = "clv.model.bgnbd.no.cov", contains = "clv.model",
         slots = list(),
         prototype = list(
           name.model = "BG/NBD Standard",
           names.original.params.model = c(r="r", alpha="alpha", a="a", b="b"),
           names.prefixed.params.model = c("log.r", "log.alpha", "log.a", "log.b"),
           start.params.model = c(r=1, alpha = 3, a = 1, b = 3)
         ))


# Methods --------------------------------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){
# Have to be > 0 as will be logged
if(any(start.params.model <= 0))
  check_err_msg(err.msg = "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")

if(length(list(...)) > 0)
  warning("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)
})

setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose, ...){
  # nothing to put specifically for this model
  return(clv.fitted)
})

#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})

#' @importFrom stats setNames
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})

setMethod(f = "clv.model.put.newdata", signature = signature(clv.model = "clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose){
  # clv.data in clv.fitted is already replaced with newdata here
  # Need to only redo cbs if given new data
  clv.fitted@cbs <- bgnbd_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})

setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){
  # Also model optimization settings should go here

  # Only add LL function args, everything else is prepared already, incl. start parameters

  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = bgnbd_nocov_LL_sum,
                                 LL.function.ind = bgnbd_nocov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,

                                 # parameter ordering for the callLL interlayer
                                 #** TODO: Hardcode from cpp interface
                                 LL.params.names.ordered = c(log.r = "log.r",log.alpha =  "log.alpha", log.a = "log.a", log.b = "log.b")),
                            keep.null = TRUE)
  return(optimx.args)
})

#' @include all_generics.R
setMethod("clv.model.expectation", signature(clv.model="clv.model.bgnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  r <- alpha_i <- a_i <- b_i <- date.first.repeat.trans<- date.first.actual.trans <- T.cal <- t_i<- period.first.trans<-NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  params_i[, r       := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha := clv.fitted@prediction.params.model[["alpha"]]]
  params_i[, a       := clv.fitted@prediction.params.model[["a"]]]
  params_i[, b  := clv.fitted@prediction.params.model[["b"]]]

  fct.bgnbd.expectation <- function(r, alpha, a, b, t){
    term1 = (a + b - 1)/(a - 1)
    term2 = (alpha/(alpha + t))^r
    term3 = vec_gsl_hyp2f1_e(r, b, a + b - 1, t/(alpha + t))$value

    return(term1 * (1 - term2 * term3))
  }

  # To caluclate expectation at point t for customers alive in t, given in params_i.t
  fct.expectation <- function(params_i.t) {

    return(params_i.t[,.(res = fct.bgnbd.expectation(r = r, alpha = alpha, a = a, b = b, t = t_i)), by = "Id"]$res)
  }


  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.bgnbd.no.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  #Id <- x <- t.x <- T.cal <-  PAlive <- CET <- DERT.R <- DERT.cpp <- NULL # cran silence

  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  predict.number.of.periods <- dt.prediction[1, period.length]
  # pass matrix(0) because no covariates are used


  # Put params together in single vec
  estimated.params <- c(r = clv.fitted@prediction.params.model[["r"]], alpha = clv.fitted@prediction.params.model[["alpha"]],
                        a = clv.fitted@prediction.params.model[["a"]], b  = clv.fitted@prediction.params.model[["b"]])


  # Add CET
  dt.prediction[, CET := bgnbd_cet(r = estimated.params[["r"]],
                                   alpha = estimated.params[["alpha"]],
                                   a = estimated.params[["a"]],
                                   b = estimated.params[["b"]],
                                   nPeriods = predict.number.of.periods,
                                   vX = clv.fitted@cbs[, x],
                                   vT_x = clv.fitted@cbs[, t.x],
                                   vT_cal = clv.fitted@cbs[, T.cal])]


  # Add PAlive
  dt.prediction[, PAlive := bgnbd_palive(r = estimated.params[["r"]],
                                         alpha = estimated.params[["alpha"]],
                                         a = estimated.params[["a"]],
                                         b = estimated.params[["b"]],
                                         vX = clv.fitted@cbs[, x],
                                         vT_x = clv.fitted@cbs[, t.x],
                                         vT_cal = clv.fitted@cbs[, T.cal])]
  # Add DERT
  dt.prediction[, DERT := 0]

  return(dt.prediction)
})

# .clv.model.vcov.jacobi.diag --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, prefixed.params){
  return(NULL)
})
