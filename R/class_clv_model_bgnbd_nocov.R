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

setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){
  # Also model optimization settings should go here
  #parameters <- bgnbd.EstimateParameters(clv.fitted@cbs , clv.model@start.params.model)

  # Only add LL function args, everything else is prepared already, incl. start parameters
  #return(list(parameters))

  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = bgnbd.cbs.LL,
                                 LL.function.ind = bgnbd.LL, # if doing correlation
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

