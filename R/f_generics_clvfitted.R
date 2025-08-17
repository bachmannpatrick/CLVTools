# . clv.controlflow.plot.check.inputs ------------------------------------------------------------------------
setMethod("clv.controlflow.plot.check.inputs", signature(obj="clv.fitted"), function(obj, prediction.end, cumulative, plot, label.line, verbose){
  # Empty fallback method.
})

# . clv.controlflow.check.prediction.params -----------------------------------------------------------------
setMethod("clv.controlflow.check.prediction.params", signature = signature(clv.fitted = "clv.fitted"), function(clv.fitted){
  # Do not check coef() because correlation coef may be NA and can still predict
  if(anyNA(clv.fitted@prediction.params.model)){
    check_err_msg("Cannot proceed because there are NAs in the estimated model coefficients!")
  }
})

# . clv.controlflow.predict.set.prediction.params ------------------------------------------------------------------------
setMethod(f = "clv.controlflow.predict.set.prediction.params", signature = signature(clv.fitted="clv.fitted"), definition = function(clv.fitted){
  clv.fitted@prediction.params.model <- coef(clv.fitted)[clv.fitted@clv.model@names.original.params.model]
  return(clv.fitted)
})



# . clv.fitted.estimate.same.specification.on.new.data ------------------------------------------------------------------------
setMethod("clv.fitted.estimate.same.specification.on.new.data", signature = "clv.fitted", def = function(clv.fitted, newdata, ...){
  cl <- match.call(expand.dots = TRUE)

  # args to model function are the original specification args + `newdata` as
  # actual data arg
  args <- c(clv.fitted@model.specification.args, list(clv.data=newdata))

  # overwrite call args with what was passed
  args <- modifyList(args, val = list(...), keep.null = TRUE)

  new.fitted <- do.call(what = clv.fitted@clv.model@fn.model.generic, args=args)

  new.fitted@call <- cl
  return(new.fitted)
})


# . clv.fitted.get.LL ----------------------------------------------------------
#' @include all_generics.R
setGeneric("clv.fitted.get.LL", signature = "clv.fitted", def = function(clv.fitted){

  # Calling the LL with the exact same inputs/specification as when the fitting
  # is not trivial as there are plenty of options.
  # To reproduce it, the exact steps of the estimation are repeated here.

  # Start parameters are not really required, they are just stored as item
  # `par` for `optimx()`
  final.coefs <- drop(tail(coef(clv.fitted@optimx.estimation.output), n=1))

  prepared.optimx.args <- clv.controlflow.estimate.prepare.optimx.args(
    clv.fitted=clv.fitted,
    start.params.all= final.coefs)

  prepared.optimx.args <- clv.model.prepare.optimx.args(
    clv.model=clv.fitted@clv.model,
    clv.fitted=clv.fitted,
    prepared.optimx.args=prepared.optimx.args)

  prepared.optimx.args[["LL.param.names.to.optimx"]] <- names(prepared.optimx.args$par)

  # In the estimation procedure, the user can also supply custom `optimx.args`
  # which override `prepared.optimx.args` here. Because optimx is not called
  # here, there is no need to add `optimx.args` here.

  # The generated args also contain parameters for optimx. These are not required
  # for the LL and need to be removed. This also removes the LL itself (`fn`).
  names.optimx.args <- setdiff(formalArgs(optimx), "...")
  call.args <- prepared.optimx.args[!(names(prepared.optimx.args) %in% names.optimx.args)]

  # Could save memory as the returned method is a closure and has the environment
  # in which is was defined attached. Hence all variables in this method here
  # which may be large. However, it can also be useful to have these objects.

  # Wrapper to call the LL with the original args.
  # It is preferred to return a method rather than calling it immediately
  # because generating the call args may take time.
  LL <- function(params){
    req.names <- call.args$LL.param.names.to.optimx

    # Ensure named same as original
    if(!(identical(sort(names(params)), sort(req.names)))){
      check_err_msg(paste0(
        "'params' has to be named ",
        paste(req.names, collapse = ", "),
        ". Often, `drop(coef(model@optimx.estimation.output))` is useful."))
    }

    # Ensure name and position are the same as original order. This is required as
    # any param input will be re-named by slapping `LL.param.names.to.optimx` on
    # them (using names() <- ) in the first interlayer and then further accessed
    # by name.
    call.args$LL.params <- params[req.names] # bring to correct order
    return(do.call(what = prepared.optimx.args$fn, args = call.args))
  }

  return(LL)
})
