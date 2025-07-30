# . clv.controlflow.estimate.check.inputs ------------------------------------------------------------------------
setMethod(f = "clv.controlflow.estimate.check.inputs", signature = signature(clv.fitted="clv.fitted"), definition = function(clv.fitted,  start.params.model, optimx.args, verbose, ...){

  # Check only basic structure
  err.msg <- c()

  if(!is.null(start.params.model)){
    # may be NULL = use model default
    err.msg <- c(err.msg, check_user_data_startparams(start.params = start.params.model,
                                                      vector.names = clv.fitted@clv.model@names.original.params.model,
                                                      param.names = "model start parameter"))
  }


  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name ="verbose"))

  # Check additional optimx args
  err.msg <- c(err.msg, check_user_data_optimxargs(optimx.args=optimx.args))
  check_err_msg(err.msg)
})

# . clv.controlflow.estimate.put.inputs ------------------------------------------------------------------------
setMethod("clv.controlflow.estimate.put.inputs", signature =  signature(clv.fitted="clv.fitted"), definition = function(clv.fitted, start.params.model, optimx.args, verbose, ...){
  clv.fitted@model.specification.args <- list(start.params.model=start.params.model, optimx.args=optimx.args, verbose=verbose)
  return(clv.fitted)
})



# . clv.controlflow.estimate.generate.start.params ------------------------------------------------------------------------
setMethod("clv.controlflow.estimate.generate.start.params", signature = signature(clv.fitted="clv.fitted"), definition = function(clv.fitted, start.params.model, verbose, start.param.cor, ...){

  # Model params
  if(is.null(start.params.model)){
    untransformed.start.params.model <- setNames(clv.fitted@clv.model@start.params.model, clv.fitted@clv.model@names.original.params.model)
  }else{
    untransformed.start.params.model <- start.params.model[clv.fitted@clv.model@names.original.params.model] # ensure order
  }

  transformed.start.params.model <- clv.model.transform.start.params.model(clv.model = clv.fitted@clv.model,
                                                                           original.start.params.model = untransformed.start.params.model)
  names(transformed.start.params.model) <- clv.fitted@clv.model@names.prefixed.params.model

  if(clv.model.supports.correlation(clv.fitted@clv.model)){
    # If the model supports correlation, start.param.cor is passed, otherwise not
    transformed.start.params.model <- clv.model.generate.start.param.cor(clv.model=clv.fitted@clv.model, start.param.cor=start.param.cor, transformed.start.params.model=transformed.start.params.model)
  }

  return(transformed.start.params.model)
})


# . clv.controlflow.estimate.prepare.optimx.args ------------------------------------------------------------------------
# Put together the individual parts needed to call optimx
#   Adding the variables needed to call the LL function is left to the model-specific optimizeLL functions as they are unknonwn at this point
#' @importFrom utils modifyList
#' @importFrom numDeriv hessian
setMethod("clv.controlflow.estimate.prepare.optimx.args", signature = signature(clv.fitted="clv.fitted"), def=function(clv.fitted, start.params.all){

  # Start with model defaults
  optimx.args <- clv.fitted@clv.model@optimx.defaults

  # Everything to call optimx and the interlayer manager
  optimx.args <- modifyList(optimx.args, list(fn            = interlayer_manager,
                                              par           = start.params.all,
                                              hessian       = TRUE),
                            keep.null = TRUE)

  # By default dont use any covariate specific interlayers ---------------------------------------------------
  #   For no covariates objects only the correlation interlayer can be used
  #
  #   However, not passing the parameters for the other interlayers results in missing parameters for
  #   the interlayer manager. This could be handled by using default parameters or with missing there,
  #   but passing them with "False" is much cleaner

  optimx.args <- modifyList(optimx.args, list(use.interlayer.constr        = FALSE,
                                              names.original.params.constr = character(0),
                                              names.prefixed.params.constr = character(0),

                                              use.interlayer.reg           = FALSE,
                                              reg.weight.trans             = numeric(0),
                                              reg.weight.life              = numeric(0),
                                              names.prefixed.params.after.constr.life  = character(0),
                                              names.prefixed.params.after.constr.trans = character(0)),
                            keep.null = TRUE)


  # Default is no correlation
  optimx.args <- modifyList(optimx.args, list(use.cor                   = FALSE,
                                              name.prefixed.cor.param.m = character(0),
                                              # By default, always check the bounds of param m
                                              check.param.m.bounds      = TRUE),
                            keep.null = TRUE)

  # Correlation interlayer ---------------------------------------------------------------------
  # Only turn on if needed
  if(clv.model.estimation.used.correlation(clv.fitted@clv.model)){

    optimx.args <- modifyList(optimx.args, list(use.cor                   = TRUE,
                                                name.prefixed.cor.param.m = clv.fitted@clv.model@name.prefixed.cor.param.m,
                                                # By default, always check the bounds of param m
                                                check.param.m.bounds      = TRUE),
                              keep.null = TRUE)

    # Use NM as default if correlation is estimated because the interlayer may return Inf
    #   if the params are out-of-bound
    optimx.args <- modifyList(optimx.args, list(method = "Nelder-Mead"))


    # . Custom hessian to set `check.param.m.bounds=FALSE` ---------------------
    # When calculating the hessian for a model with correlation, we do not want
    # to check the boundaries of param m that happen in the correlation
    # interlayer (`interlayer_correlation`). The Hessian otherways may contain
    # NAs because numDeriv often also with small stepsizes, wanders across the
    # boundaries. Therefore, the interlayer has to be called with
    # `check.param.m.bounds=FALSE` during the calculating of the hessian while
    # it has to be called with `check.param.m.bounds=TRUE` during the normal
    # optimization procedure.
    # To set `check.param.m.bounds=FALSE` during the calculation of the hessian,
    # we pass our own function for calculating the hessian. It should call the
    # same procedure as optimx with the same arguments except for setting
    # `check.param.m.bounds=FALSE`.
    # optimx (2023-10.21) internally uses `numDeriv::hessian` to derive the
    # hessian if only the objective function is supplied. We have no analytical
    # expression of the hessian or the gradient and hence for all models without
    # correlation `numDeriv::hessian` is used by optimx. This was numerically
    # verified. We therefore also use `numDeriv::hessian` here.
    # Earlier, we used optimx::grnd and passed it to optimx(gr=) because it is
    # what is used in optimx::optimx.setup.

    fn.no.check.hess <- function(x, fn.to.call.from.hess, ...){
      # This method is passed to optimx(hess=) and is called to calculate the
      # hessian. For this method `fn.no.check.hess` to know the actual target
      # function it has to be given as parameter to `fn.no.check.hess`.
      # param `...` contains all the other arguments to be given to the original
      # target function (the LL).

      all.other.args <- list(...)
      # This is the actual only change we want to make: Dont check boundaries
      # when calculating the hessian
      all.other.args$check.param.m.bounds <- FALSE

      return(do.call(
        what=numDeriv::hessian,
        args = c(alist(x = x, func = fn.to.call.from.hess), all.other.args)
      ))
    }

    # use `fn.no.check.hess` to calculate the hessian
    optimx.args$hess <- fn.no.check.hess
    # the actual loss function (LL interlayers) which should be called inside
    # `fn.no.check.hess`. Pass it as additional arg to optimx and it will
    # eventually be used in `fn.no.check.hess`
    optimx.args$fn.to.call.from.hess <- optimx.args$fn


    # . Custom gradient to set `check.param.m.bounds=FALSE` --------------------
    # Similarly as for the hessian, also a custom function for the gradient has
    # to be passed to optimx in order to set `check.param.m.bounds=FALSE` when
    # optimx calculates the gradient at the end of the optimization for hessian
    # and KKT.
    # For explanations how this works, see the comments for `fn.no.check.hess`.
    fn.no.check.grad <- function(x, fn.to.call.from.grad, ...){
      all.other.args <- list(...)
      all.other.args$check.param.m.bounds <- FALSE
      # optimx (2023-10.21) uses `numDeriv::grad()` if no gradient is given by
      # the user
      return(do.call(
        what=numDeriv::grad,
        args = c(alist(x = x, func = fn.to.call.from.grad), all.other.args)
      ))
    }

    # use `fn.no.check.grad` to calculate the gradient
    optimx.args$gr <- fn.no.check.grad
    # the actual loss function (LL interlayers) to call inside
    # `fn.no.check.hess` to calculate the gradient
    optimx.args$fn.to.call.from.grad <- optimx.args$fn

  }

  return(optimx.args)
})


# . clv.controlflow.estimate.process.post.estimation ------------------------------------------------------------------------
#' @importFrom optimx coef<-
#' @importFrom utils tail
setMethod(f = "clv.controlflow.estimate.process.post.estimation", signature = signature(clv.fitted="clv.fitted"), definition = function(clv.fitted, res.optimx){

  clv.fitted@optimx.estimation.output <- res.optimx

  optimx.last.row <- tail(clv.fitted@optimx.estimation.output, n=1)

  if(anyNA(coef(optimx.last.row))){
    warning(paste0("Estimation failed with NA coefficients. The returned object contains results but further usage is restricted.",
                  " You might want to try to fit the model again with method Nelder-Mead (using optimx.args=list(method=\"Nelder-Mead\")) or try different starting values. See examples."),
            immediate. = TRUE, call. = FALSE)
  }

  # extract hessian from "details" attribute which is a list (if more then 1 method given)
  #   name it the same as the coefs for reading out later on
  clv.fitted@optimx.hessian <- as.matrix(tail(attr(optimx.last.row, "details")[, "nhatend"], n=1)[[1]])

  if(length(clv.fitted@optimx.hessian)==1 & all(is.na(clv.fitted@optimx.hessian))){
    clv.fitted@optimx.hessian <- matrix(data = NA_real_, nrow = ncol(coef(optimx.last.row)),
                                 ncol = ncol(coef(optimx.last.row)))
    warning("Hessian could not be derived. Setting all entries to NA.",
            call. = FALSE, immediate. = TRUE)
  }

  colnames(clv.fitted@optimx.hessian) <- rownames(clv.fitted@optimx.hessian) <- colnames(tail(coef(res.optimx), n=1))

  return(clv.fitted)
})
