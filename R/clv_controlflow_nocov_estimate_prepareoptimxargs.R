# Put together the individual parts needed to call optimx
# Adding the variables needed to call the LL function is left to the model-specific optimizeLL functions as they are unknonwn at this point
setMethod("clv.controlflow.estimate.prepare.optimx.args", signature = signature(obj="clv.fitted"),
          def=function(obj, start.params.all){

  # Start with model defaults
  optimx.args <- obj@clv.model@optimx.defaults

  # Everything to call optimx and the interlayer manager
  optimx.args <- modifyList(optimx.args, list(fn            = interlayer_manager,
                                              par           = start.params.all,
                                              hessian       = TRUE),
                            keep.null = TRUE)

  # Forbid to use any covariate specific interlayers ---------------------------------------------------
  #   For no covariates objects, only the correlation interlayer can be used. For covariates objects,
  #     this functions is overwritten to prepare more args
  #
  #   However, not passing these parameters, results in missing parameters for the interlayer manager
  #   This could be handled by default parameters or with missing there,
  #   but passing them with "False" is much cleaner


  optimx.args <- modifyList(optimx.args, list(use.interlayer.constr        = FALSE,
                                              names.original.params.constr = character(0),
                                              names.prefixed.params.constr = character(0),

                                              use.interlayer.reg           = FALSE,
                                              reg.lambda.trans             = numeric(0),
                                              reg.lambda.life              = numeric(0),
                                              names.prefixed.params.after.constr.life  = character(0),
                                              names.prefixed.params.after.constr.trans = character(0)),
                            keep.null = TRUE)


  # Everything to call the correlation layer
  optimx.args <- modifyList(optimx.args, list(use.cor                   = obj@estimation.used.correlation,
                                              name.prefixed.cor.param.m = obj@name.prefixed.cor.param.m,
                                              # By default, always check the bounds of param m
                                              check.param.m.bounds      = TRUE),
                            keep.null = TRUE)

  # Correlation interlayer ---------------------------------------------------------------------
  if(obj@estimation.used.correlation){
    # Use NM as default if correlation is estimated because the interlayer may return Inf
    #   if the params are out-of-bound
    optimx.args <- modifyList(optimx.args, list(method = "Nelder-Mead"))

    # Use a custom gradient function that signals the correlation layer to
    #   not check the boundaries of param m
    # Otherwise, the Hessian likely contains NAs because numDeriv often,
    #   also with small stepsizes, wanders accross the boundaries
    # Not checking the boundaries of param m is no issue for the gradient and hessian only,
    #   the bound checks are enforced for the param during regular optimization evaluations

    # Custom Gradient function
    # Use optixm::grad because also used in optimx::optimx.setup:
    #   "ugr <- function(par, userfn = ufn, ...) { tryg <- grad(userfn, par, ...)}"
    fct.no.check.grad <- function(x, fn.to.call.from.gr, ...){
      # ... contains all the other arguments given to interlayer(s) and LL function
      # fn.to.call.from.gr is what optimx(fn) calls (ie the interlayer manager)
      all.other.args <- list(...)
      all.other.args <- modifyList(all.other.args,
                                   # dont check boundaries during gradient
                                   alist(check.param.m.bounds = FALSE))
      do.call(what=grnd, c(alist(par = x,
                                 userfn = fn.to.call.from.gr),
                           all.other.args))
    }

    # For the gradient, call the wrapper around optmix::grnd
    optimx.args <- modifyList(optimx.args,
                              list(gr= fct.no.check.grad,
                                   # function to call when doing numerical grad. Do whatever is done for optimx
                                   fn.to.call.from.gr = optimx.args$fn))
  }


  return(optimx.args)
})
