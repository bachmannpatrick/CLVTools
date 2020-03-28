#' @include class_clv_fitted.R all_generics.R
#' @importFrom optimx optimx coef<-
#' @importFrom utils modifyList
#' @include all_generics.R
clv.template.controlflow.estimate <- function(obj,
                                              cl,
                                              start.params.model,
                                              use.cor,
                                              start.param.cor,
                                              optimx.args,
                                              verbose,
                                              ...){

  # Input for covariate models, passed in "..."
  # function(obj,
  #          cl,
  #          start.params.model=c(),
  #          use.cor = FALSE,
  #          start.param.cor=c(),
  #          optimx.args=list(),
  #          verbose = TRUE,
  #          ... =
  #          names.cov.life=c(), names.cov.trans=c(),
  #          start.params.life=c(), start.params.trans=c(),
  #          names.cov.constr=c(),
  #          start.params.constr=c(),
  #          reg.lambdas = c())


  # input checks ------------------------------------------------------------------------------------------
  #   checks for model first
  clv.controlflow.estimate.check.inputs(obj=obj, start.params.model=start.params.model, use.cor=use.cor, start.param.cor=start.param.cor,
                                        optimx.args=optimx.args, verbose=verbose, ...)

  clv.model.check.input.args(clv.model=obj@clv.model, clv.fitted=obj, start.params.model=start.params.model, use.cor=use.cor, start.param.cor=start.param.cor,
                             optimx.args=optimx.args, verbose=verbose, ...)


  # Store user input for estimation ----------------------------------------------------------------------------
  obj <- clv.controlflow.estimate.put.inputs(obj=obj, cl=cl, use.cor=use.cor, start.param.cor=start.param.cor, ...)
  obj <- clv.model.put.estimation.input(clv.model=obj@clv.model, clv.fitted=obj, verbose=verbose, ...)


  # Generate start params ---------------------------------------------------------------------------------------
  start.params.all <- clv.controlflow.estimate.generate.start.params(obj=obj, start.params.model=start.params.model, start.param.cor=start.param.cor, verbose=verbose, ...)


  # prepare optimx args ------------------------------------------------------------------------------------------
  #   model needed to prepare LL part of optimx args
  prepared.optimx.args <- clv.controlflow.estimate.prepare.optimx.args(obj=obj, start.params.all=start.params.all)
  prepared.optimx.args <- clv.model.prepare.optimx.args(clv.model=obj@clv.model, clv.fitted=obj, prepared.optimx.args=prepared.optimx.args)

  # No matter what the (model) defaults, the user arguments are written ontop of what is generated
  prepared.optimx.args <- modifyList(prepared.optimx.args, optimx.args, keep.null = FALSE)


  # optimize LL --------------------------------------------------------------------------------------------------
  #   Just call optimx. Nothing model specific or similar is done.
  if(verbose)
    message("Starting estimation...")

  res.optimx <- do.call(what = optimx::optimx, args = prepared.optimx.args)

  if(verbose)
    message("Estimation finished!")

  # **FUTURE only: Extract results ---------------------------------------------------------------------------------
  # Extract results needed to build the final object
  #   the final object is in a separate builder function because it might / will be exported to use
  #     together with prepare.only=T
  #   could move to separate function because of possible mcmc estimation (clv.controlflow.post.estimation.steps())
  # Extracted: last used method's Hessian + coefs, kkt, LL value, ...

  # # dont call controlflow.xx because possibly exported
  # clv.post.optimization.processing(clv.pre.fitted, estimated.coefs, hessian=NULL)
  # clv.post.fitting.processing(clv.pre.fitted, estimated.coefs, hessian=NULL)
  #   - set prediction parameters
  #       need to be set already to to some of the post estimation process steps such as running the LL again
  #   - model post processing steps

  # Store results ------------------------------------------------------------------------------------------------
  #   also model-specifc storage and processing of optimx outputs
  obj <- clv.controlflow.estimate.put.optimx(obj=obj, res.optimx=res.optimx)
  obj <- clv.model.put.optimx.output(clv.model=obj@clv.model, clv.fitted=obj, res.optimx=res.optimx)

  return(obj)
}


