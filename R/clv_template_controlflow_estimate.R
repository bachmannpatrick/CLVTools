clv.template.controlflow.estimate <- function(clv.fitted,
                                              start.params.model,
                                              optimx.args,
                                              verbose,
                                              ...){

  # input checks ------------------------------------------------------------------------------------------
  #   checks for model first
  clv.controlflow.estimate.check.inputs(clv.fitted=clv.fitted, start.params.model=start.params.model,
                                        optimx.args=optimx.args, verbose=verbose, ...)

  clv.model.check.input.args(clv.model=clv.fitted@clv.model, clv.fitted=clv.fitted, start.params.model=start.params.model,
                             optimx.args=optimx.args, verbose=verbose, ...)


  # Store user input for estimation ----------------------------------------------------------------------------
  clv.fitted           <- clv.controlflow.estimate.put.inputs(clv.fitted=clv.fitted, verbose=verbose, ...)
  clv.fitted@clv.model <- clv.model.put.estimation.input(clv.model=clv.fitted@clv.model, ...)


  # Generate start params ---------------------------------------------------------------------------------------
  start.params.all <- clv.controlflow.estimate.generate.start.params(clv.fitted=clv.fitted, start.params.model=start.params.model, verbose=verbose, ...)


  # prepare optimx args ------------------------------------------------------------------------------------------
  #   model needed to prepare LL part of optimx args
  prepared.optimx.args <- clv.controlflow.estimate.prepare.optimx.args(clv.fitted=clv.fitted, start.params.all=start.params.all)
  prepared.optimx.args <- clv.model.prepare.optimx.args(clv.model=clv.fitted@clv.model, clv.fitted=clv.fitted, prepared.optimx.args=prepared.optimx.args)

  # No matter what the (model) defaults, the user arguments are written ontop of what is generated
  prepared.optimx.args <- modifyList(prepared.optimx.args, optimx.args, keep.null = FALSE)


  # optimize LL --------------------------------------------------------------------------------------------------
  #   Just call optimx. Nothing model specific or similar is done.
  if(verbose)
    message("Starting estimation...")

  res.optimx <- do.call(what = optimx, args = prepared.optimx.args)

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
  clv.fitted <- clv.controlflow.estimate.process.post.estimation(clv.fitted=clv.fitted, res.optimx=res.optimx)
  clv.fitted <- clv.model.process.post.estimation(clv.model=clv.fitted@clv.model, clv.fitted=clv.fitted, res.optimx=res.optimx)

  # Set prediction params from coef()
  #   Do after process.post.estimate because needs optimx result stored
  #   Needed for predict/plot/fitted (incl when processing newdata) but set here already instead of in every of these
  clv.fitted <- clv.controlflow.predict.set.prediction.params(clv.fitted=clv.fitted)

  return(clv.fitted)
}


