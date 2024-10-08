# . clv.controlflow.estimate.generate.start.params ------------------------------------------------------------------------------
#' @importFrom methods as
#' @importFrom methods callNextMethod
setMethod("clv.controlflow.estimate.generate.start.params", signature = signature(clv.fitted="clv.pnbd.dynamic.cov"),definition = function(clv.fitted,
                                                                                                                                    start.params.model,
                                                                                                                                    start.params.life,
                                                                                                                                    start.params.trans,
                                                                                                                                    start.params.constr,
                                                                                                                                    verbose,
                                                                                                                                    ...){
  # Before calling the parent (static.cov and nocov) start param generation methods
  #   set the model start params to nocov model coefs if the user did not provide
  #   any start params for the model

  # This has to be done here, and cannot be done in the model.prepare.optimx.args function
  #   because there the original estimation input is unknown
  #   ie it is not known if the user actually did supply start params or not
  #   Also, doing it as part of clv.fitted.transactions.dynamic.cov is not possible because the model
  #     to call (ie pnbd()) is unknown

  if(is.null(start.params.model)){
    # Generate start params from nocov model

    if(verbose)
      message("Generating model start parameters by fitting a no covariate pnbd model...")

    # Do optimization
    nocov.coefs <- tryCatch(coef(pnbd(clv.data = as(object = clv.fitted@clv.data, Class = "clv.data", strict = TRUE),
                                      start.params.model = c(r=0.5, alpha=15, s=0.5, beta=10),
                                      verbose = FALSE)),
                            error = function(e){stop(paste0("Failed to estimate a pnbd no covariate model: ",
                                                            e$message), call. = FALSE)})

    if(any(!is.finite(nocov.coefs)))
      stop("The parameters from the pnbd no covariate model yielded non finite values. Please revise your data!", call. = FALSE)

    if(verbose){
      coef.brakets <- paste0("(", paste(c("r", "alpha", "s", "beta"), "=", format(nocov.coefs, digits=4), collapse = ", ", sep=""), ")")
      message("Optimization of no covariate model found model start parameters ", coef.brakets)
    }

      # fake that the user input (start.params.model) was given as the params obtained here
      start.params.model <- nocov.coefs
  }

  # Continue with ordinary start parameter generation process
  return(callNextMethod())
})


# . clv.model.put.estimation.input ------------------------------------------------------------------------------------------------

setMethod(f = "clv.controlflow.estimate.put.inputs", signature = signature(clv.fitted="clv.pnbd.dynamic.cov"), definition = function(clv.fitted, start.params.model, optimx.args, verbose, use.cor, start.param.cor, ...){
  # Create walks - they are specific to the pnbd dyncov model
  clv.fitted <- callNextMethod()

  clv.fitted@model.specification.args <- c(clv.fitted@model.specification.args, list(
    use.cor=use.cor,
    start.param.cor=start.param.cor
  ))

  if(verbose)
    message("Creating walks...")

  l.walks <- pnbd_dyncov_createwalks(clv.data = clv.fitted@clv.data)

  clv.fitted@data.walks.life.aux    <- l.walks[["data.walks.life.aux"]]
  clv.fitted@data.walks.life.real   <- l.walks[["data.walks.life.real"]]
  clv.fitted@data.walks.trans.aux   <- l.walks[["data.walks.trans.aux"]]
  clv.fitted@data.walks.trans.real  <- l.walks[["data.walks.trans.real"]]

  if(verbose)
    message("Walks created.")

  return(clv.fitted)
})
