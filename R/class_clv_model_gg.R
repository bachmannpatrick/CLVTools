#' CLV Model functionality for Gamma/Gamma model
#'
#' This class implements the functionalities and model-specific steps which are required
#' to fit the Gamma/Gamma model without covariates.
#'
#' @keywords internal
#' @importFrom methods setClass
#' @seealso Other clv model classes \link{clv.model-class}, \link{clv.model.bgnbd.static.cov-class}
#' @seealso Classes using its instance: \link{clv.fitted-class}
#' @include all_generics.R class_clv_model.R
setClass(Class = "clv.model.gg", contains = "clv.model",
         slots = list(),
         prototype = list(
           name.model = character(),
           names.original.params.model = character(0),
           names.prefixed.params.model = character(0),
           start.params.model = numeric(0),
           optimx.defaults = list()
         ))

#' @importFrom methods new
clv.model.gg <- function(){
  return(new("clv.model.gg",
             name.model = "Gamma/Gamma",
             names.original.params.model = c(p = "p", q = "q", gamma = "gamma"),
             names.prefixed.params.model = c("log.p", "log.q", "log.gamma"),
             start.params.model = c(p = 1, q = 1, gamma = 1),
             optimx.defaults = list(method = "L-BFGS-B",
                                    # lower   = c(log(1*10^(-5)),log(1*10^(-5)),log(1*10^(-5)),log(1*10^(-5))),
                                    # upper   = c(log(300),log(2000),log(300),log(2000)),
                                    itnmax  = 3000,
                                    control = list(
                                      save.failures = TRUE,
                                      # Do not perform starttests because it checks the scales with max(logpar)-min(logpar)
                                      #   but all standard start parameters are <= 0, hence there are no logpars what
                                      #   produces a warning
                                      starttests = FALSE))))
}

# Methods --------------------------------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){

  err.msg <- c()

  # Have to be > 0 as will be logged
  if(any(start.params.model <= 0)){
    err.msg <- c(err.msg, "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")
  }


  if(use.cor){
    err.msg <- c(err.msg, "Correlation is not supported for the Gamma/Gamma model")
  }

  if(length(list(...)) > 0){
    stop("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)
  }

  check_err_msg(err.msg)

})

# .clv.model.put.estimation.input --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, clv.fitted, verbose, ...){
  # nothing to put specifically for this model
  return(clv.fitted)
})

# .clv.model.transform.start.params.model --------------------------------------------------------------------------------------------------------
#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})

# .clv.model.backtransform.estimated.params.model --------------------------------------------------------------------------------------------------------
#' @importFrom stats setNames
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})

# .clv.model.prepare.optimx.args --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){

  # Only add LL function args, everything else is prepared already, incl. start parameters
  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = gg_LL,
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vM_x   = clv.fitted@cbs$Spending,
                                 # parameter ordering for the callLL interlayer
                                 LL.params.names.ordered = c(log.p="log.p", log.q="log.q",
                                                             log.gamma="log.gamma")),
                            keep.null = TRUE)

  return(optimx.args)
})

# . clv.model.process.post.estimation -----------------------------------------------------------------------------------------
setMethod("clv.model.process.post.estimation", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, clv.fitted, res.optimx){
  # No additional step needed (ie store model specific stuff, extra process)
  return(clv.fitted)
})

# .clv.model.put.newdata --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.put.newdata", signature = signature(clv.model = "clv.model.gg"), definition = function(clv.model, clv.fitted, verbose){
  # clv.data in clv.fitted is already replaced with newdata here
  # Need to only redo cbs if given new data
  clv.fitted@cbs <- gg_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})

# . clv.model.expectation --------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod("clv.model.expectation", signature(clv.model="clv.model.gg"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  return(dt.expectation.seq)
})

# .clv.model.predict.clv --------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.gg"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  p <- q <- gamma <- x <- Spending <- cbs.x <- cbs.Spending <- DERT <- NULL

  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  predict.number.of.periods <- dt.prediction[1, period.length]

  dt.prediction[clv.fitted@cbs, cbs.x := i.x, on="Id"]
  dt.prediction[clv.fitted@cbs, cbs.Spending := i.Spending, on="Id"]
  dt.prediction[, predicted.Spending := (gamma + cbs.Spending * cbs.x) * p/(p * cbs.x + q - 1)]
  dt.prediction[, cbs.x        := NULL]
  dt.prediction[, cbs.Spending := NULL]
  dt.prediction[, PAlive := 0]
  dt.prediction[, CET := 0]
  dt.prediction[, DERT := 0]

  # Calculate CLV
  if("DERT" %in% colnames(dt.prediction))
    dt.prediction[, predicted.CLV := DERT * predicted.Spending]
  if("DECT" %in% colnames(dt.prediction))
    dt.prediction[, predicted.CLV := DECT * predicted.Spending]

  return(dt.prediction)
})

# .clv.model.vcov.jacobi.diag --------------------------------------------------------------------------------------------------------

setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, clv.fitted, prefixed.params){
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
