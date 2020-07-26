#' CLV Model functionality for the Gamma-Gamma spending model
#'
#' This class implements the functionalities and model-specific steps which are required
#' to fit the Gamma-Gamma spending model.
#'
#' @importFrom methods setClass
#' @keywords internal
#' @include all_generics.R class_clv_model_nocorrelation.R
setClass(Class = "clv.model.gg", contains = "clv.model.no.correlation")

#' @importFrom methods new
clv.model.gg <- function(){

  return(new("clv.model.gg",
             name.model                  = "Gamma-Gamma",
             names.original.params.model = c(p="p", q="q", gamma="gamma"),
             names.prefixed.params.model = c(log.p="log.p", log.q="log.q", log.gamma="log.gamma"),
             start.params.model          = c(p=1, q=1, gamma=1),
             optimx.defaults = list(method = "L-BFGS-B",
                                    itnmax  = 3000,
                                    # upper  = c(log(10000),log(10000),log(10000)),
                                    # lower  = c(log(0),log(0),log(0)),
                                    control = list(
                                      kkt = TRUE,
                                      save.failures = TRUE,
                                      # Do not perform starttests because it checks the scales with max(logpar)-min(logpar)
                                      #   but all standard start parameters are <= 0, hence there are no logpars what
                                      #   produces a warning
                                      starttests = FALSE))))
}

# Methods --------------------------------------------------------------------------------------------------------------------------------

# .clv.model.check.input.args -----------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, clv.fitted, start.params.model, optimx.args, verbose, ...){
  err.msg <- c()
  # Have to be > 0 as will be logged
  if(any(start.params.model <= 0))
    err.msg <- c(err.msg, "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")

  check_err_msg(err.msg)
})

#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})

# .clv.model.backtransform.estimated.params.model --------------------------------------------------------------------------------------------------------
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})


# .clv.model.prepare.optimx.args --------------------------------------------------------------------------------------------------------
#' @importFrom utils modifyList
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, clv.fitted, prepared.optimx.args){

  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = gg_LL,
                                 LL.function.ind = NULL,
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

# clv.model.process.newdata --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.process.newdata", signature = signature(clv.model = "clv.model.gg"), definition = function(clv.model, clv.fitted, verbose){

  # clv.data in clv.fitted is already replaced with newdata here
  # Only need to redo cbs if new data is given

  clv.fitted@cbs <- gg_cbs(clv.data = clv.fitted@clv.data, remove.first.transaction = clv.fitted@estimation.removed.first.transaction)
  return(clv.fitted)
})



# .clv.model.predict -------------------------------------------------------------------------------------------------------------------
setMethod("clv.model.predict", signature(clv.model="clv.model.gg"), function(clv.model, clv.fitted, dt.predictions, verbose, ...){
  cbs.x <- cbs.Spending <- i.Spending <- i.x <- predicted.mean.spending <- NULL

  p     <- clv.fitted@prediction.params.model[["p"]]
  q     <- clv.fitted@prediction.params.model[["q"]]
  gamma <- clv.fitted@prediction.params.model[["gamma"]]

  # Predict spending
  #   add data from cbs by Id to ensure matching
  dt.predictions[clv.fitted@cbs, cbs.x := i.x,               on="Id"]
  dt.predictions[clv.fitted@cbs, cbs.Spending := i.Spending, on="Id"]
  dt.predictions[, predicted.mean.spending := (gamma + cbs.Spending * cbs.x) * p/(p * cbs.x + q - 1)]
  dt.predictions[, cbs.x        := NULL]
  dt.predictions[, cbs.Spending := NULL]

  return(dt.predictions)
})


# .clv.model.vcov.jacobi.diag --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, clv.fitted, prefixed.params){

  # Jeff:
  # Delta method:
  #   h=(log(t),log(t),log(t),log(t),t,t,t)
  #   g=h^-1=(exp(t),exp(t),exp(t),exp(t),t,t,t)
  #   Deltaexp = g' = (exp(t),exp(t),exp(t),exp(t),1,1,1)

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

# .clv.model.probability.density -------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.probability.density", signature = signature(clv.model="clv.model.gg"), definition = function(clv.model, x, clv.fitted){
  a1 <- a2 <- a3 <- a4 <- a5 <- g1 <- x1 <- NULL

  cbs <- copy(clv.fitted@cbs[x>0,])

  setnames(cbs, "x", "x1")

  p <- coef(clv.fitted)["p"]
  q <- coef(clv.fitted)["q"]
  gamma <- coef(clv.fitted)["gamma"]

  results <- sapply(x, function(zbar){
    cbs[,a1 := lgamma(p*x1+q)-lgamma(p*x1)-lgamma(q)]
    cbs[,a2 := q*log(gamma)]
    cbs[,a3 := (p*x1-1)*log(zbar)]
    cbs[,a4 := (p*x1)*log(x1)]
    cbs[,a5 := (p*x1+q)*log(gamma+x1*zbar)]
    cbs[,g1 := exp(a1+a2+a3+a4-a5)]
    return(cbs[,mean(g1)])
  })

  return(results)
})
