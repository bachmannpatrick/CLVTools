#' CLV Model functionality for PNBD with dynamic covariates
#'
#' This class implements the functionalities and model-specific steps which are required
#' to fit the Pareto/NBD model with dynamic covariates.
#'
#' @keywords internal
#' @seealso Other clv model classes \linkS4class{clv.model}, \linkS4class{clv.model.pnbd.no.cov}, \linkS4class{clv.model.pnbd.static.cov}
#' @seealso Classes using its instance: \linkS4class{clv.fitted.transactions.dynamic.cov}
#'
#' @include all_generics.R class_clv_model_pnbd_staticcov.R
setClass(Class = "clv.model.pnbd.dynamic.cov", contains = "clv.model.pnbd.static.cov")


clv.model.pnbd.dynamic.cov <- function(){
  return(new("clv.model.pnbd.dynamic.cov",
             clv.model.pnbd.static.cov(),

             name.model       = "Pareto NBD with Dynamic Covariates",
             # Overwrite optimx default args
             optimx.defaults  = list(method = "Nelder-Mead",
                                     itnmax = 3000,
                                     control = list(
                                       kkt = TRUE,
                                       save.failures = TRUE,
                                       # Do not perform starttests because it checks the scales with max(logpar)-min(logpar)
                                       #   but all standard start parameters are <= 0, hence there are no logpars what
                                       #   produces a warning
                                       starttests = FALSE))))
}


# Methods --------------------------------------------------------------------------------------------------------------------------------

# Override static cov implementation
# . clv.model.prepare.optimx.args ------------------------------------------------------------------------------------------------
#' @importFrom utils modifyList
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.pnbd.dynamic.cov"),
          definition = function(clv.model, clv.fitted, prepared.optimx.args){

            # Do not call the no.cov function because the LL is different
            x <- t.x <- T.cal <- NULL

            # Everything to call the LL function
            optimx.args <- modifyList(prepared.optimx.args,
                                      list(
                                        clv.fitted = clv.fitted,
                                        LL.function.sum = pnbd_dyncov_LL_sum,
                                        LL.function.ind = pnbd_dyncov_LL_ind, # if doing correlation
                                        # Ordering does not actually matter for dyncov_LL(params), just need all params
                                        LL.params.names.ordered = c(clv.model@names.prefixed.params.model,
                                                                    clv.fitted@names.prefixed.params.after.constr.life,
                                                                    clv.fitted@names.prefixed.params.after.constr.trans)))
            return(optimx.args)
          })


# . clv.model.process.post.estimation ------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.process.post.estimation", signature = signature(clv.model="clv.model.pnbd.dynamic.cov"), definition = function(clv.model, clv.fitted, res.optimx){

  # Estimate again at found values to get LL.data (of last method used)
  # This is then used when plotting and predicting
  optimal.coefs <- drop(tail(coef(res.optimx), n=1))


  if(!anyNA(optimal.coefs)){
    # Need to set prediction params to all params needed for LL
    clv.fitted <- clv.controlflow.predict.set.prediction.params(clv.fitted)

    # For the LL, the model params need to be logged again
    #   can do directly, as know this is the pnbd dyncov model
    # Other option: read from optimx result
    #   final.params <- c(drop(tail(coef(clv.fitted@optimx.estimation.output), n=1))[clv.fitted@clv.model@names.prefixed.params.model])
    final.params <- c(setNames(log(clv.fitted@prediction.params.model[c("r", "alpha", "s", "beta")]),
                               c("log.r", "log.alpha", "log.s", "log.beta")),
                      # use the same names as after interlayers. Need these as could be constrained
                      setNames(clv.fitted@prediction.params.life, clv.fitted@names.prefixed.params.after.constr.life),
                      setNames(clv.fitted@prediction.params.trans, clv.fitted@names.prefixed.params.after.constr.trans))

    # get LL with all values, not just ind LL or summed LL
    clv.fitted@LL.data <- pnbd_dyncov_LL(params = final.params, clv.fitted=clv.fitted)
    setkeyv(clv.fitted@LL.data, cols = "Id")
  }else{
    warning("Could not derive dyncov LL data with these final parameters - cannot predict and plot!", call. = FALSE)
  }

  return(clv.fitted)
})


# . clv.model.process.newdata ------------------------------------------------------------------------------------------------
#' @importFrom methods callNextMethod
setMethod(f = "clv.model.process.newdata", signature = signature(clv.model = "clv.model.pnbd.dynamic.cov"), definition = function(clv.model, clv.fitted, verbose){
  # do nocov preparations (new cbs only)
  clv.fitted <- callNextMethod()

  # data in clv.fitted is already newdata

  # Remake cbs for dyncov becasuse in the nocov cbs, there is no d_omega.
  clv.fitted@cbs <- pnbd_dyncov_cbs(clv.data = clv.fitted@clv.data)

  if(verbose)
    message("Calculating LogLikelihood values for the provided newdata at the estimated parameters.")

  # For dyncov also neeed to calculate the LL.data again - Exact same as in

  # For the LL, the model params need to be logged again
  #   can do directly, as know this is the pnbd dyncov model
  # Other option: read from optimx result
  # final.params <- c(drop(tail(coef(clv.fitted@optimx.estimation.output), n=1))[clv.fitted@clv.model@names.prefixed.params.model])
  final.params <- c(setNames(log(clv.fitted@prediction.params.model[c("r", "alpha", "s", "beta")]),
                             c("log.r", "log.alpha", "log.s", "log.beta")),
                    # use the same names as after interlayers. Need these as could be constrained
                    setNames(clv.fitted@prediction.params.life, clv.fitted@names.prefixed.params.after.constr.life),
                    setNames(clv.fitted@prediction.params.trans, clv.fitted@names.prefixed.params.after.constr.trans))

  # also need to re-do the walks if there is new data
  l.walks <- pnbd_dyncov_makewalks(clv.data = clv.fitted@clv.data)
  clv.fitted@data.walks.life  = l.walks[["data.walks.life"]]
  clv.fitted@data.walks.trans = l.walks[["data.walks.trans"]]

  # get LL with all values, not just ind LL or summed LL
  clv.fitted@LL.data <- pnbd_dyncov_LL(params = final.params, clv.fitted=clv.fitted)
  setkeyv(clv.fitted@LL.data, cols = "Id")

  return(clv.fitted)
})


# . clv.model.predict ------------------------------------------------------------------------------------------------
setMethod("clv.model.predict", signature(clv.model="clv.model.pnbd.dynamic.cov"), function(clv.model, clv.fitted, dt.predictions, verbose, continuous.discount.factor, ...){

  period.length <- period.last <- CET <- i.CET <- PAlive <- i.palive <-  DECT <- i.DECT <-  NULL

  predict.number.of.periods <- dt.predictions[1, period.length]
  tp.period.last <- dt.predictions[1, period.last]

  if(verbose)
    message("Predicting for dyn cov model....")


  # Palive
  dt.palive <- pnbd_dyncov_palive(clv.fitted=clv.fitted)
  dt.predictions[dt.palive, PAlive := i.palive, on="Id"]


  # CET
  dt.cet <- pnbd_dyncov_CET(clv.fitted = clv.fitted,
                            predict.number.of.periods = predict.number.of.periods,
                            prediction.end.date       = tp.period.last)
  dt.predictions[dt.cet, CET := i.CET, on="Id"]


  # DECT
  if(continuous.discount.factor != 0){
    dt.dect <- pnbd_dyncov_DECT(clv.fitted = clv.fitted,
                                predict.number.of.periods  = predict.number.of.periods,
                                prediction.end.date        = tp.period.last,
                                continuous.discount.factor = continuous.discount.factor)
    dt.predictions[dt.dect, DECT :=i.DECT, on="Id"]
  }else{
    # If the discount factor is zero, the results correspond to CET
    #   DECT crashes for discount.factor = 0
    dt.predictions[, DECT := CET]
  }

  return(dt.predictions)
})


# . clv.model.expectation ------------------------------------------------------------------------------------------------
setMethod("clv.model.expectation", signature(clv.model="clv.model.pnbd.dynamic.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  return(pnbd_dyncov_expectation(clv.fitted=clv.fitted, dt.expectation.seq=dt.expectation.seq, verbose=verbose))
})


