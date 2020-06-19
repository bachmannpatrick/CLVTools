#' @templateVar name_model_full BG/NBD
#' @template template_class_clvmodelnocov
#'
#' @importFrom methods setClass
#' @seealso Other clv model classes \link{clv.model-class}, \link{clv.model.bgnbd.static.cov-class}
#' @seealso Classes using its instance: \link{clv.fitted-class}
#' @include all_generics.R class_clv_model_nocorrelation.R
setClass(Class = "clv.model.bgnbd.no.cov", contains = "clv.model.no.correlation",
         slots = list(),
         prototype = list(
           name.model = character(0),
           names.original.params.model = character(0),
           names.prefixed.params.model = character(0),
           start.params.model = numeric(0)
         ))

#' @importFrom methods new
clv.model.bgnbd.no.cov <- function(){
  return(new("clv.model.bgnbd.no.cov",
             name.model = "BG/NBD Standard",
             names.original.params.model = c(r="r", alpha="alpha", a="a", b="b"),
             names.prefixed.params.model = c("log.r", "log.alpha", "log.a", "log.b"),
             start.params.model = c(r=1, alpha = 3, a = 1, b = 3)))
}

# Methods --------------------------------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){

  err.msg <- c()

  # Have to be > 0 as will be logged
  if(any(start.params.model <= 0)){
    err.msg <- c(err.msg, "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")
  }


  if(use.cor){
    err.msg <- c(err.msg, "Correlation is not supported for the BG/NBD model")
  }

  if(length(list(...)) > 0){
    stop("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)
  }

  check_err_msg(err.msg)

})


# .clv.model.put.estimation.input --------------------------------------------------------------------------------------------------------
# setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose, ...){
#   # nothing to put specifically for this model
#   return(clv.model)
# })

# .clv.model.transform.start.params.model --------------------------------------------------------------------------------------------------------
#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})

# .clv.model.backtransform.estimated.params.model --------------------------------------------------------------------------------------------------------
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})

# .clv.model.prepare.optimx.args --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){

  # Only add LL function args, everything else is prepared already, incl. start parameters
  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = bgnbd_nocov_LL_sum,
                                 LL.function.ind = bgnbd_nocov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,

                                 # parameter ordering for the callLL interlayer
                                 LL.params.names.ordered = c(log.r = "log.r",log.alpha =  "log.alpha", log.a = "log.a", log.b = "log.b")),
                            keep.null = TRUE)
  return(optimx.args)
})


# . clv.model.process.post.estimation -----------------------------------------------------------------------------------------
setMethod("clv.model.process.post.estimation", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, res.optimx){
  # No additional step needed (ie store model specific stuff, extra process)
  return(clv.fitted)
})


# .clv.model.put.newdata --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.put.newdata", signature = signature(clv.model = "clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose){
  # clv.data in clv.fitted is already replaced with newdata here
  # Need to only redo cbs if given new data
  clv.fitted@cbs <- bgnbd_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})


# . clv.model.expectation --------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod("clv.model.expectation", signature(clv.model="clv.model.bgnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  r <- alpha <- a <- b <- date.first.repeat.trans<- date.first.actual.trans <- T.cal <- t_i<- period.first.trans<-NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  params_i[, r := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha := clv.fitted@prediction.params.model[["alpha"]]]
  params_i[, a := clv.fitted@prediction.params.model[["a"]]]
  params_i[, b := clv.fitted@prediction.params.model[["b"]]]

  fct.bgnbd.expectation <- function(params_i.t){
    term1 <- params_i.t[,(a + b - 1)/(a - 1)]
    term2 <- params_i.t[,(alpha/(alpha + t_i))^r]
    term3 <- params_i.t[, vec_gsl_hyp2f1_e(r, b, a+b-1, t_i/(alpha+t_i) )$value]

    return(term1 * (1 - term2 * term3))
  }

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.bgnbd.expectation, clv.time = clv.fitted@clv.data@clv.time))
})



# .clv.model.predict.clv --------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.bgnbd.no.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  r <- alpha <- a <- b <- period.length <- CET <- PAlive <- DERT <- i.CET <- i.PAlive <- i.DERT <- x <- t.x <- T.cal <- NULL

  predict.number.of.periods <- dt.prediction[1, period.length]

  # To ensure sorting, do everything in a single table
  dt.result <- copy(clv.fitted@cbs[, c("Id", "x", "t.x", "T.cal")])

  # Add CET
  dt.result[, CET := bgnbd_nocov_CET(r     = clv.fitted@prediction.params.model[["r"]],
                                     alpha = clv.fitted@prediction.params.model[["alpha"]],
                                     a     = clv.fitted@prediction.params.model[["a"]],
                                     b     = clv.fitted@prediction.params.model[["b"]],
                                     dPeriods = predict.number.of.periods,
                                     vX = x,
                                     vT_x = t.x,
                                     vT_cal = T.cal)]


  # Add PAlive
  dt.result[, PAlive := bgnbd_nocov_PAlive(r     = clv.fitted@prediction.params.model[["r"]],
                                           alpha = clv.fitted@prediction.params.model[["alpha"]],
                                           a     = clv.fitted@prediction.params.model[["a"]],
                                           b     = clv.fitted@prediction.params.model[["b"]],
                                           vX = x,
                                           vT_x = t.x,
                                           vT_cal = T.cal)]
  # Add DERT
  dt.result[, DERT := 0]

  # Add results to prediction table, by matching Id
  dt.prediction[dt.result, CET    := i.CET,    on = "Id"]
  dt.prediction[dt.result, PAlive := i.PAlive, on = "Id"]
  dt.prediction[dt.result, DERT   := i.DERT,   on = "Id"]

  return(dt.prediction)
})

# .clv.model.vcov.jacobi.diag --------------------------------------------------------------------------------------------------------

setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, prefixed.params){
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

