#' @templateVar name_model_full Pareto/NBD
#' @template template_class_clvmodelnocov
#'
#' @keywords internal
#' @seealso Other clv model classes \linkS4class{clv.model}, \linkS4class{clv.model.pnbd.static.cov}, \linkS4class{clv.model.pnbd.dynamic.cov}
#' @seealso Classes using its instance: \linkS4class{clv.fitted}
#' @include all_generics.R class_clv_model_withcorrelation.R
#' @importFrom methods setClass
setClass(Class = "clv.model.pnbd.no.cov", contains = "clv.model.with.correlation")


#' @importFrom methods new
clv.model.pnbd.no.cov <- function(){

  return(new("clv.model.pnbd.no.cov",
             name.model                  = "Pareto NBD Standard",
             names.original.params.model = c(r="r", alpha="alpha", s="s", beta="beta"),
             names.prefixed.params.model = c("log.r","log.alpha", "log.s", "log.beta"),
             start.params.model          = c(r=1, alpha=1, s=1, beta=1),
             optimx.defaults = list(method = "L-BFGS-B",
                                    # lower   = c(log(1*10^(-5)),log(1*10^(-5)),log(1*10^(-5)),log(1*10^(-5))),
                                    # upper   = c(log(300),log(2000),log(300),log(2000)),
                                    itnmax  = 3000,
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
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, start.params.model, optimx.args, verbose, use.cor, start.param.cor, ...){
  err.msg <- c()
  # Have to be > 0 as will be logged
  if(any(start.params.model <= 0))
    err.msg <- c(err.msg, "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")

  err.msg <- c(err.msg, .check_user_data_single_boolean(b=use.cor, var.name ="use.cor"))
  err.msg <- c(err.msg, check_user_data_startparamcorm(start.param.cor=start.param.cor, use.cor=use.cor))

  check_err_msg(err.msg)
})


# .clv.model.put.estimation.input --------------------------------------------------------------------------------------------------------
# Nothing required, use clv.model.with.correlation


# .clv.model.transform.start.params.model --------------------------------------------------------------------------------------------------------
#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})

# .clv.model.backtransform.estimated.params.model --------------------------------------------------------------------------------------------------------
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})

# .clv.model.prepare.optimx.args --------------------------------------------------------------------------------------------------------
#' @importFrom utils modifyList
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args){

  # Only add LL function args, everything else is prepared already, incl. start parameters
  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = pnbd_nocov_LL_sum,
                                 LL.function.ind = pnbd_nocov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,

                                 # parameter ordering for the callLL interlayer
                                 LL.params.names.ordered = c(log.r="log.r", log.alpha="log.alpha",
                                                             log.s="log.s", log.beta="log.beta")),
                            keep.null = TRUE)
  return(optimx.args)
})

# . clv.model.process.post.estimation -----------------------------------------------------------------------------------------
setMethod("clv.model.process.post.estimation", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, res.optimx){
  # No additional step needed (ie store model specific stuff, extra process)
  return(clv.fitted)
})


# .clv.model.cor.to.m --------------------------------------------------------------------------------------------------------
setMethod(f="clv.model.cor.to.m", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, prefixed.params.model, param.cor){

  .cor.part <- function(params){
    r     <- exp(params[["log.r"]])
    alpha <- exp(params[["log.alpha"]])
    s     <- exp(params[["log.s"]])
    beta  <- exp(params[["log.beta"]])

    fct.part <- function(param.ab, param.rs){
      return( (sqrt(param.rs) / (1+param.ab)) * ((param.ab / (1+param.ab))^param.rs))
    }
    return(fct.part(param.ab = alpha, param.rs = r) * fct.part(param.ab = beta, param.rs = s))
  }
  res.m <- param.cor / .cor.part(params=prefixed.params.model)

  # return unnamed as otherwise still called "cor"
  return(unname(res.m))
})

setMethod(f="clv.model.m.to.cor", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, prefixed.params.model, param.m){
  .cor.part <- function(params){
    r     <- exp(params[["log.r"]])
    alpha <- exp(params[["log.alpha"]])
    s     <- exp(params[["log.s"]])
    beta  <- exp(params[["log.beta"]])

    fct.part <- function(param.ab, param.rs){
      return( (sqrt(param.rs) / (1+param.ab)) * ((param.ab / (1+param.ab))^param.rs) )
    }
    return(fct.part(param.ab = alpha, param.rs = r) * fct.part(param.ab = beta, param.rs = s))
  }
  res.cor <- param.m * .cor.part(params=prefixed.params.model)

  # return unnamed as otherwise still called "m"
  return(unname(res.cor))
})


# .clv.model.vcov.jacobi.diag --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, prefixed.params){

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

  # If correlation, add the transformations for each parameter vs correlation param.m
  if(clv.model@estimation.used.correlation){
    # This is same as m.to.cor
    cor.phi <- function(param.m, a, r, s, b){
      return(param.m * (sqrt(r)/(1+a)) * (a/(1+a))^r * (sqrt(s)/(1+b)) * (b/(1+b))^s)
    }
    a <- exp(prefixed.params["log.alpha"])
    r <- exp(prefixed.params["log.r"])
    b <- exp(prefixed.params["log.beta"])
    s <- exp(prefixed.params["log.s"])
    param.m <- prefixed.params[clv.model@name.prefixed.cor.param.m]

    # eq 2
    phi_dloga <- cor.phi(param.m=param.m, a=a, r=r, b=b, s=s) * (r - ((a*(1+r))/(1+a)))
    # eq 3
    phi_dlogb <- cor.phi(param.m=param.m, a=a, r=r, b=b, s=s) * (s - ((b*(1+s))/(1+b)))
    # eq 4
    phi_dlogr <- cor.phi(param.m=param.m, a=a, r=r, b=b, s=s) * (0.5 - r*log(1+a) + r*log(a))
    # eq 5
    phi_dlogs <- cor.phi(param.m=param.m, a=a, r=r, b=b, s=s) * (0.5 - s*log(1+b) + s*log(b))
    # eq 6
    phi_dlogm <- (sqrt(r)/(1+a)) * (a/(1+a))^r * (sqrt(s)/(1+b)) * (b/(1+b))^s

    # Add to transformation matrix on last line only! (not aswell on the last column)
    m.diag[clv.model@name.prefixed.cor.param.m, "log.alpha"] <- phi_dloga
    m.diag[clv.model@name.prefixed.cor.param.m, "log.r"]     <- phi_dlogr
    m.diag[clv.model@name.prefixed.cor.param.m, "log.beta"]  <- phi_dlogb
    m.diag[clv.model@name.prefixed.cor.param.m, "log.s"]     <- phi_dlogs
    m.diag[clv.model@name.prefixed.cor.param.m,
           clv.model@name.prefixed.cor.param.m]              <- phi_dlogm
  }

  return(m.diag)
})

# clv.model.process.newdata --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.process.newdata", signature = signature(clv.model = "clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose){

  # clv.data in clv.fitted is already replaced with newdata here
  # Only need to redo cbs if new data is given

  clv.fitted@cbs <- pnbd_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})


# clv.model.predict --------------------------------------------------------------------------------------------------------
setMethod("clv.model.predict", signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, dt.predictions, verbose, continuous.discount.factor, ...){
  period.length <- Id <- x <- t.x <- T.cal <-  PAlive <- i.PAlive <- CET <- i.CET <- DERT <- i.DERT <- NULL # cran silence


  predict.number.of.periods <- dt.predictions[1, period.length]


  # To ensure sorting, do everything in a single table
  dt.result <- copy(clv.fitted@cbs[, c("Id", "x", "t.x", "T.cal")])


  # Add CET
  dt.result[, CET :=  pnbd_nocov_CET(r       = clv.fitted@prediction.params.model[["r"]],
                                     alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                     s       = clv.fitted@prediction.params.model[["s"]],
                                     beta_0  = clv.fitted@prediction.params.model[["beta"]],
                                     dPeriods = predict.number.of.periods,
                                     vX     = x,
                                     vT_x   = t.x,
                                     vT_cal = T.cal)]

  # Add PAlive
  dt.result[, PAlive := pnbd_nocov_PAlive(r       = clv.fitted@prediction.params.model[["r"]],
                                          alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                          s       = clv.fitted@prediction.params.model[["s"]],
                                          beta_0  = clv.fitted@prediction.params.model[["beta"]],
                                          vX     = x,
                                          vT_x   = t.x,
                                          vT_cal = T.cal)]

  # Add DERT
  dt.result[, DERT := pnbd_nocov_DERT(r       = clv.fitted@prediction.params.model[["r"]],
                                      alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                      s       = clv.fitted@prediction.params.model[["s"]],
                                      beta_0  = clv.fitted@prediction.params.model[["beta"]],
                                      continuous_discount_factor = continuous.discount.factor,
                                      vX     = x,
                                      vT_x   = t.x,
                                      vT_cal = T.cal)]

  # Add results to prediction table, by matching Id
  dt.predictions[dt.result, CET    := i.CET,    on = "Id"]
  dt.predictions[dt.result, PAlive := i.PAlive, on = "Id"]
  dt.predictions[dt.result, DERT   := i.DERT,   on = "Id"]

  return(dt.predictions)
})

# . clv.model.expectation --------------------------------------------------------------------------------------------------------
setMethod("clv.model.expectation", signature(clv.model="clv.model.pnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  r <- s <- alpha_i <- beta_i <- date.first.actual.trans <- T.cal <- t_i <- NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  fct.expectation <- function(params_i.t) {return(pnbd_nocov_expectation(r = clv.fitted@prediction.params.model[["r"]],
                                                                            s = clv.fitted@prediction.params.model[["s"]],
                                                                            alpha_0 = clv.fitted@prediction.params.model[["alpha"]],
                                                                            beta_0 = clv.fitted@prediction.params.model[["beta"]],
                                                                            vT_i = params_i.t$t_i))}

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

