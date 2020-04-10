# Class --------------------------------------------------------------------------------------------------------------------------------
#' @importFrom methods setClass
#' @include all_generics.R class_clv_model.R
setClass(Class = "clv.model.pnbd.no.cov", contains = "clv.model",
         # no additional slots for pnbd base model
         slots = list(),
         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         #  init with model defaults
         prototype = list(
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
                                    all.methods = FALSE,
                                    save.failures = TRUE,
                                    # Do not perform starttests because it checks the scales with max(logpar)-min(logpar)
                                    #   but all standard start parameters are <= 0, hence there are no logpars what
                                    #   produces a warning
                                    starttests = FALSE))))


# Methods --------------------------------------------------------------------------------------------------------------------------------

# .clv.model.check.input.args -----------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){
  # Have to be > 0 as will be logged
  if(any(start.params.model <= 0))
    check_err_msg(err.msg = "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")

  if(length(list(...)) > 0)
    warning("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)
})


# .clv.model.put.estimation.input --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose, ...){
  # nothing to put specifically for this model
  return(clv.fitted)
})

# .clv.model.transform.start.params.model --------------------------------------------------------------------------------------------------------
#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})

# .clv.model.backtransform.estimated.params.model --------------------------------------------------------------------------------------------------------
#' @importFrom stats setNames
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})

# .clv.model.prepare.optimx.args --------------------------------------------------------------------------------------------------------
#' @importFrom utils modifyList
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){
  # Also model optimization settings should go here

  # Only add LL function args, everything else is prepared already, incl. start parameters
  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = pnbd_nocov_LL_sum,
                                 LL.function.ind = pnbd_nocov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,

                                 # parameter ordering for the callLL interlayer
                                 #** TODO: Hardcode from cpp interface
                                 LL.params.names.ordered = c(log.r="log.r", log.alpha="log.alpha",
                                                             log.s="log.s", log.beta="log.beta")),
                            keep.null = TRUE)
  return(optimx.args)
})

# . clv.model.process.post.estimation -----------------------------------------------------------------------------------------
setMethod("clv.model.process.post.estimation", signature = signature(clv.model="clv.model"), definition = function(clv.model, clv.fitted, res.optimx){
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
  if(clv.fitted@estimation.used.correlation){
    # This is same as m.to.cor
    cor.phi <- function(param.m, a, r, s, b){
      return(param.m * (sqrt(r)/(1+a)) * (a/(1+a))^r * (sqrt(s)/(1+b)) * (b/(1+b))^s)
    }
    a <- exp(prefixed.params["log.alpha"])
    r <- exp(prefixed.params["log.r"])
    b <- exp(prefixed.params["log.beta"])
    s <- exp(prefixed.params["log.s"])
    param.m <- prefixed.params[clv.fitted@name.prefixed.cor.param.m]

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
    m.diag[clv.fitted@name.prefixed.cor.param.m, "log.alpha"] <- phi_dloga
    m.diag[clv.fitted@name.prefixed.cor.param.m, "log.r"]     <- phi_dlogr
    m.diag[clv.fitted@name.prefixed.cor.param.m, "log.beta"]  <- phi_dlogb
    m.diag[clv.fitted@name.prefixed.cor.param.m, "log.s"]     <- phi_dlogs
    m.diag[clv.fitted@name.prefixed.cor.param.m,
           clv.fitted@name.prefixed.cor.param.m]              <- phi_dlogm
  }

  return(m.diag)
})

# .clv.model.put.newdata --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.put.newdata", signature = signature(clv.model = "clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose){

  # clv.data in clv.fitted is already replaced with newdata here
  # Only need to redo cbs if new data is given

  clv.fitted@cbs <- pnbd_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})


# .clv.model.predict.clv --------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  period.length <- Id <- x <- t.x <- T.cal <-  PAlive <- CET <- DERT <- NULL # cran silence

  predict.number.of.periods <- dt.prediction[1, period.length]

  # Put params together in single vec
  estimated.params <- c(r = clv.fitted@prediction.params.model[["r"]], alpha = clv.fitted@prediction.params.model[["alpha"]],
                        s = clv.fitted@prediction.params.model[["s"]], beta  = clv.fitted@prediction.params.model[["beta"]])


  # To ensure sorting, do everything in a single table
  dt.result <- copy(clv.fitted@cbs[, c("Id", "x", "t.x", "T.cal")])


  # Add CET
  dt.result[, CET :=  pnbd_nocov_CET(vEstimated_params = estimated.params,
                                     dPrediction_period = predict.number.of.periods,
                                     vX     = x,
                                     vT_x   = t.x,
                                     vT_cal = T.cal)]

  # Add PAlive
  dt.result[, PAlive := pnbd_nocov_PAlive(vEstimated_params = estimated.params,
                                          vX     = x,
                                          vT_x   = t.x,
                                          vT_cal = T.cal)]

  # Add DERT
  dt.result[, DERT := pnbd_nocov_DERT(vEstimated_params = estimated.params,
                                      continuous_discount_factor = continuous.discount.factor,
                                      vX     = x,
                                      vT_x   = t.x,
                                      vT_cal = T.cal)]

  # Add results to prediction table, by matching Id
  dt.prediction[dt.result, CET    := i.CET,    on = "Id"]
  dt.prediction[dt.result, PAlive := i.PAlive, on = "Id"]
  dt.prediction[dt.result, DERT   := i.DERT,   on = "Id"]

  return(dt.prediction)
})

# . clv.model.expectation --------------------------------------------------------------------------------------------------------
setMethod("clv.model.expectation", signature(clv.model="clv.model.pnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){

  r <- s <- alpha_i <- beta_i <- date.first.actual.trans <- T.cal <- t_i<- period.first.trans<-NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  # all params exactly the same for all customers because there are no covariates
  params_i[, r       := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha_i := clv.fitted@prediction.params.model[["alpha"]]]
  params_i[, s       := clv.fitted@prediction.params.model[["s"]]]
  params_i[, beta_i  := clv.fitted@prediction.params.model[["beta"]]]

  # To caluclate expectation at point t for customers alive in t, given in params_i.t
  fct.expectation <- function(params_i.t) {return( params_i.t[, (r * beta_i)/(alpha_i * (s - 1)) * (1 - (beta_i/(beta_i + t_i))^(s - 1))] )}

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

