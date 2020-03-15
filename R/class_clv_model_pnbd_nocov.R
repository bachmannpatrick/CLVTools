# Class --------------------------------------------------------------------------------------------------------------------------------
#' @importFrom methods setClass
#' @include all_generics.R class_clv_model_basestrategy.R
setClass(Class = "clv.model.pnbd.no.cov", contains = "clv.model",
         # no additional slots for pnbd base model
         slots = list(),
         # init with model defaults
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
                                    starttests = FALSE))
         ))


# setValidity("clv.model.pnbd.no.cov", method = function(object){
#   # At anypoint need to be exactly like the prototype. Never change the slots to anything.
#   #   clv.model parent does checks on content
#   print("checking clv.model.pnbd.no.cov")
#   # print(all.equal(object, new("clv.model.pnbd.no.cov")))
#   return(isTRUE(all.equal(object, new("clv.model.pnbd.no.cov"))))
# })

# Methods --------------------------------------------------------------------------------------------------------------------------------
#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){
  # Have to be > 0 as will be logged
  if(any(start.params.model <= 0))
    check_err_msg(err.msg = "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")

  if(length(list(...)) > 0)
    warning("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)
})


setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose, ...){
  # nothing to put specifically for this model
  return(clv.fitted)
})

#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})

#' @importFrom stats setNames
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.pnbd.no.cov"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})

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

    # Add new row and column
    # m.diag <- cbind(rbind(m.diag, 0),0)
    #   For some reasons, name are not taken in cbind/rbind
    # rownames(m.diag) <- c(rownames(m.diag)[-nrow(m.diag)], clv.fitted@name.prefixed.cor.param.m)
    # colnames(m.diag) <- c(colnames(m.diag)[-ncol(m.diag)], clv.fitted@name.prefixed.cor.param.m)

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


setMethod(f = "clv.model.put.newdata", signature = signature(clv.model = "clv.model.pnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose){
  # clv.data in clv.fitted is already replaced with newdata here
  # Need to only redo cbs if given new data
  clv.fitted@cbs <- pnbd_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})

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
  return(unname(res.m)) # return unnamed as otherwise still called "cor"
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
  return(unname(res.cor)) # return unnamed as otherwise still called "m"
})


#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.pnbd.no.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  Id <- x <- t.x <- T.cal <-  PAlive <- CET <- DERT.R <- DERT.cpp <- NULL # cran silence

  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  predict.number.of.periods <- dt.prediction[1, period.length]

  # pass matrix(0) because no covariates are used


  # Put params together in single vec
  estimated.params <- c(r = clv.fitted@prediction.params.model[["r"]], alpha = clv.fitted@prediction.params.model[["alpha"]],
                        s = clv.fitted@prediction.params.model[["s"]], beta  = clv.fitted@prediction.params.model[["beta"]])

  # Add CET
  dt.prediction[, CET :=  pnbd_nocov_CET(vEstimated_params = estimated.params,
                                         dPrediction_period = predict.number.of.periods,
                                         vX     = clv.fitted@cbs[, x],
                                         vT_x   = clv.fitted@cbs[, t.x],
                                         vT_cal = clv.fitted@cbs[, T.cal])]

  # Add PAlive
  dt.prediction[, PAlive := pnbd_nocov_PAlive(vEstimated_params = estimated.params,
                                              vX     = clv.fitted@cbs[, x],
                                              vT_x   = clv.fitted@cbs[, t.x],
                                              vT_cal = clv.fitted@cbs[, T.cal])]

  # Add DERT
  dt.prediction[, DERT := pnbd_nocov_DERT(vEstimated_params = estimated.params,
                                          continuous_discount_factor = continuous.discount.factor,
                                          vX     = clv.fitted@cbs[, x],
                                          vT_x   = clv.fitted@cbs[, t.x],
                                          vT_cal = clv.fitted@cbs[, T.cal])]

  return(dt.prediction)
})


setMethod("clv.model.expectation", signature(clv.model="clv.model.pnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){

  r <- s <- alpha_i <- beta_i <- date.first.repeat.trans<- date.first.actual.trans <- T.cal <- t_i<- period.first.trans<-NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  # all params exactly the same for all customers as there are no covariates
  params_i[, r       := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha_i := clv.fitted@prediction.params.model[["alpha"]]]
  params_i[, s       := clv.fitted@prediction.params.model[["s"]]]
  params_i[, beta_i  := clv.fitted@prediction.params.model[["beta"]]]

  # To caluclate expectation at point t for customers alive in t, given in params_i.t
  fct.expectation <- function(params_i.t) {return( params_i.t[, (r * beta_i)/(alpha_i * (s - 1)) * (1 - (beta_i/(beta_i + t_i))^(s - 1))] )}

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})

