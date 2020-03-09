#' @include all_generics.R
#' @include class_clv_model_bgnbd_nocov.R

setMethod("clv.model.expectation", signature(clv.model="clv.model.bgnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){

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
