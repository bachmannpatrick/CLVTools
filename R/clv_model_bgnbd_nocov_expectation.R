#' @include all_generics.R
#' @include class_clv_model_bgnbd_nocov.R

setMethod("clv.model.expectation", signature(clv.model="clv.model.bgnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  #TODO: figure out what this calculates / how this is calculated
  r <- alpha_i <- a_i <- b_i <- date.first.repeat.trans<- date.first.actual.trans <- T.cal <- t_i<- period.first.trans<-NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  params_i[, r       := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha := clv.fitted@prediction.params.model[["alpha"]]]
  params_i[, a       := clv.fitted@prediction.params.model[["a"]]]
  params_i[, b  := clv.fitted@prediction.params.model[["b"]]]

  fct.bgnbd.exp <- function(r, alpha, a, b, t){ #todo: rename to ...expectation
    term1 = (a + b - 1)/(a - 1)
    term2 = (alpha/(alpha + t))^r
    term3 = hypWrap(r, b, a + b - 1, t/(alpha + t))
    #replace with cephes hypergeo

    return(term1 * (1 - term2 * term3))
  }


  # To caluclate expectation at point t for customers alive in t, given in params_i.t
  fct.expectation <- function(params_i.t) {
   # bgnbd.Expectation(params = c(r = r, alpha = alpha, a = a, b = b), t = t_i))
    return(params_i.t[,.(res = fct.bgnbd.exp(r = r, alpha = alpha, a = a, b = b, t = t_i)), by = "Id"]$res)
  }


  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.expectation, clv.time = clv.fitted@clv.data@clv.time))
})
