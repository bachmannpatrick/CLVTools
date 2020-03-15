#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.bgnbd.no.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  #Id <- x <- t.x <- T.cal <-  PAlive <- CET <- DERT.R <- DERT.cpp <- NULL # cran silence

  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  predict.number.of.periods <- dt.prediction[1, period.length]

  # pass matrix(0) because no covariates are used


  # Put params together in single vec
  estimated.params <- c(r = clv.fitted@prediction.params.model[["r"]], alpha = clv.fitted@prediction.params.model[["alpha"]],
                        a = clv.fitted@prediction.params.model[["a"]], b  = clv.fitted@prediction.params.model[["b"]])


  #bgnbd.ConditionalExpectedTransactions
  # Add CET
  dt.prediction[, CET := bgnbd.ConditionalExpectedTransactions(params = estimated.params,
                                                               T.star = predict.number.of.periods,
                                                               x = clv.fitted@cbs[, x],
                                                               t.x = clv.fitted@cbs[, t.x],
                                                               T.cal = clv.fitted@cbs[, T.cal])]


  #bgnbd.PAlive
  # Add PAlive
  dt.prediction[, PAlive := bgnbd.PAlive(params = estimated.params,
                                         x = clv.fitted@cbs[, x],
                                         t.x = clv.fitted@cbs[, t.x],
                                         T.cal = clv.fitted@cbs[, T.cal])]
  # Add DERT
  dt.prediction[, DERT := 0]

  return(dt.prediction)
})

