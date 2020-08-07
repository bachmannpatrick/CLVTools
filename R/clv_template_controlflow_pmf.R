clv.template.controlflow.pmf <- function(clv.fitted, x, plot = FALSE){
  i <- 1
  res <- rep(0, length(x))

  for(ind.x in x){
    res[i] <- clv.model.pmf(clv.model = clv.fitted@clv.model, clv.fitted = clv.fitted, x = ind.x, t = clv.fitted@cbs[,T.cal])
    i <- i + 1
  }

  return(res)
}
