clv.template.controlflow.pmf <- function(clv.fitted, x, plot = FALSE){

  l.res.pmf <- lapply(x, function(i){
    dt.pmf <- clv.model.pmf(clv.model = clv.fitted@clv.model, clv.fitted = clv.fitted, x = i)
    # measure.vars = NULL: all except id vars
    return(melt(dt.pmf, id.vars="Id", measure.vars=NULL, variable.name="pmf", na.rm=FALSE))
  })

  return(dcast(rbindlist(l.res.pmf), Id~pmf))
}
