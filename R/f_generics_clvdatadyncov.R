setMethod("clv.data.create.bootstrapping.data", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data, ids){

  # jump over implementation for clv.data.static.covariates
  clv.data.no.cov <- clv.data.create.bootstrapping.data(clv.data=as(object = clv.data, Class = 'clv.data'), ids = ids)

  data.cov.life <- clv.data.select.customer.data.duplicating.ids(dt.data=clv.data@data.cov.life, ids=ids)
  data.cov.trans <- clv.data.select.customer.data.duplicating.ids(dt.data=clv.data@data.cov.trans, ids=ids)


  # TODO: This should be moved into the constructor -----------------------------
  names.cov.life <- clv.data@names.cov.data.life
  names.cov.trans <- clv.data@names.cov.data.trans
  setcolorder(data.cov.life,  c("Id", "Cov.Date", "tp.cov.lower", "tp.cov.upper", names.cov.life))
  setcolorder(data.cov.trans, c("Id", "Cov.Date", "tp.cov.lower", "tp.cov.upper", names.cov.trans))

  setkeyv(data.cov.life,  cols = c("Id", "Cov.Date", "tp.cov.lower", "tp.cov.upper"))
  setkeyv(data.cov.trans, cols = c("Id", "Cov.Date", "tp.cov.lower", "tp.cov.upper"))

  return(
    clv.data.dynamic.covariates(
      no.cov.obj = clv.data.no.cov,
      data.cov.life = data.cov.life,
      data.cov.trans = data.cov.trans,
      names.cov.data.life = names.cov.life,
      names.cov.data.trans = names.cov.trans))
})
