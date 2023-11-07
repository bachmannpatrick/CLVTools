setMethod("clv.data.create.bootstrapping.data", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data, ids){
  clv.data.no.cov <- callNextMethod()

  return(
    clv.data.static.covariates(
      no.cov.obj = clv.data.no.cov,
      data.cov.life = clv.data.select.customer.data.with.duplicates(dt.data=clv.data@data.cov.life, ids=ids),
      data.cov.trans = clv.data.select.customer.data.with.duplicates(dt.data=clv.data@data.cov.trans, ids=ids),
      names.cov.data.life = clv.data@names.cov.data.life,
      names.cov.data.trans = clv.data@names.cov.data.trans))
})
