setMethod("clv.data.create.bootstrapping.data", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data, ids){
  clv.data.no.cov <- callNextMethod()

  dt.cov.life <- clv.data.select.customer.data.duplicating.ids(dt.data=clv.data@data.cov.life, ids=ids)
  dt.cov.trans <- clv.data.select.customer.data.duplicating.ids(dt.data=clv.data@data.cov.trans, ids=ids)

  setkeyv(dt.cov.life, cols = "Id")
  setkeyv(dt.cov.trans, cols = "Id")

  return(
    clv.data.static.covariates(
      no.cov.obj = clv.data.no.cov,
      data.cov.life = dt.cov.life,
      data.cov.trans = dt.cov.trans,
      names.cov.data.life = clv.data@names.cov.data.life,
      names.cov.data.trans = clv.data@names.cov.data.trans))
})
