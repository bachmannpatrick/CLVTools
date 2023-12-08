setMethod("clv.data.create.bootstrapping.data", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data, ids){

  # jump over implementation for clv.data.static.covariates
  clv.data.no.cov <- clv.data.create.bootstrapping.data(clv.data=as(object = clv.data, Class = 'clv.data'), ids = ids)


  # select full length of existing covariates (= filter only by Id) and not only covariates in the estimation period
  # this allows to still predict on the original length

  return(
    clv.data.dynamic.covariates(
      no.cov.obj = clv.data.no.cov,
      data.cov.life = clv.data.select.customer.data.duplicating.ids(dt.data=clv.data@data.cov.life, ids=ids),
      data.cov.trans = clv.data.select.customer.data.duplicating.ids(dt.data=clv.data@data.cov.trans, ids=ids),
      names.cov.data.life = clv.data@names.cov.data.life,
      names.cov.data.trans = clv.data@names.cov.data.trans))
})
