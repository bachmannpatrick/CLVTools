setMethod("clv.data.create.bootstrapping.data", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data, ids){

  # jump over implementation for clv.data.static.covariates
  clv.data.no.cov <- clv.data.create.bootstrapping.data(clv.data=as(object = clv.data, Class = 'clv.data'), ids = ids)
  ids <- unique(ids)

  return(
    clv.data.dynamic.covariates(
      no.cov.obj = clv.data.no.cov,
      data.cov.life = clv.data@data.cov.life[SJ(Id=ids), on="Id", nomatch = NULL],
      data.cov.trans = clv.data@data.cov.trans[SJ(Id=ids), on="Id", nomatch = NULL],
      names.cov.data.life = clv.data@names.cov.data.life,
      names.cov.data.trans = clv.data@names.cov.data.trans))
})
