setMethod("clv.data.create.bootstrapping.data", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data, ids){
  clv.data.no.cov <- callNextMethod()
  ids <- unique(ids)

  return(
    clv.data.static.covariates(
      no.cov.obj = clv.data.no.cov,
      data.cov.life = clv.data@data.cov.life[SJ(Id=ids), on="Id", nomatch = NULL],
      data.cov.trans = clv.data@data.cov.trans[SJ(Id=ids), on="Id", nomatch = NULL],
      names.cov.data.life = clv.data@names.cov.data.life,
      names.cov.data.trans = clv.data@names.cov.data.trans))
})
