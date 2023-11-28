setMethod("clv.data.create.new.customer.prediction.data", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data, ids, data.cov.life, data.cov.trans, name.id, ...){
  # Use clv.data created as if no cov clv.data
  clv.nocov <- callNextMethod()

  # TODO[Implement]: Subset covariate data to given ids?
  # Only Ids
  # data.cov.life <- data.cov.life[Id %in% ids]
  # data.cov.trans <- data.cov.trans[Id %in% ids]

  return(
    SetStaticCovariates(
      clv.data = clv.nocov,
      data.cov.life = data.cov.life,
      data.cov.trans = data.cov.trans,
      names.cov.life = clv.data@names.cov.data.life,
      names.cov.trans = clv.data@names.cov.data.trans,
      name.id = name.id
    )
  )
})
