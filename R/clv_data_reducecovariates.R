#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.reduce.covariates", signature = signature(clv.data="clv.data.static.covariates"),
definition = function(clv.data, names.cov.life, names.cov.trans){
  # Reduce covariate data to Id + cov names if told by user

  if(length(names.cov.life) != 0 & !identical(names.cov.life, clv.data@names.cov.data.life)){
    clv.data@names.cov.data.life  <- names.cov.life
    clv.data@data.cov.life        <- clv.data@data.cov.life[,  .SD, .SDcols=c("Id", clv.data@names.cov.data.life)]
  }

  if(length(names.cov.trans) !=0 & !identical(names.cov.trans, clv.data@names.cov.data.trans)){
    clv.data@names.cov.data.trans <- names.cov.trans
    clv.data@data.cov.trans       <- clv.data@data.cov.trans[, .SD, .SDcols=c("Id", clv.data@names.cov.data.trans)]
  }
  return(clv.data)
})


