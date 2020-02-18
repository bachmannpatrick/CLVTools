#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.get.matrix.data.cov.life", signature = signature(clv.data="clv.data.static.covariates"),definition = function(clv.data){
  return(data.matrix(clv.data@data.cov.life[, .SD, .SDcols=clv.data@names.cov.data.life]))
})


#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.get.matrix.data.cov.trans", signature = signature(clv.data="clv.data.static.covariates"),definition = function(clv.data){
  return(data.matrix(clv.data@data.cov.trans[, .SD, .SDcols=clv.data@names.cov.data.trans]))
})

#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.get.names.cov.life", signature = signature(clv.data="clv.data.static.covariates"),definition = function(clv.data){
  return(clv.data@names.cov.data.life)
})

#' @include all_generics.R class_clv_data_static_covariates.R
setMethod("clv.data.get.names.cov.trans", signature = signature(clv.data="clv.data.static.covariates"),definition = function(clv.data){
  return(clv.data@names.cov.data.trans)
})
