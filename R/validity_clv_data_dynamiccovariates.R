setValidity(Class = "clv.data.dynamic.covariates", method = function(object){
  # do not callNext

  err.msg <- c()
return(TRUE)
  # data.walks.life ------------------------------------------------------------------------------------

  # same number of walks as covariates
  # if(length(object@data.walks.life) != length(object@names.cov.data.life))
  #   err.msg <- c(err.msg, "Need a Walk for every Lifetime covariate.")
  #
  # # Check each Walk to be of correct structure
  # for(i in seq_along(object@data.walks.life)){
  #   if(!is.data.table(object@data.walks.life[[i]]))
  #     err.msg <- c(err.msg, "All walk data need to be of type data.table.")
  #   # Columns Named correctly
  #   # Has columns Id, d, Max.Walk, Num.Walk,
  #   err.msg <- c(err.msg, "All walk data need to be of type data.table.")
  # }

  # Ends and starts on the same date for every Id (important especially since cutting)

  # data.walks.trans -----------------------------------------------------------------------------------
  # **TODO

})
