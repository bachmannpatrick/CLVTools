#' @include class_clv_fitted_staticcov.R
setValidity(Class = "clv.fitted.static.cov", method = function(object){
  # covariates: coef named with prefix
  # do not callNext

  # estimation not finished, yet (ie after new())
  if(NROW(object@optimx.estimation.output) == 0)
    return(TRUE)

  err.msg <- c()

  # names.original.params.free.life --------------------------------------------------------------------
  if(!all(object@names.original.params.free.life %in% colnames(object@clv.data@data.cov.life)))
    err.msg <- c(err.msg, "All names.original.params.free.life have to be column names in data.cov.life")

  if(anyNA(object@names.original.params.free.life))
    err.msg <- c(err.msg, "names.original.params.free.life may not contain any NAs (should be character(0))")

  if(length(object@names.original.params.free.life) != length(object@names.prefixed.params.free.life))
    err.msg <- c(err.msg, "there is a mismatch between original and prefixed names for free.life")

  # names.original.params.free.trans --------------------------------------------------------------------
  if(!all(object@names.original.params.free.trans %in% colnames(object@clv.data@data.cov.trans)))
    err.msg <- c(err.msg, "All names.original.params.free.trans have to be column names in data.cov.trans")

  if(anyNA(object@names.original.params.free.trans))
    err.msg <- c(err.msg, "names.original.params.free.trans may not contain any NAs (should be character(0))")

  if(length(object@names.original.params.free.trans) != length(object@names.prefixed.params.free.trans))
    err.msg <- c(err.msg, "there is a mismatch between original and prefixed names for free.trans")

  # names.original.params.constr -------------------------------------------------------------------------

  if(anyNA(object@names.original.params.constr))
    err.msg <- c(err.msg, "names.original.params.constr may not contain any NAs (should be character(0))")

  if(!all(object@names.original.params.constr %in% colnames(object@clv.data@data.cov.life)))
    err.msg <- c(err.msg, "names.original.params.constr need to be in data.cov.life")

  if(!all(object@names.original.params.constr %in% colnames(object@clv.data@data.cov.trans)))
    err.msg <- c(err.msg, "names.original.params.constr need to be in data.cov.trans")

  if(length(object@names.original.params.constr) != length(object@names.prefixed.params.constr))
    err.msg <- c(err.msg, "there is a mismatch between original and prefixed names for constraints")

  if(length(object@names.original.params.free.trans) == 0 & length(object@names.original.params.constr) == 0)
    err.msg <- c(err.msg, "Too few names.original.params.free.trans or names.original.params.constr")

  if(length(object@names.original.params.free.life) == 0 & length(object@names.original.params.constr) == 0)
    err.msg <- c(err.msg, "Too few names.original.params.free.life or names.original.params.constr")

  if(!all(object@names.original.params.constr %in% colnames(object@clv.data@data.cov.life)))
    err.msg <- c(err.msg, "Not all names.original.params.constr are in life covariates data")

  if(!all(object@names.original.params.constr %in% colnames(object@clv.data@data.cov.trans)))
    err.msg <- c(err.msg, "Not all names.original.params.constr are in trans covariates data")

  if(!all(sort(c(object@names.original.params.free.life, object@names.original.params.constr)) ==
          sort(setdiff(colnames(object@clv.data@data.cov.life), "Id"))))
    err.msg <- c(err.msg, "Combined free and constr life params do not match colnames data.cov.life")

  if(!all(sort(c(object@names.original.params.free.trans, object@names.original.params.constr)) ==
          sort(setdiff(colnames(object@clv.data@data.cov.trans), "Id"))))
    err.msg <- c(err.msg, "Combined free and constr trans params do not match colnames data.cov.trans")

  # names.prefixed.params.free.life --------------------------------------------------------------------
  if(anyNA(object@names.prefixed.params.free.life))
    err.msg <- c(err.msg, "names.prefixed.params.free.life may not contain any NAs (should be character(0))")


  if(!all(object@names.prefixed.params.free.life %in% colnames(coef(object@optimx.estimation.output))))
    err.msg <- c(err.msg, "Not all names.prefixed.params.free.life in optimx results")

  # names.prefixed.params.free.trans --------------------------------------------------------------------
  if(anyNA(object@names.prefixed.params.free.trans))
    err.msg <- c(err.msg, "names.prefixed.params.free.trans may not contain any NAs (should be character(0))")

  if(!all(object@names.prefixed.params.free.trans %in% colnames(coef(object@optimx.estimation.output))))
    err.msg <- c(err.msg, "Not all names.prefixed.params.free.trans in optimx results")

  # names.prefixed.params.constr ------------------------------------------------------------------------
  if(anyNA(object@names.prefixed.params.constr))
    err.msg <- c(err.msg, "names.prefixed.params.constr may not contain any NAs (should be character(0))")

  if(length(intersect(object@names.prefixed.params.free.trans, object@names.prefixed.params.constr)) > 0)
    err.msg <- c(err.msg, "Overlap between names.prefixed.params.free.trans and names.prefixed.params.free.trans")

  if(length(intersect(object@names.prefixed.params.free.life, object@names.prefixed.params.constr)) > 0)
    err.msg <- c(err.msg, "Overlap between names.prefixed.params.constr and names.prefixed.params.free.life")

  if(length(object@names.prefixed.params.free.trans) == 0 & length(object@names.prefixed.params.constr) == 0)
    err.msg <- c(err.msg, "Too few names.prefixed.params.free.trans or names.prefixed.params.constr")

  if(length(object@names.prefixed.params.free.life) == 0 & length(object@names.prefixed.params.constr) == 0)
    err.msg <- c(err.msg, "Too few names.prefixed.params.free.life or names.prefixed.params.constr")


  # names.prefixed.params.after.constr.life ------------------------------------------------------------
  if(length(object@names.prefixed.params.after.constr.life) < ncol(object@clv.data@data.cov.life)-1) # -1 for Id
    err.msg <- c(err.msg, "Too few names.prefixed.params.after.constr.life")

  if(length(object@names.prefixed.params.after.constr.trans) < ncol(object@clv.data@data.cov.trans)-1)
    err.msg <- c(err.msg, "Too few names.prefixed.params.after.constr.trans")

  if(length(object@names.prefixed.params.after.constr.life) !=
     length(object@names.prefixed.params.free.life) + length(object@names.prefixed.params.constr))
    err.msg <- c(err.msg, "names.prefixed.params.after.constr.life does not have the correct length")

  if(length(object@names.prefixed.params.after.constr.trans) !=
     length(object@names.prefixed.params.free.trans) + length(object@names.prefixed.params.constr))
    err.msg <- c(err.msg, "names.prefixed.params.after.constr.trans does not have the correct length")

  # names.prefixed.params.after.constr.life/trans dont need to be same length

  # estimation.used.constraints ------------------------------------------------------------------------
  if(length(object@estimation.used.constraints) > 1)
    err.msg <- c(err.msg, "estimation.used.constraints too long")

  if(length(object@estimation.used.constraints)>0)
    if(object@estimation.used.constraints){
      if(length(object@names.prefixed.params.constr) == 0)
        err.msg <- c(err.msg, "Prefixed names not set for constraint params")

      if(length(object@names.original.params.constr) == 0)
        err.msg <- c(err.msg, "Original names not set for constraint params")
    }else{
      if(length(object@names.prefixed.params.constr) != 0)
        err.msg <- c(err.msg, "Prefixed names set for constraint params but not using constraints")

      if(length(object@names.original.params.constr) != 0)
        err.msg <- c(err.msg, "Prefixed names set for constraint params but not using constraints")
    }

  # estimation.used.regularization, reg.lambda.life, reg.lambda.trans ----------------------------------
  if(length(object@estimation.used.regularization)>1)
    err.msg <- c(err.msg, "estimation.used.regularization too long")

  if(length(object@estimation.used.regularization)>0)
    if(object@estimation.used.regularization == TRUE){
      if(anyNA(object@reg.lambda.life))
        err.msg <- c(err.msg, "reg.lambda.life NA but using regularization")

      if(anyNA(object@reg.lambda.trans))
        err.msg <- c(err.msg, "reg.lambda.trans NA but using regularization")

      if(length(object@reg.lambda.life) == 0)
        err.msg <- c(err.msg, "reg.lambda.life not set but using reguarlization")

      if(length(object@reg.lambda.trans) == 0)
        err.msg <- c(err.msg, "reg.lambda.trans not set but using reguarlization")

      if(object@reg.lambda.trans < 0)
        err.msg <- c(err.msg, "reg.lambda.trans smaller 0")
      if(object@reg.lambda.life < 0)
        err.msg <- c(err.msg, "reg.lambda.life smaller 0")

    }else{
      if(length(object@reg.lambda.life) > 0)
        err.msg <- c(err.msg, "reg.lambda.life set but not using regularization")

      if(length(object@reg.lambda.trans) > 0)
        err.msg <- c(err.msg, "reg.lambda.trans set but not using regularization")
    }

  if(length(object@reg.lambda.life)>1)
    err.msg <- c(err.msg, "reg.lambda.life too long")
  if(length(object@reg.lambda.trans)>1)
    err.msg <- c(err.msg, "reg.lambda.trans too long")


  # prediction.params.life, prediction.params.trans ----------------------------------------------------
  if(length(object@prediction.params.life)>0 |
     length(object@prediction.params.trans)>0){
    # in prediction mode

    if(length(object@prediction.params.life) != ncol(object@clv.data@data.cov.life)-1)
      err.msg <- c(err.msg, "Number of prediction.params.life does not match number of covariates")

    if(length(object@prediction.params.trans) != ncol(object@clv.data@data.cov.trans)-1)
      err.msg <- c(err.msg, "Number of prediction.params.trans does not match number of covariates")

    if(!all(names(object@prediction.params.life) %in% colnames(object@clv.data@data.cov.life)))
      err.msg <- c(err.msg, "Not all life prediction param names in cov data")

    if(!all(names(object@prediction.params.trans) %in% colnames(object@clv.data@data.cov.trans)))
      err.msg <- c(err.msg, "Not all trans prediction param names in cov data")

    if(length(object@estimation.used.constraints)>0)
      if(object@estimation.used.constraints){
        if(!all(object@names.original.params.constr %in% names(object@prediction.params.trans)))
          err.msg <- c(err.msg, "Not all constraint param original names are in prediction.params.trans")

        if(!all(object@names.original.params.constr %in% names(object@prediction.params.life)))
          err.msg <- c(err.msg, "Not all constraint param original names are in prediction.params.life")

        last.optimx.coef <- tail(coef(object@optimx.estimation.output), n = 1)[1,]
        if(!all(last.optimx.coef[object@names.prefixed.params.constr] %in%
                object@prediction.params.life)) # constr values in pred params
          err.msg <- c(err.msg, "Not all constraint param values are in prediction.params.life")

        if(!all(last.optimx.coef[object@names.prefixed.params.constr] %in%
                object@prediction.params.trans)) # constr values in pred params
          err.msg <- c(err.msg, "Not all constraint param values are in prediction.params.trans")
      }else{
        if(length(object@prediction.params.life) != length(object@names.prefixed.params.free.life))
          err.msg <- c(err.msg, "Not all estimated free life params in predicition params")

        if(length(object@prediction.params.trans) != length(object@names.prefixed.params.free.trans))
          err.msg <- c(err.msg, "Not all estimated free trans params in predicition params")

        if(!all(sort(names(object@names.original.params.free.life)) == sort(names(object@prediction.params.life))))
          err.msg <- c(err.msg, "prediction.params.life does not have same names as original free life params")

        if(!all(sort(names(object@names.original.params.free.trans)) == sort(names(object@prediction.params.trans))))
          err.msg <- c(err.msg, "prediction.params.trans does not have same names as original free trans params")
      }

    # prediction.params dont need to have the same length for trans and life
  }




  #'   # Functional checks ----------------------------------------------------------------------------------
  #'     # Coefs for covariates have to be prefixed names to distinguish
  #'     if(!all(object@names.prefixed.params.free.life %in% names(coef(object))))
  #'       err.msg <- c(err.msg, "Not all names.prefixed.params.free.life in coef()")
  #'
  #'     if(!all(object@names.prefixed.params.free.trans %in% names(coef(object))))
  #'       err.msg <- c(err.msg, "Not all names.prefixed.params.free.trans in coef()")
  #'
  #'     if(!all(object@names.prefixed.params.constr %in% names(coef(object))))
  #'       err.msg <- c(err.msg, "Not all names.prefixed.params.constr in coef()")
  #'
  #'     # same for vcov names
  #'     # if(!all(object@names.prefixed.params.free.life %in% rownames(vcov(object))))
  #'     #   err.msg <- c(err.msg, "Not all names.prefixed.params.free.life in vcov()")
  #'     #
  #'     # if(!all(object@names.prefixed.params.free.trans %in% rownames(vcov(object))))
  #'     #   err.msg <- c(err.msg, "Not all names.prefixed.params.free.trans in vcov()")
  #'     #
  #'     # if(!all(object@names.prefixed.params.constr %in% rownames(vcov(object))))
  #'     #   err.msg <- c(err.msg, "Not all names.prefixed.params.constr in vcov()")

  if(length(err.msg)>0)
    return(err.msg)
  else
    return(TRUE)
  })
