setMethod("clv.controlflow.check.newdata", signature(clv.fitted="clv.fitted"), definition = function(clv.fitted, user.newdata, prediction.end){
  err.msg <- c()

  # Check newdata
  if(!is(object = user.newdata, class2 = "clv.data")){
    # This also catches NULL, NA, empty vecs, and so on
    #   but allows all cov data subclasses

    err.msg <- c(err.msg, paste0("The parameter newdata needs to be a clv data object of class ",
                                 class(clv.fitted@clv.data)))

  }else{
    # Is actually a clv.data object. Also check if it is the right type
    # Check if the provided newdata is of the exact same class as the currently
    #   stored data object.
    # Cannot use is() because subclasses are recognized as well
    #   (ie clv.data.static.cov is recognized as clv.data)
    # Use extends with fullInfo=TRUE. If it is the exact same class, no distance object is
    #   returned but only TRUE. Use isTRUE to check for equality because can also return non boolean
    if(!isTRUE(extends(class1 = class(clv.fitted@clv.data), class2 = class(user.newdata), fullInfo = TRUE)))
      err.msg <- c(err.msg, paste0("An object of class ", class(clv.fitted@clv.data),
                                   " needs to be supplied for parameter newdata."))
  }

  check_err_msg(err.msg)
})

setMethod("clv.controlflow.check.newdata", signature(clv.fitted="clv.fitted.static.cov"), definition = function(clv.fitted, user.newdata, prediction.end){
  # Do nocov newdata checks first
  #   newdata fulfills all basic properties after this
  callNextMethod()

  err.msg <- c()

  # Check that it does have the same covariates as the ones used for fitting
  #   nocov already checked that is of correct class
  if(!all(sort(user.newdata@names.cov.data.life) == sort(clv.fitted@clv.data@names.cov.data.life)))
    err.msg <- c(err.msg, "Not all Lifetime covariates used for fitting are present in the 'newdata' object!")

  if(!all(sort(user.newdata@names.cov.data.trans) == sort(clv.fitted@clv.data@names.cov.data.trans)))
    err.msg <- c(err.msg, "Not all Transaction covariates used for fitting are present in the 'newdata' object!")

  check_err_msg(err.msg)
})

setMethod("clv.controlflow.check.newdata", signature(clv.fitted="clv.fitted.dynamic.cov"), definition = function(clv.fitted, user.newdata, prediction.end){
  # Do static cov (and hence also nocov) inputchecks first for newdata
  callNextMethod()

  # prediction.end needs to be ok to work with it
  check_err_msg(check_user_data_predictionend(obj=clv.fitted, prediction.end=prediction.end))

  err.msg <- c()

  # Check that dyncov covariate is long enough for prediction end
  #   Convert prediction.end already for this
  #   newdata will replace existing data therefore check its cov
  #     Also clv.time in newdata has to be used for conversion of prediction.end
  dt.prediction <- clv.time.get.prediction.table(clv.time = user.newdata@clv.time,
                                                 user.prediction.end = prediction.end)

  tp.last.required.cov.period <- clv.time.floor.date(clv.time = user.newdata@clv.time,
                                                     timepoint = dt.prediction[1, period.last])

  # only need to check one cov data, guaranteed that both are same length
  if(tp.last.required.cov.period > user.newdata@data.cov.trans[, max(Cov.Date)])
    err.msg <- c(err.msg, "The dynamic covariates in parameter newdata are not long enough for the given parameter prediction.end!")

  check_err_msg(err.msg)
})




