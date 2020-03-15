setMethod("clv.controlflow.plot.check.inputs", signature(obj="clv.fitted"), function(obj, prediction.end, cumulative, plot, label.line, verbose){
  # Empty fallback method.
  #   clv.controlflow.plot.check.inputs is needed for fitted.dyncov models only to check dyncov length
})


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
