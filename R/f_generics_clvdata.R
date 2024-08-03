setMethod("clv.data.create.bootstrapping.data", signature = signature(clv.data="clv.data"), definition = function(clv.data, ids){
  Id <- NULL

  # For bootstrapped fitting, the estimation end has to be the same as in the
  # original clv.data in order to get the same cbs values
  #
  # Create with holdout if clv.data has holdout.
  # While it is not required for bootstrapped fitting, it is required for
  # evaluating holdout performance and for plotting (CIs).
  # To create an object with holdout period, preserve the periods of original
  # clv.time in order to a) make predictions by default to same end and b) not
  # accidentally have not enough data in holdout period. Hence, keep the last
  # and first transaction the same as in original data, even if the actuals
  # in dt.transactions are different.
  #
  # For efficiency the object is created without verifying the content of the data

  cl <- match.call(expand.dots = TRUE)


  if(length(setdiff(ids, clv.data@data.transactions[, unique(Id)])) > 0){
    warning("Not all given Ids were found and selected into the new data.", call. = FALSE)
  }

  dt.transactions <- clv.data.select.customer.data.duplicating.ids(
    dt.data=clv.data@data.transactions,
    ids=ids)

  # Repeat transactions are required to fit GG with remove.first.transactions = TRUE
  dt.repeat.trans <- clv.data.select.customer.data.duplicating.ids(
    dt.data=clv.data@data.repeat.trans,
    ids=ids)

  return(clv.data(
    call = cl,
    data.transactions = dt.transactions,
    data.repeat.trans = dt.repeat.trans,
    has.spending = clv.data.has.spending(clv.data),
    clv.time = copy(clv.data@clv.time)))
})
