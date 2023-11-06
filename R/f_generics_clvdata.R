setMethod("clv.data.create.bootstrapping.data", signature = signature(clv.data="clv.data"), definition = function(clv.data, ids){
  # For bootstrapped fitting, the estimation end has to be the same as in the
  # original clv.data in order to get the same cbs values
  #
  # Create without holdout data as it is not required for bootstrapped fitting and could lead to
  # clvdata with too small holdout period
  #
  # For efficiency the object is created without verifiying the content of the data
  #
  # TODO [test]: sampling all customers yields orignal parameters / same cbs for sampled customers
  # TODO [test]: clv.time keeps all other properties
  # TODO [test]: Works correctly when sampling with replacement

  cl <- match.call(expand.dots = TRUE)


  if(length(setdiff(ids, clv.data@data.transactions[, unique(Id)])) > 0){
    warning("Not all given Ids were found and selected into the new data.", call. = FALSE)
  }

  dt.transactions <- clv.data.select.customer.data.with.duplicates(
    dt.data=clv.data.get.transactions.in.estimation.period(clv.data),
    ids=ids)

  # Repeat transactions are required to fit GG with remove.first.transactions = TRUE
  dt.repeat.trans <- clv.data.select.customer.data.with.duplicates(
    dt.data=clv.data.get.repeat.transactions.in.estimation.period(clv.data),
    ids=ids)


  # Set properties as if no holdout
  # keep last and first transaction the same as in original data, even
  # if the actuals in dt.transactions are different
  clv.t <- clv.time.set.sample.periods(
    clv.time = clv.data@clv.time,
    tp.first.transaction = clv.data@clv.time@timepoint.estimation.start,
    tp.last.transaction = clv.data@clv.time@timepoint.estimation.end,
    user.estimation.end = NULL)

  return(clv.data(
    call = cl,
    data.transactions = dt.transactions,
    data.repeat.trans = dt.repeat.trans,
    has.spending = clv.data.has.spending(clv.data),
    clv.time = clv.t))
})
