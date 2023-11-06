setMethod("clv.data.create.bootstrapping.data", signature = signature(clv.data="clv.data"), definition = function(clv.data, ids){
  # For bootstrapped fitting, the estimation end has to be the same as in the
  # original clv.data in order to get the same cbs values
  #
  # Create without holdout data as it is not required for bootstrapped fitting and could lead to
  # clvdata with too small holdout period
  #
  # TODO [test]: sampling all customers yields orignal parameters / same cbs for sampled customers
  # TODO [test]: clv.time keeps all other properties
  # TODO [implement]: When ids appear multiple times in ids (sample with replacement), change their name in the transaction data to have
  #                   them as duplicates. Otherwise they are aggregated to single transactions in clv.data
  # TODO [test]: Works correctly when sampling with replacement

  cl <- match.call(expand.dots = TRUE)
  ids <- unique(ids)

  dt.transactions <- clv.data.get.transactions.in.estimation.period(clv.data)
  dt.transactions <- dt.transactions[SJ(Id=ids), on="Id", nomatch=NULL]

  # Repeat transactions are required to fit GG with remove.first.transactions = TRUE
  dt.repeat.trans <- clv.data.get.repeat.transactions.in.estimation.period(clv.data)
  dt.repeat.trans <- dt.repeat.trans[SJ(Id=ids), on="Id", nomatch=NULL]

  if(dt.transactions[, uniqueN(Id)] != length(ids)){
    warning("Not all given Ids were found and selected into the new data.", call. = FALSE)
  }

  # Set properties as if no holdout
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
