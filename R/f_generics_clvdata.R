setMethod("clv.data.create.new.customer.prediction.data", signature = signature(clv.data="clv.data"), definition = function(clv.data, ids, ...){

  # A valid estimation period has be >1 period long.
  # Therefore need to have min 1 customer which has 2 transactions that are spaced at least 1 period apart
  # We are generous and give every customer 2 transactions: On 2000-01-01 and 2 periods later
  # The actual date of the transactions do not matter (dyncov they have to be at the min cov date)
  # There is no holdout period and no spending information

  tp.transactions <- c(
    as.Date("2000-01-01"),
    as.Date("2000-01-01") + CLVTools:::clv.time.number.timeunits.to.timeperiod(clv.time = clv.data@clv.time, user.number.periods = 2))

  return(
    clvdata(
      # For every given id, create two transactions (without spending info)
      data.transactions = rbindlist(lapply(ids, function(id){
        data.table(Id=c(id, id), Date=tp.transactions)
      })),
      date.format = clv.data@clv.time@time.format,
      time.unit = clv.data@clv.time@name.time.unit,
      estimation.split = NULL,
      name.price = NULL)
  )
})
