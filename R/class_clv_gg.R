#' @templateVar name_model_full Gamma-Gamma
#' @templateVar name_class_clvmodel clv.model.gg
#' @template template_class_clvfittedspendingmodels
#'
#' @slot cbs Single \code{data.table} that contains information about
#' each customers' mean spending per transaction (Spending) and the number of purchases (x) which
#' both depend on whether the first transaction was removed or not.
#'
#' @seealso \linkS4class{clv.fitted}, \linkS4class{clv.fitted.spending}, \linkS4class{clv.model.gg}
#'
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_gg.R class_clv_fitted_spending.R
setClass(Class = "clv.gg", contains = "clv.fitted.spending",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))


#' @importFrom methods new
clv.gg <- function(cl, clv.data, remove.first.transaction){

  dt.cbs.gg   <- gg_cbs(clv.data = clv.data, remove.first.transaction = remove.first.transaction)
  clv.model   <- clv.model.gg()

  return(new("clv.gg",
             clv.fitted.spending(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.gg))
}

gg_cbs <- function(clv.data, remove.first.transaction){
  Date <- Price <- x <- i.x <- Spending <- i.Spending <- NULL

  # Customer-By-Sufficiency (CBS) Matrix
  #   Only for transactions in calibration period
  #   After first transaction was removed (if required)
  #   For every customer:
  #     x:        Number of transactions
  #     Spending: Average (mean) spending per transaction

  dt.transactions <- clv.data.get.transactions.in.estimation.period(clv.data = clv.data)


  # Removing the first transaction and then counting transactions and spending on it, will
  #   lose customers. Therefore do in separate steps: Id of all, then match their data

  if(!remove.first.transaction){
    # Ordinary approach is ok because will not lose Ids
    cbs <- dt.transactions[ , list(x         = .N,
                                   Spending  = mean(Price)),
                            by="Id"]
  }else{
    # Ensure all Ids are kept in cbs
    cbs <- unique(dt.transactions[, "Id"])

    # Add statistics based on repeat transactions only
    #   Cannot use clv.data@data.repeat.trans because these also include holdout
    dt.transactions        <- clv.data.get.transactions.in.estimation.period(clv.data)
    dt.repeat.transactions <- clv.data.make.repeat.transactions(dt.transactions)

    dt.stats.repeat.trans <- dt.repeat.transactions[ , list(x         = .N,
                                                            Spending  = mean(Price)),
                                                     keyby="Id"]

    cbs[dt.stats.repeat.trans, x        := i.x,        on = "Id"]
    cbs[dt.stats.repeat.trans, Spending := i.Spending, on = "Id"]

    # Zero-repeaters have no spending and repeat-transactions
    cbs[is.na(x),        x        := 0]
    cbs[is.na(Spending), Spending := 0]
  }

  setcolorder(cbs, c("Id", "x", "Spending"))
  setkeyv(cbs, "Id")

  return(cbs)
}
