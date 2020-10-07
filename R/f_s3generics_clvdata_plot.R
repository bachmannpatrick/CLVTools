#' @title Plot actual repeat transactions
#' @param x The clv data object to plot
#' @param cumulative Whether the cumulative actual repeat transactions should be plotted.
#' @param plot Whether a plot should be created or only the assembled data returned.
#' @template template_param_predictionend
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#' Plots the actual repeat transactions for the given CLV data object.
#'
#' @template template_details_predictionend
#'
#' @details If there are no repeat transactions until \code{prediction.end}, only the time for which there is data
#' is plotted. If the data is returned (i.e. with argument \code{plot=FALSE}), the respective rows
#' contain \code{NA} in column \code{Number of Repeat Transactions}.
#'
#' @return
#' An object of class \code{ggplot} from package \code{ggplot2} is returned by default.
#' If the parameter \code{plot} is \code{FALSE}, the data that would have been melted and used to
#' create the plot is returned. It is a \code{data.table} which contains the following columns:
#' \item{period.until}{The timepoint that marks the end (up until and including) of the period to which the data in this row refers.}
#' \item{Number of Repeat Transactions}{The number of actual repeat transactions in
#' the period that ends at \code{period.until}.}
#'
#'
#' @examples
#'
#' data("cdnow")
#' clv.data.cdnow <- clvdata(cdnow, time.unit="w",
#'                           estimation.split=37,
#'                           date.format="ymd")
#'
#' # Plot the actual repeat transactions
#' plot(clv.data.cdnow)
#'
#' # plot cumulative repeat transactions
#' plot(clv.data.cdnow, cumulative=TRUE)
#'
#' # Dont automatically plot but tweak further
#' gg.cdnow <- plot(clv.data.cdnow)
#'
#' # change Title
#' library(ggplot2)
#' gg.cdnow + ggtitle("CDnow repeat transactions")
#'
#' # Dont return a plot but only the data from
#' #   which it would have been created
#' dt.plot.data <- plot(clv.data.cdnow, plot=FALSE)
#'
#'
#' @importFrom graphics plot
#' @include all_generics.R class_clv_data.R
#' @method plot clv.data
#' @export
plot.clv.data <- function(x, prediction.end=NULL, cumulative=FALSE, plot=TRUE, verbose=TRUE, ...){

  period.until <- period.num <- NULL

  # This is nearly the same as plot.clv
  #   However, creating a single plotting controlflow leads to all kinds of side effects and special cases.
  #   Because there are only 2 functions that would profit, it was decided to leave it in their own separate
  #   functions. (It is only the Rule of three ("Three strikes and you refactor"), not the Rule of two)

  # Check inputs ------------------------------------------------------------------------------------------------------
  err.msg <- c()
  err.msg <- c(err.msg, check_user_data_emptyellipsis(...))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=cumulative, var.name="cumulative"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=plot, var.name="plot"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))
  err.msg <- c(err.msg, check_user_data_predictionend(clv.fitted=x, prediction.end=prediction.end))
  check_err_msg(err.msg)


  # Define time period to plot -----------------------------------------------------------------------------------------
  # Use table with exactly defined periods as reference to save the repeat transactions
  # End date further than transactions:
  #   If there are not enough transactions for all dates, they are set to NA (= not plotted)

  dt.dates.expectation <- clv.time.expectation.periods(clv.time = x@clv.time, user.tp.end = prediction.end)

  tp.data.start <- dt.dates.expectation[, min(period.until)]
  tp.data.end   <- dt.dates.expectation[, max(period.until)]

  if(verbose)
    message("Plotting from ", tp.data.start, " until ", tp.data.end, ".")

  if(clv.data.has.holdout(x)){
    if(tp.data.end < x@clv.time@timepoint.holdout.end){
      warning("Not plotting full holdout period.", call. = FALSE, immediate. = TRUE)
    }
  }else{
    if(tp.data.end < x@clv.time@timepoint.estimation.end){
      warning("Not plotting full estimation period.", call. = FALSE, immediate. = TRUE)
    }
  }


  # Get repeat transactions ----------------------------------------------------------------------------------------
  label.transactions <- "Number of Repeat Transactions"
  dt.repeat.trans <- clv.controlflow.plot.get.data(obj=x, dt.expectation.seq=dt.dates.expectation,
                                                   cumulative=cumulative, verbose=verbose)
  setnames(dt.repeat.trans, old = "num.repeat.trans", new = label.transactions)


  # Plot data, if needed --------------------------------------------------------------------------------------------

  # Merge data for plotting
  #   To be sure to have all dates, merge data on original dates
  dt.dates.expectation[, period.num := NULL]
  dt.dates.expectation[dt.repeat.trans, (label.transactions) := get(label.transactions), on="period.until"]
  dt.plot <- dt.dates.expectation

  # data.table does not print when returned because it is returned directly after last [:=]
  # " if a := is used inside a function with no DT[] before the end of the function, then the next
  #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
  #   will print. To avoid this: include a DT[] after the last := in your function."
  dt.plot[]

  # Only if needed
  if(!plot)
    return(dt.plot)

  # Plot table with formatting, label etc
  line.colors <- setNames(object = "black", nm = label.transactions)
  return(clv.controlflow.plot.make.plot(dt.data = dt.plot, clv.data = x, line.colors = line.colors))

}


