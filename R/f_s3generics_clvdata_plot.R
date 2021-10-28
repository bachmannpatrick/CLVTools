#' @title Plot Diagnostics for the Transaction data in a clv.data Object
#'
#' @param x The clv.data object to plot
#' @param which Which plot to produce, either "tracking", "numtrans", "spending" or "interpurchasetime".
#' May be abbreviated but only one may be selected. Defaults to "tracking".
#'
#' @template template_param_verbose
#' @param plot Whether a plot should be created or only the assembled data returned.
#'
#' @param cumulative "tracking": Whether the cumulative actual repeat transactions should be plotted.
# @template template_param_predictionend
#' @param prediction.end "tracking": Until what point in time to plot. This can be the number of periods (numeric) or
#' a form of date/time object. See details.
#'
#' @param trans.bins "numtrans": Vector of integers indicating the number of transactions (x axis) for which the customers should be counted.
#' @param count.repeat.trans "numtrans": Whether repeat transactions (TRUE, default) or all transactions (FALSE) should be counted.
#' @param count.remaining "numtrans": Whether the customers which are not captured with \code{trans.bins} should be counted in a separate last bar.
#' @param label.remaining "numtrans": Label for the last bar, if \code{count.remaining=TRUE}.
#'
#' @param mean.spending "spending": Whether customer's mean spending per transaction (\code{TRUE}, default) or the
#' value of every transaction in the data (\code{FALSE}) should be plotted.
#'
#' @param sample Name of the sample for which the plot should be made, either
#' "estimation", "full", or "holdout". Defaults to "estimation". Not for "tracking".
#' @param color Color of resulting geom object in the plot. Not for "tracking".
#' @param geom The geometric object of ggplot2 to display the data. Forwarded to
#' \link[ggplot2:stat_density]{ggplot2::stat_density}. Not for "tracking" and "numtrans".
#' @param ... Forwarded to \link[ggplot2:stat_density]{ggplot2::stat_density} ("spending", "interpurchasetime")
#' or \link[ggplot2:geom_bar]{ggplot2::geom_bar} ("numtrans"). Not for "tracking".
#'
#'
#'
#' @description
#' Depending on the value of parameter \code{which}, one of the following plots will be produced:
#'
#' \subsection{Tracking Plot}{
#' Plot the aggregated repeat transactions per period over the given time-horizon (\code{prediction.end}).
#' See Details for the definition of plotting periods.
#' }
#'
#' \subsection{Number of Transactions Plot}{
# Plot distribution of the number of customers having the number of transactions.
#' Plot the distribution of transactions or repeat transactions per customer, after aggregating transactions
#' of the same customer on a single time point.
#' Note that if \code{trans.bins} is changed, \code{label.remaining} usually needs to be adapted as well.
#' }
#'
#' \subsection{Spending Plot}{
#' Plot the empirical density of either customer's average spending per transaction or the value
#' of every transaction in the data, after aggregating transactions of the same customer on a single time point.
#' Note that in all cases this includes all transactions and not only repeat-transactions.
#' }
#'
#' \subsection{Interpurchase Time Plot}{
#' Plot the empirical density of customer's mean time (in number of periods) between transactions,
#' after aggregating transactions of the same customer on a single time point.
#' Note that customers without repeat-transactions are removed.
#' }
#'
#' @template template_details_predictionend
#'
#' @details If there are no repeat transactions until \code{prediction.end}, only the time for which there is data
#' is plotted. If the data is returned (i.e. with argument \code{plot=FALSE}), the respective rows
#' contain \code{NA} in column \code{Number of Repeat Transactions}.
#'
#'
#' @seealso \link[ggplot2:stat_density]{ggplot2::stat_density} and \link[ggplot2:geom_bar]{ggplot2::geom_bar}
#' for possible arguments to \code{...}
#' @seealso \link[CLVTools:plot.clv.fitted.transactions]{plot} to plot fitted transaction models
#' @seealso \link[CLVTools:plot.clv.fitted.spending]{plot} to plot fitted spending models
#'
#' @return
#' An object of class \code{ggplot} from package \code{ggplot2} is returned by default.
#' If \code{plot=FALSE}, the data that would have been used to create the plot is returned.
#' Depending on which plot was selected, this is a \code{data.table}
#' which contains some of the following columns:
#' \item{Id}{Customer Id}
#' \item{period.until}{The timepoint that marks the end (up until and including) of the period to which the data in this row refers.}
#' \item{Number of Repeat Transactions}{The number of actual repeat transactions in the period that ends at \code{period.until}.}
#' \item{Spending}{Spending as defined by parameter \code{mean.spending}.}
#' \item{mean.interpurchase.time}{Mean number of periods between transactions per customer,
#' excluding customers with no repeat-transactions.}
#' \item{num.transactions}{The number of (repeat) transactions, depending on \code{count.repeat.trans}.}
#' \item{num.customers}{The number of customers.}
#'
#'
#' @examples
#'
#' data("cdnow")
#' clv.cdnow <- clvdata(cdnow, time.unit="w",estimation.split=37,
#'                      date.format="ymd")
#'
#' ### TRACKING PLOT
#' # Plot the actual repeat transactions
#' plot(clv.cdnow)
#' # same, explicitly
#' plot(clv.cdnow, which="tracking")
#'
#' # plot cumulative repeat transactions
#' plot(clv.cdnow, cumulative=TRUE)
#'
#' # Dont automatically plot but tweak further
#' library(ggplot2) # for ggtitle()
#' gg.cdnow <- plot(clv.cdnow)
#' # change Title
#' gg.cdnow + ggtitle("CDnow repeat transactions")
#'
#' # Dont return a plot but only the data from
#' #   which it would have been created
#' dt.plot.data <- plot(clv.cdnow, plot=FALSE)
#'
#'
#' ### NUM CUSTOMERS WITH NUM TRANSACTION
#' plot(clv.cdnow, which="numtrans")
#'
#' # Bins from 0 to 15, all remaining in bin labelled "16+"
#' plot(clv.cdnow, which="numtrans", trans.bins=0:15,
#'      label.remaining="16+")
#'
#' # Count all transactions, not only repeat
#' #  Note that the bins have to be adapted to start from 1
#' plot(clv.cdnow, which="numtrans", count.repeat.trans = FALSE,
#'      trans.bins=1:9)
#'
#'
#' ### SPENDING DENSITY
#' # plot customer's average transaction value
#' plot(clv.cdnow, which="spending", mean.spending = TRUE)
#'
#' # distribution of the values of every transaction
#' plot(clv.cdnow, which="spending", mean.spending = FALSE)
#'
#'
#' ### INTERPURCHASE TIME DENSITY
#' # plot as small points, in blue
#' plot(clv.cdnow, which="interpurchasetime",
#'      geom="point", color="blue", size=0.02)
#'
#'
#' @importFrom graphics plot
#' @include all_generics.R class_clv_data.R
#' @method plot clv.data
#' @export
plot.clv.data <- function(x, which=c("tracking", "numtrans", "spending", "interpurchasetime"),
                          # tracking plot
                          prediction.end=NULL, cumulative=FALSE,
                          # numtrans
                          trans.bins=0:9, count.repeat.trans=TRUE, count.remaining=TRUE,
                          label.remaining="10+",
                          # spending density
                          mean.spending=TRUE,
                          # all density
                          sample=c("estimation", "full", "holdout"),
                          geom="line", color="black",
                          # all plots
                          plot=TRUE, verbose=TRUE, ...){

  # do not check ggplot inputs (geom, color)
  err.msg <- c()
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=plot, var.name="plot"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))
  err.msg <- c(err.msg, .check_userinput_matcharg(char=which, choices=c("tracking", "numtrans", "spending", "interpurchasetime"),
                                                  var.name="which"))
  check_err_msg(err.msg)

  return(
    switch(EXPR = match.arg(arg=which, choices = c("tracking", "numtrans", "spending", "interpurchasetime"),
                            several.ok = FALSE),
           "tracking" =
             clv.data.plot.tracking(x=x, prediction.end = prediction.end, cumulative = cumulative,
                                    plot = plot, verbose = verbose, ...=...),
         "spending" =
           clv.data.plot.density.spending(x = x, sample=sample, mean.spending = mean.spending,
                                          plot = plot, verbose=verbose,
                                          color = color, geom=geom, ...),
         "interpurchasetime" =
           clv.data.plot.density.interpurchase.time(clv.data = x, sample=sample,
                                                    plot=plot, verbose=verbose,
                                                    color=color, geom=geom, ...),
         "numtrans" =
           clv.data.plot.barplot.numtrans(clv.data = x, sample=sample,
                                          trans.bins=trans.bins,
                                          count.repeat.trans=count.repeat.trans,
                                          label.remaining=label.remaining,
                                          count.remaining=count.remaining,
                                          plot=plot, verbose=verbose,
                                          color=color, ...)
         ))
}

#' @importFrom ggplot2 rel
clv.data.plot.add.theme <- function(p){
  return(p + theme(
    plot.title = element_text(face = "bold", size = rel(1.5)),
    text = element_text(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.background  = element_rect(colour = NA),
    axis.title   = element_text(face = "bold",size = rel(1)),
    axis.title.y = element_text(angle=90,vjust =2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text = element_text(),
    axis.line = element_line(colour="black"),
    axis.ticks = element_line(),
    panel.grid.major = element_line(colour="#d2d2d2"),
    panel.grid.minor = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face="italic"),
    strip.background=element_rect(colour="#d2d2d2",fill="#d2d2d2"),
    strip.text = element_text(face="bold", size = rel(0.8))))
}


clv.data.plot.tracking <- function(x, prediction.end, cumulative, plot, verbose, ...){

  period.until <- period.num <- NULL

  # This is nearly the same as plot.clv
  #   However, creating a single plotting controlflow leads to all kinds of side effects and special cases.
  #   Because there are only 2 functions that would profit, it was decided to leave it in their own separate
  #   functions. (It is only the Rule of three ("Three strikes and you refactor"), not the Rule of two)

  # Check inputs ------------------------------------------------------------------------------------------------------
  err.msg <- c()
  err.msg <- c(err.msg, check_user_data_emptyellipsis(...))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=cumulative, var.name="cumulative"))
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

clv.data.make.density.plot <- function(dt.data, mapping, labs_x, title, geom, ...){

  p <- ggplot(data = dt.data) + stat_density(mapping = mapping, geom = geom, ...)

  # Axis and title
  p <- p + labs(x = labs_x, y="Density", title=title)

  return(clv.data.plot.add.theme(p))
}

#' @importFrom ggplot2 aes
clv.data.plot.density.spending <- function(x, sample, mean.spending, plot, verbose, color, geom, ...){
  Id <- Price <- Spending <- NULL

  # only check non-ggplot inputs
  # sample is checked in select.sample.data
  check_err_msg(.check_user_data_single_boolean(mean.spending, var.name="mean.spending"))

  # get transaction data data
  dt.trans <- clv.data.select.sample.data(clv.data = x, sample = sample, choices=c("estimation", "full", "holdout"))

  # Calculate spending
  if(mean.spending){
    dt.spending <- dt.trans[, list(Spending = mean(Price)), by="Id"]
    title  <- "Density of Average Transaction Value"
    labs_x <- "Average Value per Transaction"
  }else{
    dt.spending <- dt.trans[, list(Spending = Price, Id)]
    title  <- "Density of Transaction Value"
    labs_x <- "Value per Transaction"
  }

  if(plot){
    return(clv.data.make.density.plot(dt.data = dt.spending,
                                      mapping = aes(x = Spending),
                                      labs_x = labs_x, title = title,
                                      # pass to stat_density
                                      geom = geom, color=color, ...))
  }else{
    return(dt.spending)
  }
}

#' @importFrom ggplot2 aes_string
clv.data.plot.density.interpurchase.time <- function(clv.data, sample,
                                                     plot, verbose, color, geom, ...){
  interp.time <- NULL
  dt.trans <- clv.data.select.sample.data(clv.data=clv.data, sample=sample, choices=c("estimation", "full", "holdout"))

  # interpurchase time in given period
  dt.mean.interp <- clv.data.mean.interpurchase.times(clv.data=clv.data, dt.transactions=dt.trans)

  # only such with repeat-transaction
  dt.mean.interp <- dt.mean.interp[!is.na(interp.time)]
  setcolorder(dt.mean.interp, c("Id", "interp.time"))
  setnames(dt.mean.interp, old="interp.time", new="mean.interpurchase.time")

  labs_x <- paste0("Mean Interpurchase Time (",clv.data@clv.time@name.time.unit,")")

  if(plot){
    return(clv.data.make.density.plot(dt.data = dt.mean.interp,
                                      mapping = aes_string(x = "mean.interpurchase.time"),
                                      labs_x = labs_x,
                                      title = "Density of Customer's Mean Time between Transactions",
                                      geom = geom, color = color, ...))
  }else{
    return(dt.mean.interp)
  }
}


#' @importFrom ggplot2 ggplot geom_col aes_string labs geom_text position_dodge rel
clv.data.plot.barplot.numtrans <- function(clv.data, count.repeat.trans, count.remaining, label.remaining, trans.bins,
                                           sample, plot, verbose, color, ...){
  x <- num.customers <- num.transactions <- NULL

  err.msg <- c()
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=count.repeat.trans, var.name="count.repeat.trans"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=count.remaining,    var.name="count.remaining"))
  err.msg <- c(err.msg, .check_userinput_single_character(char=label.remaining, var.name="label.remaining"))
  err.msg <- c(err.msg, check_user_data_emptyellipsis(...))
  check_err_msg(err.msg) # count.repeat.trans has to be checked first
  check_err_msg(check_userinput_datanocov_transbins(trans.bins=trans.bins, count.repeat.trans=count.repeat.trans))


  # Number of customers which have the given number of transactions
  trans.bins <- unique(trans.bins)
  dt.bins <- data.table(x = trans.bins)

  dt.trans <- clv.data.select.sample.data(clv.data=clv.data, sample=sample, choices=c("estimation", "full", "holdout"))

  dt.num.trans <- dt.trans[, list(x = .N), by="Id"]

  if(count.repeat.trans){
    dt.num.trans[, x := x-1]
    x.lab <- "Number of Repeat Transactions"
  }else{
    x.lab <- "Number of Transactions"
  }

  # count how many customers had how many num trans
  dt.num.cust.by.trans <- dt.num.trans[, list(num.customers = .N), by="x"]

  # add how many customers made x num trans
  #   NA where does not match, set 0 to keep
  dt.bins <- dt.bins[dt.num.cust.by.trans, num.customers := num.customers, on="x"]
  dt.bins[is.na(num.customers), num.customers := 0]
  setnames(dt.bins, "x", "num.transactions")
  # Make char to bind together with "remaining"
  #   get levels order before turning into character
  levels.bins <- dt.bins[, sort(num.transactions)]
  dt.bins[, num.transactions := as.character(num.transactions)]

  if(count.remaining){
    # find all num.trans which are not already in bins
    dt.remaining <- dt.num.cust.by.trans[!(x %in% dt.bins[, as.numeric(num.transactions)])]
    dt.remaining <- dt.remaining[, list(num.transactions=label.remaining, num.customers = sum(num.customers))]
    dt.bins <- rbindlist(list(dt.bins, dt.remaining))
    # add remaining as last/highest level
    levels.bins <- c(levels.bins, label.remaining)
  }

  # turn into ordered category for plotting
  dt.bins[, num.transactions := factor(num.transactions, levels = levels.bins, ordered = TRUE)]

  if(!plot){
    return(dt.bins)
  }else{
    #   use geom_col because geom_bar does the counting itself
    p <- ggplot(data = dt.bins) + geom_col(mapping = aes_string(x="num.transactions",
                                                                y="num.customers"),
                                           fill=color,
                                           width = 0.5,
                                           show.legend = FALSE)
    # Axis and title
    p <- p + labs(x = x.lab, y="Number of Customers",
                  # Number of Customers per Number of Transactions
                  title="Distribution of Transaction Count")

    # add count annotation
    p <- p + geom_text(aes_string(label = "num.customers",
                                  x = "num.transactions", y = "num.customers"),
                       position = position_dodge(width = 0.8),
                       vjust = -0.6,
                       size = rel(3))

    return(clv.data.plot.add.theme(p))
  }
}


