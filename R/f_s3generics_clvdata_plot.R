#' @title Plot Diagnostics for the Transaction data in a clv.data Object
#'
#' @param x The clv.data object to plot
#' @param which Which plot to produce, either "tracking" or "spending". May be abbreviated
#' but only one may be selected. Defaults to "tracking".
#' @param cumulative "tracking": Whether the cumulative actual repeat transactions should be plotted.
#' @param plot Whether a plot should be created or only the assembled data returned.
#' @param sample Name of the sample for which the plot should be made. Defaults to "estimation". Not for "tracking".
# @template template_param_predictionend
#' @param prediction.end "tracking": Until what point in time to plot. This can be the number of periods (numeric) or
#' a form of date/time object. See details.
#' @template template_param_verbose
#' @param mean.spending "spending": Whether customer's mean spending per transaction (\code{TRUE}, default) or the
#' value of every transaction in the data (\code{FALSE}) should be plotted.
#' @param color Color of resulting geom object in the plot. Not for "tracking".
#' @param geom The geometric object of ggplot2 to display the data. Forwarded to
#' \link[ggplot2:stat_density]{ggplot2::stat_density}. Not for "tracking".
#' @param ... Forwarded to \link[ggplot2:stat_density]{ggplot2::stat_density}. Not for "tracking".
#'
#' @seealso \link[ggplot2:stat_density]{ggplot2::stat_density} for possible arguments to \code{...}
#' @seealso \link[CLVTools:gg]{gg} to fit customer's average spending per transaction
#' with the \code{Gamma-Gamma} model
#' @seealso \link[CLVTools:plot.clv.fitted.transactions]{plot} to plot fitted transaction models
#'
#' @description
#' Depending on the value of parameter \code{which}, one of the following plots will be produced:
#'
#' \subsection{Tracking Plot}{
#' Plot the aggregated repeat transactions per period over the given time-horizon (\code{prediction.end}).
#' See Details for the definition of plotting periods.
#' }
#'
#' \subsection{Spending Plot}{
#' Plot the empirical density of either customer's average spending per transaction or the value
#' of every transaction in the data, after aggregating transactions of the same customer on the same day.
#' Note that in all cases this includes all transactions and not only repeat-transactions.
#' }
#'
#' @template template_details_predictionend
#'
#' @details If there are no repeat transactions until \code{prediction.end}, only the time for which there is data
#' is plotted. If the data is returned (i.e. with argument \code{plot=FALSE}), the respective rows
#' contain \code{NA} in column \code{Number of Repeat Transactions}.
#'
#' @return
#' An object of class \code{ggplot} from package \code{ggplot2} is returned by default.
#' If the parameter \code{plot} is \code{FALSE}, the data that would have been used to
#' create the plot is returned. Depending on which plot was selected, this is a \code{data.table}
#' which contains some of the following columns:
#' \item{Id}{Customer Id}
#' \item{period.until}{The timepoint that marks the end (up until and including) of the period to which the data in this row refers.}
#' \item{Number of Repeat Transactions}{The number of actual repeat transactions in the period that ends at \code{period.until}.}
#' \item{Spending}{Spending as defined by parameter \code{mean.spending}.}
#'
#'
#' @examples
#'
#' data("cdnow")
#' clv.data.cdnow <- clvdata(cdnow, time.unit="w",
#'                           estimation.split=37,
#'                           date.format="ymd")
#'
#' ### TRACKING PLOT
#' # Plot the actual repeat transactions
#' plot(clv.data.cdnow)
#' # same, explicitly
#' plot(clv.data.cdnow, which="tracking")
#'
#' # plot cumulative repeat transactions
#' plot(clv.data.cdnow, cumulative=TRUE)
#'
#' # Dont automatically plot but tweak further
#' library(ggplot2) # for ggtitle()
#' gg.cdnow <- plot(clv.data.cdnow)
#' # change Title
#' gg.cdnow + ggtitle("CDnow repeat transactions")
#'
#' # Dont return a plot but only the data from
#' #   which it would have been created
#' dt.plot.data <- plot(clv.data.cdnow, plot=FALSE)
#'
#'
#' ### SPENDING DENSITY
#' # plot customer's average transaction value
#' plot(clv.data.cdnow, which="spending", mean.spending = TRUE)
#'
#' # distribution of the values of every transaction
#' plot(clv.data.cdnow, which="spending", mean.spending = FALSE)
#'
#'
#' @importFrom graphics plot
#' @include all_generics.R class_clv_data.R
#' @method plot clv.data
#' @export
plot.clv.data <- function(x, which=c("tracking", "spending"),
                          # tracking plot
                          prediction.end=NULL, cumulative=FALSE,
                          # density
                          sample=c("estimation", "full", "holdout"),
                          geom="line", color="black",
                          # spending density
                          mean.spending=TRUE,
                          # general
                          plot=TRUE, verbose=TRUE, ...){

  # **** TODO: input checks for which
  # do not check ggplot inputs (geom, color)
  err.msg <- c()
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=plot, var.name="plot"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))
  err.msg <- c(err.msg, .check_userinput_matcharg(char=which, choices=c("tracking", "spending"), var.name="which"))
  check_err_msg(err.msg)

  return(
    switch(EXPR = match.arg(arg=which, choices = c("tracking", "spending"), several.ok = FALSE),
           "tracking" =
             clv.data.plot.tracking(x=x, prediction.end = prediction.end, cumulative = cumulative,
                                    plot = plot, verbose = verbose, ...=...),
         "spending" =
           clv.data.plot.density.spending(x = x, sample=sample, mean.spending = mean.spending,
                                          plot = plot, verbose=verbose,
                                          color = color, geom=geom, ...)))
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

  p <- p + theme(
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
    strip.text = element_text(face="bold", size = rel(0.8)))

  return(p)
}

#' @importFrom ggplot2 aes
clv.data.plot.density.spending <- function(x, sample, mean.spending, plot, verbose, color, geom, ...){
  Price <- Spending <- NULL

  # only check non-ggplot inputs
  # sample is checked in select.sample.data
  err.msg <- c()
  err.msg <- c(err.msg, .check_user_data_single_boolean(mean.spending, var.name="mean.spending"))
  check_err_msg(err.msg)

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
