#' @title Plot Diagnostics for the Transaction data in a clv.data Object
#'
#' @param x The clv.data object to plot
#' @param which Which plot to produce, either "tracking", "frequency", "spending", "interpurchasetime", or "timings".
#' May be abbreviated but only one may be selected. Defaults to "tracking".
#'
#' @template template_param_verbose
#' @param plot Whether a plot should be created or only the assembled data returned.
#'
#' @param cumulative "tracking": Whether the cumulative actual repeat transactions should be plotted.
#' @templateVar prefix "tracking":
#' @templateVar plot_or_predict plot
#' @template template_param_predictionend
#'
#' @param trans.bins "frequency": Vector of integers indicating the number of transactions (x axis) for which the customers should be counted.
#' @param count.repeat.trans "frequency": Whether repeat transactions (TRUE, default) or all transactions (FALSE) should be counted.
#' @param count.remaining "frequency": Whether the customers which are not captured with \code{trans.bins} should be counted in a separate last bar.
#' @param label.remaining "frequency": Label for the last bar, if \code{count.remaining=TRUE}.
#'
#' @param mean.spending "spending": Whether customer's mean spending per transaction (\code{TRUE}, default) or the
#' value of every transaction in the data (\code{FALSE}) should be plotted.
#'
#' @param Ids "timings": A character vector of customer ids or a single integer specifying the number of customers to sample.
#' Defaults to \code{NULL} for which 50 random customers are selected.
#' @param annotate.ids "timings": Whether timelines should be annotated with customer ids.
#'
#' @param sample Name of the sample for which the plot should be made, either
#' "estimation", "full", or "holdout". Defaults to "estimation". Not for "tracking" and "timing".
#' @param color Color of resulting geom object in the plot. Not for "tracking" and "timing".
#' @param geom "spending" and "interpurchasetime": The geometric object of ggplot2 to display the data. Forwarded to
#' \link[ggplot2:stat_density]{ggplot2::stat_density}.
#' @param ... Forwarded to \link[ggplot2:stat_density]{ggplot2::stat_density} ("spending", "interpurchasetime")
#' or \link[ggplot2:geom_bar]{ggplot2::geom_bar} ("frequency"). Not for "tracking" and "timings".
#'
#'
#'
#' @description
#' Depending on the value of parameter \code{which}, one of the following plots will be produced.
#' Note that the \code{sample} parameter determines the period for which the
#' selected plot is made (either estimation, holdout, or full).
#'
#' \subsection{Tracking Plot}{
#' Plot the aggregated repeat transactions per period over the given time-horizon (\code{prediction.end}).
#' See Details for the definition of plotting periods.
#' }
#'
#' \subsection{Frequency Plot}{
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
#' \subsection{Transaction Timing Plot}{
#' Plot the transaction timings of selected or sampled customers on their respective timelines.
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
#' \item{type}{"timings": Which purpose the value in this row is used for.}
#' \item{variable}{"timings": Coordinate (x or y) for which to use the value in this row for.}
#' \item{value}{"timings": Date or numeric (stored as string)}
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
#' ### FREQUENCY PLOT
#' plot(clv.cdnow, which="frequency")
#'
#' # Bins from 0 to 15, all remaining in bin labelled "16+"
#' plot(clv.cdnow, which="frequency", trans.bins=0:15,
#'      label.remaining="16+")
#'
#' # Count all transactions, not only repeat
#' #  Note that the bins have to be adapted to start from 1
#' plot(clv.cdnow, which="frequency", count.repeat.trans = FALSE,
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
#' ### TIMING PATTERNS
#' # selected customers and annotating them
#' plot(clv.cdnow, which="timings", Ids=c("123", "1041"), annotate.ids=TRUE)
#'
#' # plot 25 random customers
#' plot(clv.cdnow, which="timings", Ids=25)
#'
#' # plot all customers
#' \donttest{\dontrun{
#' plot(clv.cdnow, which="timings", Ids=nobs(clv.cdnow))
#' }}
#'
#' @importFrom graphics plot
#' @include all_generics.R class_clv_data.R
#' @method plot clv.data
#' @export
plot.clv.data <- function(x, which=c("tracking", "frequency", "spending", "interpurchasetime", "timings"),
                          # tracking plot
                          prediction.end=NULL, cumulative=FALSE,
                          # frequency
                          trans.bins=0:9, count.repeat.trans=TRUE, count.remaining=TRUE,
                          label.remaining="10+",
                          # spending density
                          mean.spending=TRUE,
                          # timings
                          annotate.ids=FALSE,
                          Ids=c(),
                          # all density
                          sample=c("estimation", "full", "holdout"),
                          geom="line", color="black",
                          # all plots
                          plot=TRUE, verbose=TRUE, ...){

  # do not check ggplot inputs (geom, color)
  err.msg <- c()
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=plot, var.name="plot"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))
  err.msg <- c(err.msg, .check_userinput_matcharg(char=which, choices=c("tracking", "frequency", "spending", "interpurchasetime", "timings"),
                                                  var.name="which"))
  check_err_msg(err.msg)

  return(
    switch(EXPR = match.arg(arg=tolower(which), choices = c("tracking", "frequency", "spending", "interpurchasetime", "timings"),
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
         "frequency" =
           clv.data.plot.barplot.frequency(clv.data = x, sample=sample,
                                          trans.bins=trans.bins,
                                          count.repeat.trans=count.repeat.trans,
                                          label.remaining=label.remaining,
                                          count.remaining=count.remaining,
                                          plot=plot, verbose=verbose,
                                          color=color, ...),
         "timings" =
           clv.data.plot.transaction.timings(clv.data = x, Ids = Ids,
                                             annotate.ids = annotate.ids,
                                             plot = plot, verbose = verbose,
                                             ...)
         ))
}

#' @importFrom ggplot2 theme rel element_text element_blank element_rect element_line
#' @importFrom utils modifyList
clv.data.plot.add.default.theme <- function(p, custom=list()){
  l.default.args <- list(
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

  # Overwrite with custom args
  l.default.args <- modifyList(l.default.args, custom)

  return(p + do.call(what=theme, args = l.default.args))
}


clv.data.plot.tracking <- function(x, prediction.end, cumulative, plot, verbose, ...){

  period.until <- period.num <- value <- NULL

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
  dt.repeat.trans <- clv.data.add.repeat.transactions.to.periods(clv.data=x, dt.date.seq=dt.dates.expectation,
                                                                 cumulative=cumulative)
  setnames(dt.repeat.trans, old = "num.repeat.trans", new = label.transactions)


  # Plot data, if needed --------------------------------------------------------------------------------------------

  # Merge data for plotting
  #   To be sure to have all dates, merge data on original dates
  dt.dates.expectation[, period.num := NULL]
  dt.dates.expectation[dt.repeat.trans, (label.transactions) := get(label.transactions), on="period.until"]
  dt.plot <- melt(dt.dates.expectation, id.vars="period.until")

  # last period often has NA as it marks the full span of the period
  dt.plot <- dt.plot[!is.na(value)]

  # data.table does not print when returned because it is returned directly after last [:=]
  # " if a := is used inside a function with no DT[] before the end of the function, then the next
  #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
  #   will print. To avoid this: include a DT[] after the last := in your function."
  dt.plot[]

  # Only if needed
  if(!plot){
    return(dt.plot)
  }

  # Plot table with formatting, label etc
  p <- clv.controlflow.plot.tracking.base(dt.plot = dt.plot, clv.data = x,
                                          color.mapping = setNames(object = "black", nm = label.transactions))
  p <- p + theme(legend.position = "none")
  return(p)
}

clv.data.make.density.plot <- function(dt.data, mapping, labs_x, title, geom, ...){

  p <- ggplot(data = dt.data) + stat_density(mapping = mapping, geom = geom, ...)

  # Axis and title
  p <- p + labs(x = labs_x, y="Density", title=title)

  return(clv.data.plot.add.default.theme(p))
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


clv.data.plot.density.interpurchase.time <- function(clv.data, sample,
                                                     plot, verbose, color, geom, ...){
  interp.time <- mean.interpurchase.time <- NULL
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
                                      mapping = aes(x = mean.interpurchase.time),
                                      labs_x = labs_x,
                                      title = "Density of Customer's Mean Time between Transactions",
                                      geom = geom, color = color, ...))
  }else{
    return(dt.mean.interp)
  }
}


#' @importFrom ggplot2 ggplot geom_col labs geom_text position_dodge rel
clv.data.plot.barplot.frequency <- function(clv.data, count.repeat.trans, count.remaining, label.remaining, trans.bins,
                                           sample, plot, verbose, color, ...){
  x <- num.customers <- num.transactions <- NULL

  err.msg <- c()
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=count.repeat.trans, var.name="count.repeat.trans"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=count.remaining,    var.name="count.remaining"))
  err.msg <- c(err.msg, .check_userinput_charactervec(char=label.remaining, var.name="label.remaining", n=1))
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
    # data.table does not print when returned because it is returned directly after last [:=]
    # " if a := is used inside a function with no DT[] before the end of the function, then the next
    #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
    #   will print. To avoid this: include a DT[] after the last := in your function."
    dt.bins[]
    return(dt.bins)
  }else{
    #   use geom_col because geom_bar does the counting itself
    p <- ggplot(data = dt.bins) + geom_col(mapping = aes(x=num.transactions, y=num.customers),
                                           fill=color,
                                           width = 0.5,
                                           show.legend = FALSE)
    # Axis and title
    p <- p + labs(x = x.lab, y="Number of Customers",
                  # Number of Customers per Number of Transactions
                  title="Distribution of Transaction Count")

    # add count annotation
    p <- p + geom_text(aes(label = num.customers, x = num.transactions, y = num.customers),
                       position = position_dodge(width = 0.8),
                       vjust = -0.6,
                       size = rel(3))

    # Standard theme, but make x ticks for the bins bold (num repeat trans)
    return(clv.data.plot.add.default.theme(p, custom = list(axis.text.x = element_text(face="bold"))))
  }
}



#' @importFrom ggplot2 ggplot geom_segment geom_point geom_text geom_vline theme xlab ylim ggtitle element_blank scale_x_datetime scale_x_date
clv.data.plot.transaction.timings <- function(clv.data, Ids, annotate.ids, plot, verbose, ...){
  # cran silence
  Id <- x <- y <- i.y <- date.first.actual.trans <- xstart <- xend <- ystart <- yend <- type <- Date <- NULL

  err.msg <- c()
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=annotate.ids, var.name="annotate.ids"))
  err.msg <- c(err.msg, check_userinput_datanocov_ids(Ids=Ids))
  err.msg <- c(err.msg, check_user_data_emptyellipsis(...))
  check_err_msg(err.msg)


  # Draw lines with segments
  #   For each customer: xstart, ystart, xend, yend
  # Add calibration points
  #   For each customer x,y of varying number
  # Add holdout points
  #   For each customer x,y of varying number

  x.max <- clv.data@clv.time@timepoint.holdout.end

  # Select first transaction of all ids, sample if ids not given
  dt.customer.y <- pnbd_cbs(clv.data)

  Ids <- unique(Ids)
  if(is.null(Ids) | (length(Ids) == 1 & is.numeric(Ids))){
    # sample given number of random customers (50 if none given)
    if(is.null(Ids)){
      Ids <- 50
    }
    if(Ids > nobs(clv.data)){
      Ids <- nobs(clv.data)
      warning(paste0("Id may not be larger than the number of customers and is set to ", nobs(clv.data), "."))
    }
    Ids <- dt.customer.y[, sample(x = Id, size = Ids, replace = FALSE)]
  }
  if(!all(Ids %in% dt.customer.y[, unique(Id)])){
    warning("Not all given Ids were found in the transaction data.", call. = FALSE)
  }
  dt.customer.y <- dt.customer.y[Id %in% Ids, c("Id", "date.first.actual.trans")]

  # Determine y position based on date of first transaction
  #   shortest on top, ordered by Id if same timepoint
  setorderv(dt.customer.y, c("date.first.actual.trans", "Id"), order = c(1, -1))
  dt.customer.y[, y := seq(from=10, length.out=.N, by=10)]

  # Line segments
  #   x: from first transaction (start) to holdout end (end)
  #   y: start and end same per customer
  dt.segments <- dt.customer.y[, list(Id, xstart=date.first.actual.trans, xend=x.max, ystart=y, yend=y)]

  # Points in calibration period
  # x: transaction
  # y: per customer y
  dt.calibration <- clv.data.get.transactions.in.estimation.period(clv.data)
  dt.calibration <- dt.calibration[Id %in% Ids]
  dt.calibration[, x := Date]
  dt.calibration[dt.customer.y, y := i.y, on="Id"]

  # Points in holdout period
  if(clv.data.has.holdout(clv.data)){
    dt.holdout <- clv.data.get.transactions.in.holdout.period(clv.data)
    dt.holdout <- dt.holdout[Id %in% Ids]
    dt.holdout[, x := Date]
    dt.holdout[dt.customer.y, y := i.y, on="Id"]
  }


  if(!plot){
    # put data in single data.table to return if needed
    # columns: Id, type, x, y
    #   types: point_calibration, point_holdout, segment_start, segment_end
    #   x: Date
    #   y: number

    dt.segments.start <- dt.segments[, list(Id, x=xstart, y=ystart, type="segment_start")]
    dt.segments.end <- dt.segments[, list(Id, x=xend, y=yend, type="segment_end")]
    dt.calibration[, type := "point_calibration"]
    if(clv.data.has.holdout(clv.data)){
      dt.holdout[, type := "point_holdout"]
      l.plot <- list(dt.segments.start, dt.segments.end, dt.calibration, dt.holdout)
    }else{
      l.plot <- list(dt.segments.start, dt.segments.end, dt.calibration)
    }

    # melt all tables to bind them to common long-format
    dt.plot <- rbindlist(lapply(l.plot, function(dt){
        dt[, x := as.character(x)] # return x and y as chars to mix different types
        dt[, y := as.character(y)]
        dt <- melt(dt, id.vars = c('Id', 'type'), measure.vars = c('x', 'y'), variable.factor=FALSE)
        dt
      }))
    return(dt.plot)
  }

  # Use single data.tables for plotting each part because much simpler than subsetting and cast()ing dt.plot

  # Customer lines
  g <- ggplot() + geom_segment(aes(x=xstart, xend=xend, y=ystart, yend=yend), data=dt.segments, color="#efefef")

  # transaction points
  g <- g + geom_point(aes(x=x, y=y), data=dt.calibration, color="#454545")

  # holdout points & split line
  if(clv.data.has.holdout(clv.data)){
    g <- g + geom_point(aes(x=x, y=y), data=dt.holdout, color="#454545", fill="#999999", pch=21)
    g <- g + geom_vline(aes(xintercept=x), linetype="dashed", show.legend = FALSE,
                        data=data.frame(x=clv.data@clv.time@timepoint.estimation.end))
  }

  # mark ids
  if(annotate.ids){
    g <- g + geom_text(aes(x=x, y=y, label=Id),
                       data = dt.customer.y[, list(Id, y, x=min(date.first.actual.trans))],
                       nudge_x = -50, size=2.5, fontface="bold")
  }

  # y limits start at 0 to ensure a gap between first line and x axis
  g <- g + ylim(c(0, dt.customer.y[, max(y)] + 10))

  # x limits from first transaction to holdout/estimation end
  # exactly 4 breaks from first plotted transaction to end
  # dont set limits as id annotations will fall outside and trigger a waring
  x.breaks <- seq(from=dt.customer.y[, min(date.first.actual.trans)], to=x.max, length.out=4)
  if(is(clv.data@clv.time, "clv.time.date")){
    g <- g + scale_x_date(breaks = x.breaks)
  }else{
    g <- g + scale_x_datetime(breaks = x.breaks)
  }

  # cosmetics
  g <- clv.data.plot.add.default.theme(g)
  g <- g + theme(axis.title.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), panel.grid.major = element_blank())
  g <- g + xlab("Date")
  g <- g + ggtitle('Transaction Timings', subtitle = paste0("Estimation end: ",  clv.time.format.timepoint(clv.time=clv.data@clv.time, timepoint=clv.data@clv.time@timepoint.estimation.end)))

  return(g)
}
