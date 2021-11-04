
#' @title Plot Diagnostics for a Fitted Transaction Model
#' @param x The fitted transaction model for which to produce diagnostic plots
#' @param which Which plot to produce, either "tracking" or "pmf". May be abbreviated but only one may be selected. Defaults to "tracking".
#'
#' @param cumulative "tracking": Whether the cumulative expected (and actual) transactions should be plotted.
#' @template template_param_predictionend
#'
#' @param trans.bins "pmf": Vector of positive integer numbers (>=0) indicating the number of repeat transactions (x axis) to plot.
#'
#' @param newdata An object of class clv.data for which the plotting should be made with the fitted model.
#' If none or NULL is given, the plot is made for the data on which the model was fit.
#' @param transactions Whether the actual observed repeat transactions should be plotted.
#' @param label Character string to label the model in the legend.
#' @param plot Whether a plot is created or only the assembled data is returned.
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#' Depending on the value of parameter which, one of the following plots will be produced.
#' See \code{\link[CLVTools:plot.clv.data]{plot.clv.data}} to plot more nuanced diagnostics for the transaction data only.
#'
#' \subsection{Tracking Plot}{
#' Plot the actual repeat transactions and overlay it with the repeat transaction as predicted
#' by the fitted model. Currently, following previous literature, the in-sample unconditional
#' expectation is plotted in the holdout period. In the future, we might add the option to also
#' plot the summed CET for the holdout period as an alternative evaluation metric.
#' Note that only whole periods can be plotted and that the prediction end might not exactly match \code{prediction.end}.
#' See the Note section for more details.
#' }
#'
#' \subsection{PMF Plot}{
#' The expected number is the sum of all customer's PMF value for a single transaction count
#' based on the probability mass function (PMF)
#' the probability to make exactly x repeat transactions in the estimation period
#' xxxx compare the actual with the expected number of customers which are, in the estimation period.
#' sum pmf values
#' Plot the actual and expected number of customers which make given repeat transaction in the estimation period
#'
#' and compare with the number of customers which are expected to have this many repeat transactions,
#' as by the fitted model.
#' }
#'
#' @template template_details_predictionend
#' @template template_details_newdata
#'
#'
#' @note Because the unconditional expectation for a period is derived as the difference of
#' the cumulative expectations calculated at the beginning and at end of the period,
#' all timepoints for which the expectation is calculated are required to be spaced exactly 1 time unit apart.
#'
#' If \code{prediction.end} does not coincide with the start of a time unit, the last timepoint
#' for which the expectation is calculated and plotted therefore is not \code{prediction.end}
#' but the start of the first time unit after \code{prediction.end}.
#'
#' @seealso \code{\link[CLVTools:plot.clv.fitted.spending]{plot.clv.fitted.spending}} for diagnostics of spending models
#' @seealso \code{\link[CLVTools:plot.clv.data]{plot.clv.data}} for transaction diagnostics of \code{clv.data} objects
#' @seealso \code{\link[CLVTools:pmf.clv.data]{pmf}} for the values on which the PMF plot is based
#'
#'
#' @return
#' An object of class \code{ggplot} from package \code{ggplot2} is returned by default.
#' If \code{plot=FALSE}, the data that would have been used to create the plot is returned.
#' Depending on which plot was selected, this is a data.table which contains the
#' following columns:
#'
#' For the Tracking plot:
#' \item{period.until}{The timepoint that marks the end (up until and including) of the period to which the data in this row refers.}
#' \item{Number of Repeat Transactions}{The number of actual repeat transactions in
#' the period that ends at \code{period.until}. Only if \code{transactions} is \code{TRUE}.}
#' \item{"Name of Model" or "label"}{The value of the unconditional expectation for the period that ends on \code{period.until}.}
#'
#' For the PMF plot:
#' \item{num.transactions}{The number of observed repeat transactions in the estimation period (as ordered factor).}
#' \item{actual.num.customers}{The actual number of customers which have the respective number of repeat transactions.}
#' \item{expected.customers}{The number of customers which are expected to have the respective number of repeat transactions, as by the fitted model.}
#'
#'
#' @examples
#' \donttest{
#'
#' data("cdnow")
#'
#' # Fit ParetoNBD model on the CDnow data
#' pnbd.cdnow <- pnbd(clvdata(cdnow, time.unit="w",
#'                            estimation.split=37,
#'                            date.format="ymd"))
#'
#' ## TRACKING PLOT
#' # Plot actual repeat transaction, overlayed with the
#' #  expected repeat transactions as by the fitted model
#' plot(pnbd.cdnow)
#'
#' # Plot cumulative expected transactions of only the model
#' plot(pnbd.cdnow, cumulative=TRUE, transactions=FALSE)
#'
#' # Plot until 2001-10-21
#' plot(pnbd.cdnow, prediction.end = "2001-10-21")
#'
#' # Plot until 2001-10-21, as date
#' plot(pnbd.cdnow,
#'      prediction.end = lubridate::dym("21-2001-10"))
#'
#' # Plot 15 time units after end of estimation period
#' plot(pnbd.cdnow, prediction.end = 15)
#'
#' # Save the data generated for plotting
#' #   (period, actual transactions, expected transactions)
#' plot.out <- plot(pnbd.cdnow, prediction.end = 15)
#'
#' # A ggplot object is returned that can be further tweaked
#' library("ggplot2")
#' gg.pnbd.cdnow <- plot(pnbd.cdnow)
#' gg.pnbd.cdnow + ggtitle("PNBD on CDnow")
#'
#' ## PMF PLOT
#' plot(pnbd.cdnow, which="pmf")
#'
#' # for 5 to 15 bins
#' plot(pnbd.cdnow, which="pmf", trans.bins=5:15)
#'
#' }
#'
# # Compose plot from separate model plots
# # pnbd vs bgnbd
# p.m1 <- plot(pnbd.cdnow, transactions = TRUE)
#
# # static cov model
# p.m2 <- plot(pnbd.cdnow.cov, transactions = FALSE)
# p.m1 + geom_line(mapping=p.m2$mapping, data=p.m2$data,
#                  color="blue")
#
#' @importFrom graphics plot
#' @include class_clv_fitted.R
#' @method plot clv.fitted.transactions
#' @aliases plot
#' @export
plot.clv.fitted.transactions <- function (x, which=c("tracking", "pmf"),
                                          # tracking
                                          prediction.end=NULL, cumulative=FALSE,
                                          # pmf
                                          trans.bins = 0:10,
                                          # general
                                          newdata=NULL, transactions=TRUE, label=NULL, plot=TRUE, verbose=TRUE,...) {

  # Check inputs ------------------------------------------------------------------------------------------------------
  # Do before replacing newdata as it may be costly

  # Cannot plot if there are any NAs in any of the prediction.params
  clv.controlflow.check.prediction.params(clv.fitted = x)

  err.msg <- c()
  err.msg <- c(err.msg, .check_userinput_matcharg(char=which, choices=c("tracking", "pmf"), var.name="which"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=transactions, var.name="transactions"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=plot, var.name="plot"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))
  err.msg <- c(err.msg, check_user_data_emptyellipsis(...))
  if(!is.null(label)) # null is allowed = std. model name
    err.msg <- c(err.msg, .check_userinput_single_character(char=label, var.name="label"))
  check_err_msg(err.msg)

  # Newdata ------------------------------------------------------------------------------------------------
  # Because many of the following steps refer to the data stored in the fitted model,
  #   it first is replaced with newdata before any other steps are done
  if(!is.null(newdata)){
    # check newdata
    clv.controlflow.check.newdata(clv.fitted = x, user.newdata = newdata, prediction.end=prediction.end)

    # Replace data in model with newdata
    #   Deep copy to not change user input
    x@clv.data <- copy(newdata)

    # Do model dependent steps of adding newdata
    x <- clv.model.process.newdata(clv.model = x@clv.model, clv.fitted=x, verbose=verbose)
  }



  return(switch(match.arg(arg = which, choices = c("tracking","pmf")),
    "tracking" = clv.fitted.transactions.plot.tracking(x=x, newdata=newdata, prediction.end=prediction.end,
                                                       cumulative=cumulative, transactions=transactions,
                                                       label=label, plot=plot, verbose=verbose),
    "pmf" = clv.fitted.transactions.plot.barplot.pmf(x=x, trans.bins=trans.bins, transactions=transactions,
                                                     label=label, plot=plot, verbose=verbose)))
}

#' @exportMethod plot
#' @include class_clv_fitted.R
#' @rdname plot.clv.fitted.transactions
setMethod("plot", signature(x="clv.fitted.transactions"), definition = plot.clv.fitted.transactions)


#' @importFrom ggplot2 ggplot aes geom_line geom_vline labs theme scale_fill_manual guide_legend element_text element_rect element_blank element_line rel
clv.controlflow.plot.make.plot <- function(dt.data, clv.data, line.colors){
  # cran silence
  period.until <- value <- variable <- NULL

  # Melt everything except what comes from the standard expectation table
  meas.vars   <- setdiff(colnames(dt.data), c("period.num", "period.until"))
  data.melted <- melt(data=dt.data, id.vars = c("period.until"),
                      variable.factor = FALSE, na.rm = TRUE,
                      measure.vars = meas.vars)

  p <- ggplot(data = data.melted, aes(x=period.until, y=value, colour=variable)) + geom_line()

  # Add holdout line if there is a holdout period
  if(clv.data.has.holdout(clv.data)){
    p <- p + geom_vline(xintercept = as.numeric(clv.data@clv.time@timepoint.holdout.start),
                        linetype="dashed", show.legend = FALSE)
  }

  # Variable color and name
  p <- p + scale_fill_manual(values = line.colors,
                             aesthetics = c("color", "fill"),
                             guide = guide_legend(title="Legend"))

  # Axis and title
  p <- p + labs(x = "Date", y= "Number of Repeat Transactions", title= paste0(clv.time.tu.to.ly(clv.time=clv.data@clv.time), " tracking plot"),
                subtitle = paste0("Estimation end: ",  clv.time.format.timepoint(clv.time=clv.data@clv.time, timepoint=clv.data@clv.time@timepoint.estimation.end)))

  return(clv.data.plot.add.default.theme(p))
}

# clv.controlflow.plot.get.data ---------------------------------------------------------------
setMethod(f="clv.controlflow.plot.get.data", signature = signature(obj="clv.fitted.transactions"), definition = function(obj, dt.expectation.seq, cumulative, verbose){

  expectation <- i.expectation <- NULL

  #   Pass copy of expectation table file because will be modified and contain column named expecation
  dt.model.expectation <- clv.model.expectation(clv.model=obj@clv.model, clv.fitted=obj, dt.expectation.seq=copy(dt.expectation.seq),
                                                verbose = verbose)

  # Only the expectation data
  dt.model.expectation <- dt.model.expectation[, c("period.until", "expectation")]

  if(cumulative)
    dt.model.expectation[, expectation := cumsum(expectation)]

  # add expectation to plot data
  #   name columns by model
  dt.expectation.seq[dt.model.expectation, expectation := i.expectation, on = "period.until"]
  return(dt.expectation.seq)
})


# Tracking plot --------------------------------------------------------------------------------------------
clv.fitted.transactions.plot.tracking <- function(x, newdata, prediction.end, cumulative, transactions,
                                                  label, plot, verbose, ...){
  period.until <- period.num <- NULL

  # Check inputs ------------------------------------------------------------------------------------------------------
  err.msg <- c()
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=cumulative, var.name="cumulative"))
  err.msg <- c(err.msg, check_user_data_predictionend(clv.fitted=x, prediction.end=prediction.end))
  check_err_msg(err.msg)


  # do fitted object specific checks (ie dyncov checks cov data length)
  clv.controlflow.plot.check.inputs(obj=x, prediction.end=prediction.end, cumulative=cumulative,
                                    plot=plot, label.line=label, verbose=verbose)


  # Define time period to plot -----------------------------------------------------------------------------------------
  # Use table with exactly defined periods as reference and to save all generated data
  # End date:
  #   Use same prediction.end date for clv.data (actual transactions) and clv.fitted (unconditional expectation)
  #     If there are not enough transactions for all dates, they are set to NA (= not plotted)

  dt.dates.expectation <- clv.time.expectation.periods(clv.time = x@clv.data@clv.time, user.tp.end = prediction.end)

  tp.data.start <- dt.dates.expectation[, min(period.until)]
  tp.data.end   <- dt.dates.expectation[, max(period.until)]

  if(verbose)
    message("Plotting from ", tp.data.start, " until ", tp.data.end, ".")


  if(clv.data.has.holdout(x@clv.data)){
    if(tp.data.end < x@clv.data@clv.time@timepoint.holdout.end){
      warning("Not plotting full holdout period.", call. = FALSE, immediate. = TRUE)
    }
  }else{
    if(tp.data.end < x@clv.data@clv.time@timepoint.estimation.end){
      warning("Not plotting full estimation period.", call. = FALSE, immediate. = TRUE)
    }
  }


  # Get expectation values -----------------------------------------------------------------------------------------
  dt.expectation <- clv.controlflow.plot.get.data(obj=x, dt.expectation.seq=dt.dates.expectation,
                                                  cumulative=cumulative, verbose=verbose)
  if(length(label)==0)
    label.model.expectation <- x@clv.model@name.model
  else
    label.model.expectation <- label

  setnames(dt.expectation,old = "expectation", new = label.model.expectation)

  # Get repeat transactions ----------------------------------------------------------------------------------------
  if(transactions){
    label.transactions <- "Actual Number of Repeat Transactions"
    dt.repeat.trans <- clv.controlflow.plot.get.data(obj=x@clv.data, dt.expectation.seq=dt.dates.expectation,
                                                     cumulative=cumulative, verbose=verbose)
    setnames(dt.repeat.trans, old = "num.repeat.trans", new = label.transactions)
  }

  # Plot data, if needed --------------------------------------------------------------------------------------------
  # Merge data for plotting
  #   To be sure to have all dates, merge data on original dates

  dt.dates.expectation[, period.num := NULL]
  dt.dates.expectation[dt.expectation, (label.model.expectation) := get(label.model.expectation), on="period.until"]

  if(transactions){
    dt.dates.expectation[dt.repeat.trans, (label.transactions) := get(label.transactions), on="period.until"]
  }
  dt.plot <- dt.dates.expectation

  # data.table does not print when returned because it is returned directly after last [:=]
  # " if a := is used inside a function with no DT[] before the end of the function, then the next
  #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
  #   will print. To avoid this: include a DT[] after the last := in your function."
  dt.plot[]

  # Only plot if needed
  if(!plot){
    return(dt.plot)
  }else{
    if(transactions){
      line.colors <- setNames(object = c("black", "red"),
                              nm = c(label.transactions, label.model.expectation))
    }else{
      line.colors <- setNames(object = "red", nm = label.model.expectation)
    }

    # Plot table with formatting, label etc
    return(clv.controlflow.plot.make.plot(dt.data = dt.plot, clv.data = x@clv.data, line.colors = line.colors))
  }

}


# PMF plot -----------------------------------------------------------------------------------------------
#' @importFrom ggplot2 ggplot geom_col aes_string position_dodge2 guide_legend scale_x_discrete
clv.fitted.transactions.plot.barplot.pmf <- function(x, trans.bins, transactions, label, plot, verbose){
  pmf.x <- pmf.value  <- expected.customers <- i.expected.customers <- NULL
  num.customers <- num.transactions <- char.num.transactions <- variable <- NULL

  check_err_msg(check_user_data_integer_vector_greater0(vec=trans.bins, var.name="x"))

  # Always work with the actuals from plot() as basis to have ordered factors
  #   Collect actual transactions
  dt.actuals <- plot(x@clv.data, which="frequency", plot=FALSE, verbose=FALSE,
                     trans.bins=trans.bins, count.repeat.trans=TRUE,
                     count.remaining=FALSE) #, label.remaining = "remaining")

  # Collect pmf values
  #   are per customer, aggregate per x
  dt.pmf <- pmf(x, x=trans.bins)
  dt.pmf <- melt(dt.pmf, id.vars = "Id", variable.factor = FALSE,
                 variable.name="pmf.x", value.name="pmf.value")
  dt.pmf <- dt.pmf[, list(expected.customers = sum(pmf.value)), by="pmf.x"]

  # Add num expected trans for each num.transactions
  #   match on separate char representation of num.transactions to keep it as ordered factor
  dt.pmf[,           char.num.transactions := gsub(x=pmf.x, pattern="pmf.x.", replacement="")]
  dt.actuals[,       char.num.transactions := as.character(num.transactions)]
  dt.actuals[dt.pmf, expected.customers    := i.expected.customers, on = "char.num.transactions"]
  dt.actuals[, char.num.transactions := NULL]

  dt.actuals[, num.customers := as.numeric(num.customers)] # integer, leads to melt warning
  setnames(dt.actuals, "num.customers", "actual.num.customers")

  if(!transactions){
    # Drop actuals if not needed
    dt.actuals[, actual.num.customers := NULL]
  }

  if(!plot){
    # data.table does not print when returned because it is returned directly after last [:=]
    # " if a := is used inside a function with no DT[] before the end of the function, then the next
    #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
    #   will print. To avoid this: include a DT[] after the last := in your function."
    dt.actuals[]

    return(dt.actuals)
  }else{

    if(is.null(label)){
      label.model <- x@clv.model@name.model
    }else{
      label.model <- label
    }

    dt.plot <- melt(dt.actuals, id.vars="num.transactions", variable.factor=FALSE)
    dt.plot[variable=="expected.customers",   variable := label.model]

    if(transactions){
      dt.plot[variable=="actual.num.customers", variable := "Actual Number of Repeat Transactions"]
      lines.color <- setNames(object = c("black", "red"),
                              nm = c("Actual Number of Repeat Transactions", label.model))
    }else{
      lines.color <- setNames(object = "red", nm = label.model)
    }

    p <- ggplot(dt.plot)+geom_col(aes_string(x="num.transactions", fill="variable", y="value"),
                                  width = 0.5, position=position_dodge2(width = 0.9))

    # add count annotation
    p <- p + geom_text(aes_string(group="variable", label = "round(value, digits=1)",
                                  x = "num.transactions", y = "value"),
                       position = position_dodge2(width = 0.5),
                       vjust = -0.6,
                       size = rel(3))

    # Variable color and name
    p <- p + scale_fill_manual(values = lines.color,
                               aesthetics = c("color", "fill"),
                               guide = guide_legend(title="Legend"))

    # # show missing values as 0 (if there are)
    p <- p + scale_x_discrete(na.translate=TRUE, na.value=0)

    # Axis and title
    p <- p + labs(x = "Number of Repeat Transactions", y="Number of Customers",
                  title="Frequency of Repeat Transactions in the Estimation Period")

    return(clv.data.plot.add.default.theme(p, custom = list(axis.text.x = element_text(face="bold"))))
  }
}

