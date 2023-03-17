
#' @title Plot Diagnostics for a Fitted Transaction Model
#' @param x The fitted transaction model for which to produce diagnostic plots
#' @param which Which plot to produce, either "tracking" or "pmf". May be abbreviated but only one may be selected. Defaults to "tracking".
#' @param other.models List of fitted transaction models to plot. List names are used as colors, standard colors are chosen if unnamed (see examples).
#'
#' @param cumulative "tracking": Whether the cumulative expected (and actual) transactions should be plotted.
#' @templateVar prefix "tracking":
#' @templateVar plot_or_predict plot
#' @template template_param_predictionend
#'
#' @param trans.bins "pmf": Vector of positive integer numbers (>=0) indicating the number of repeat transactions (x axis) to plot. Should contain 0 in nearly all cases as it refers to repeat-transactions.
#' @param calculate.remaining "pmf": Whether the probability for the remaining number of transactions not in \code{trans.bins} should be calculated.
#' @param label.remaining "pmf": Label for the last bar, if \code{calculate.remaining=TRUE}.
#'
#' @param newdata An object of class clv.data for which the plotting should be made with the fitted model.
#' If none or NULL is given, the plot is made for the data on which the model was fit.
#' If \code{other.models} was specified, the data in each model is replaced with \code{newdata}.
#' @param transactions Whether the actual observed repeat transactions should be plotted.
#' @param label Character vector to label each model. If NULL, the model(s) internal name is used (see examples).
#' @param plot Whether a plot is created or only the assembled data is returned.
#' @template template_param_verbose
#' @template template_param_dots
#'
#'
#' @description
#' Depending on the value of parameter \code{which}, one of the following plots will be produced.
#' See \code{\link[CLVTools:plot.clv.data]{plot.clv.data}} to plot more nuanced diagnostics for the transaction data only.
#' For comparison, other models can be drawn into the same plot by specifying them in \code{other.models} (see examples).
#'
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
# Plot the actual number of customers which made a given number of repeat transactions in the estimation period
# and compare it to the expected number, as by the fitted model.
#
#' Plot the actual and expected number of customers which made a given number of repeat
#' transaction in the estimation period. The expected number is based on the PMF of the fitted model,
#' the probability to make exactly a given number of repeat transactions in the estimation period.
#' For each bin, the expected number is the sum of all customers' individual PMF value.
#' Note that if \code{trans.bins} is changed, \code{label.remaining} needs to be adapted as well.
#'
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
#' @seealso \code{\link[CLVTools:pmf]{pmf}} for the values on which the PMF plot is based
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
#' \item{variable}{Type of variable that 'value' refers to. Either "model name" or "Actual" (if \code{transactions=TRUE}).}
#' \item{value}{Depending on variable either (Actual) the actual number of repeat transactions in the period that ends at \code{period.until},
#' or the unconditional expectation for the period that ends on \code{period.until} ("model name").}
#'
#' For the PMF plot:
#' \item{num.transactions}{The number of repeat transactions in the estimation period (as ordered factor).}
#' \item{variable}{Type of variable that 'value' refers to. Either "model name" or "Actual" (if \code{transactions=TRUE}).}
#' \item{value}{Depending on variable either (Actual) the actual number of customers which have the respective number of repeat transactions,
#' or the number of customers which are expected to have the respective number of repeat transactions, as by the fitted model ("model name").}
#'
#'
#'
#' @examples
#' \donttest{
#'
#' data("cdnow")
#'
#' # Fit ParetoNBD model on the CDnow data
#' clv.cdnow <- clvdata(cdnow, time.unit="w",
#'                            estimation.split=37,
#'                            date.format="ymd")
#' pnbd.cdnow <- pnbd(clv.cdnow)
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
#'
#' ## PMF PLOT
#' plot(pnbd.cdnow, which="pmf")
#'
#' # For transactions 0 to 15, also have
#' #  to change label for remaining
#' plot(pnbd.cdnow, which="pmf", trans.bins=0:15,
#'      label.remaining="16+")
#'
#' # For transactions 0 to 15 bins, no remaining
#' plot(pnbd.cdnow, which="pmf", trans.bins=0:15,
#'      calculate.remaining=FALSE)
#'
#' ## MODEL COMPARISON
#' # compare vs bgnbd
#' bgnbd.cdnow <- bgnbd(clv.cdnow)
#' ggomnbd.cdnow <- ggomnbd(clv.cdnow)
#'
#' # specify colors as names of other.models
#' # note that ggomnbd collapses into the pnbd on this dataset
#' plot(pnbd.cdnow, cumulative=TRUE,
#'      other.models=list(blue=bgnbd.cdnow, "#00ff00"=ggomnbd.cdnow))
#'
#' # specify names as label, using standard colors
#' plot(pnbd.cdnow, which="pmf",
#'      other.models=list(bgnbd.cdnow),
#'      label=c("Pareto/NBD", "BG/NBD"))
#' }
#'
#' @importFrom graphics plot
#' @include class_clv_fitted.R
#' @method plot clv.fitted.transactions
#' @aliases plot
#' @export
plot.clv.fitted.transactions <- function (x,
                                          which=c("tracking", "pmf"),
                                          other.models=list(),
                                          # tracking
                                          prediction.end=NULL,
                                          cumulative=FALSE,
                                          # pmf
                                          trans.bins = 0:9,
                                          calculate.remaining = TRUE,
                                          label.remaining="10+",
                                          # general
                                          newdata=NULL,
                                          transactions=TRUE,
                                          label=NULL,
                                          plot=TRUE,
                                          verbose=TRUE,
                                          ...) {

  # Check inputs ------------------------------------------------------------------------------------------------------
  # Do before replacing newdata as it may be costly

  # Cannot plot if there are any NAs in any of the prediction.params
  clv.controlflow.check.prediction.params(clv.fitted = x)

  err.msg <- c()
  err.msg <- c(err.msg, .check_userinput_matcharg(char=which, choices=c("tracking", "pmf"), var.name="which"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=transactions, var.name="transactions"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=plot, var.name="plot"))
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))
  err.msg <- c(err.msg, check_user_data_othermodels(other.models=other.models))
  err.msg <- c(err.msg, check_user_data_emptyellipsis(...))
  check_err_msg(err.msg)

  if(length(other.models)==0){
    if(!is.null(label)){ # null is allowed = std. model name
      check_err_msg(.check_userinput_charactervec(char=label, var.name="label", n=1))
    }else{
      label <- x@clv.model@name.model
    }
  }else{
    # names for main and all other models
    if(!is.null(label)){
      check_err_msg(.check_userinput_charactervec(char=label, var.name="label", n=1+length(other.models)))
    }
  }


  # Newdata ------------------------------------------------------------------------------------------------
  # Because many of the following steps refer to the data stored in the fitted model,
  #   it first is replaced with newdata before any other steps are done
  #
  # Skip replacing newdata if there are other models, as each model will be called again separately with newdata.
  # This saves replacing the clv.data in the main model (x) two times at initial call with other.models and separte call to plot for x only.
  # (costly mem copy and some models require re-doing LL)
  if(!is.null(newdata) & length(other.models)==0){

    # check newdata
    clv.controlflow.check.newdata(clv.fitted = x, user.newdata = newdata, prediction.end=prediction.end)

    # Replace data in model with newdata
    #   Deep copy to not change user input
    x@clv.data <- copy(newdata)

    # Do model dependent steps of adding newdata
    x <- clv.model.process.newdata(clv.model = x@clv.model, clv.fitted=x, verbose=verbose)
  }



  return(switch(match.arg(arg = tolower(which), choices = c("tracking","pmf")),
    "tracking" = clv.fitted.transactions.plot.tracking(x=x, other.models=other.models, newdata=newdata, prediction.end=prediction.end,
                                                       cumulative=cumulative, transactions=transactions,
                                                       label=label, plot=plot, verbose=verbose),
    "pmf" = clv.fitted.transactions.plot.barplot.pmf(x=x, newdata=newdata, other.models=other.models, trans.bins=trans.bins, transactions=transactions,
                                                     calculate.remaining=calculate.remaining, label.remaining=label.remaining,
                                                     label=label, plot=plot, verbose=verbose)))
}

#' @exportMethod plot
#' @include class_clv_fitted.R
#' @rdname plot.clv.fitted.transactions
setMethod("plot", signature(x="clv.fitted.transactions"), definition = plot.clv.fitted.transactions)


#' @importFrom ggplot2 ggplot aes geom_line geom_vline labs scale_fill_manual guide_legend
clv.controlflow.plot.tracking.base <- function(dt.plot, clv.data, color.mapping){
  # cran silence
  period.until <- value <- variable <- NULL

  # Plotting order
  dt.plot[, variable := factor(variable, levels=names(color.mapping), ordered = TRUE)]

  p <- ggplot(data = dt.plot, aes(x=period.until, y=value, colour=variable)) + geom_line()

  # Add holdout line if there is a holdout period
  if(clv.data.has.holdout(clv.data)){
    p <- p + geom_vline(xintercept = as.numeric(clv.data@clv.time@timepoint.holdout.start),
                        linetype="dashed", show.legend = FALSE)
  }

  # Variable color and name
  p <- p + scale_fill_manual(values = color.mapping,
                             aesthetics = c("color", "fill"),
                             guide = guide_legend(title="Legend"))

  # Axis and title
  p <- p + labs(x = "Date", y= "Number of Repeat Transactions", title= paste0(clv.time.tu.to.ly(clv.time=clv.data@clv.time), " tracking plot"),
                subtitle = paste0("Estimation end: ",  clv.time.format.timepoint(clv.time=clv.data@clv.time, timepoint=clv.data@clv.time@timepoint.estimation.end)))

  return(clv.data.plot.add.default.theme(p))
}

# Tracking plot --------------------------------------------------------------------------------------------
clv.fitted.transactions.plot.tracking <- function(x, other.models, newdata, prediction.end, cumulative, transactions,
                                                  label, plot, verbose, ...){
  variable <- period.until <- period.num <- NULL

  err.msg <- c()
  err.msg <- c(err.msg, .check_user_data_single_boolean(b=cumulative, var.name="cumulative"))
  err.msg <- c(err.msg, check_user_data_predictionend(clv.fitted=x, prediction.end=prediction.end))
  check_err_msg(err.msg)


  # do fitted object specific checks (ie dyncov checks cov data length)
  clv.controlflow.plot.check.inputs(obj=x, prediction.end=prediction.end, cumulative=cumulative,
                                    plot=plot, label.line=label, verbose=verbose)

  if(length(other.models) == 0){

    dt.plot <- clv.fitted.transactions.plot.tracking.get.data(
      x=x, prediction.end = prediction.end, cumulative=cumulative, label=label, transactions=transactions, verbose=verbose)

    dt.plot[variable == "expectation", variable := label]
    dt.plot[variable == "num.repeat.trans", variable := "Actual"]

    colors <- 'red'

  }else{
    label <- clv.fitted.transactions.plot.multiple.models.prepare.label(label=label, main.model=x, other.models=other.models)
    other.models <- clv.fitted.transactions.plot.multiple.models.prepare.othermodels(other.models=other.models)

    if(verbose){
      message("Collecting data for other models...")
    }
    dt.plot <- clv.fitted.transactions.plot.multiple.models.get.data(main.model=x, other.models=other.models, label=label,
                                                                     l.plot.args = list(which='tracking',
                                                                                        prediction.end=prediction.end,
                                                                                        newdata=newdata,
                                                                                        cumulative=cumulative, transactions=transactions, verbose=verbose))
    # main model always in red
    colors <- c('red', names(other.models))
  }


  if(!plot){

    # data.table does not print when returned because it is returned directly after last [:=]
    # " if a := is used inside a function with no DT[] before the end of the function, then the next
    #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
    #   will print. To avoid this: include a DT[] after the last := in your function."
    dt.plot[]

    return(dt.plot)
  }else{

    # add color and label for actuals
    if(transactions){
      label <- c('Actual', label) # Add 'Actual' as first! Required for correct ordering
      colors <-c('black', colors)
    }
    # Plotting order as in label
    dt.plot[, variable := factor(variable, levels=label, ordered = TRUE)]

    # Plot table with formatting, label etc
    return(clv.controlflow.plot.tracking.base(dt.plot = dt.plot, clv.data = x@clv.data, color.mapping = setNames(colors, label)))
  }

}


clv.fitted.transactions.plot.tracking.get.data <- function(x, prediction.end, cumulative, transactions, label, verbose){
  i.expectation <- expectation <- value <- num.repeat.trans <- i.num.repeat.trans <- period.num <- period.until <- NULL

  # Define time period to plot
  # Use table with exactly defined periods as reference and to save all generated data
  # End date:
  #   Use same prediction.end date for clv.data (actual transactions) and clv.fitted (unconditional expectation)
  #     If there are not enough transactions for all dates, they are set to NA (= not plotted)

  dt.dates.expectation <- clv.time.expectation.periods(clv.time = x@clv.data@clv.time, user.tp.end = prediction.end)

  tp.data.start <- dt.dates.expectation[, min(period.until)]
  tp.data.end   <- dt.dates.expectation[, max(period.until)]

  if(verbose){
    message("Plotting from ", tp.data.start, " until ", tp.data.end, ".")
  }

  if(clv.data.has.holdout(x@clv.data)){
    if(tp.data.end < x@clv.data@clv.time@timepoint.holdout.end){
      warning("Not plotting full holdout period.", call. = FALSE, immediate. = TRUE)
    }
  }else{
    if(tp.data.end < x@clv.data@clv.time@timepoint.estimation.end){
      warning("Not plotting full estimation period.", call. = FALSE, immediate. = TRUE)
    }
  }

  # Merge data for plotting
  #   To be sure to have all dates, merge data on original date

  # Get expectation values
  dt.expectation <- clv.fitted.transactions.add.expectation.data(clv.fitted.transactions=x, dt.expectation.seq=dt.dates.expectation,
                                                                 cumulative=cumulative, verbose=verbose)
  dt.dates.expectation[dt.expectation, expectation := i.expectation, on="period.until"]

  # Get repeat transactions
  if(transactions){
    dt.repeat.trans <- clv.data.add.repeat.transactions.to.periods(clv.data=x@clv.data, dt.date.seq=dt.dates.expectation,
                                                                   cumulative=cumulative)
    dt.dates.expectation[dt.repeat.trans, num.repeat.trans := i.num.repeat.trans, on="period.until"]
  }

  dt.dates.expectation[, period.num := NULL]
  dt.plot <- melt(dt.dates.expectation, id.vars='period.until')

  # last period often has NA as it marks the full span of the period
  dt.plot <- dt.plot[!is.na(value)]
  return(dt.plot)
}


# PMF plot -----------------------------------------------------------------------------------------------
#' @importFrom ggplot2 ggplot geom_col position_dodge2 guide_legend scale_x_discrete
clv.fitted.transactions.plot.barplot.pmf <- function(x, newdata, other.models, trans.bins, transactions, label,
                                                     calculate.remaining, label.remaining, plot, verbose){
  pmf.x <- pmf.value  <- actual.num.customers <- expected.customers <- i.expected.customers <- NULL
  num.customers <- num.transactions <- variable <- value <- NULL

  check_err_msg(check_user_data_integer_vector_greater0(vec=trans.bins, var.name="trans.bins"))
  check_err_msg(.check_userinput_charactervec(char=label.remaining, var.name="label.remaining", n=1))
  check_err_msg(.check_user_data_single_boolean(b=calculate.remaining, var.name="calculate.remaining"))

  if(length(other.models) == 0){

    dt.plot <- clv.fitted.transactions.plot.barplot.pmf.get.data(
      x=x, trans.bins = trans.bins, calculate.remaining=calculate.remaining, label.remaining = label.remaining, transactions = transactions)

    dt.plot[variable=="expected.customers",   variable := label]
    dt.plot[variable=="actual.num.customers", variable := "Actual"]

    colors <- 'red'

  }else{
    label <- clv.fitted.transactions.plot.multiple.models.prepare.label(label=label, main.model = x, other.models = other.models)
    other.models <- clv.fitted.transactions.plot.multiple.models.prepare.othermodels(other.models = other.models)
    if(verbose){
      message("Collecting data for other models...")
    }
    dt.plot <- clv.fitted.transactions.plot.multiple.models.get.data(main.model = x, other.models = other.models, label = label,
                                                                     l.plot.args = list(which="pmf",
                                                                                        trans.bins=trans.bins,
                                                                                        newdata=newdata,
                                                                                        label.remaining=label.remaining,
                                                                                        calculate.remaining=calculate.remaining, verbose=verbose))
    colors <- c('red', names(other.models))
  }

  if(!plot){
    # data.table does not print when returned because it is returned directly after last [:=]
    # " if a := is used inside a function with no DT[] before the end of the function, then the next
    #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
    #   will print. To avoid this: include a DT[] after the last := in your function."
    dt.plot[]

    return(dt.plot)
  }else{

    # add color and label for actuals
    if(transactions){
      label <- c('Actual', label) # Add 'Actual' as first! Required for correct ordering
      colors <-c('black', colors)
    }

    # Plotting order
    dt.plot[, variable := factor(variable, levels=label, ordered = TRUE)]

    p <- ggplot(dt.plot)+geom_col(aes(x=num.transactions, fill=variable, y=value),
                                  width = 0.5, position=position_dodge2(width = 0.9))

    # add count annotation
    p <- p + geom_text(aes(group=variable, label = round(value, digits=1),
                                  x = num.transactions, y = value),
                       position = position_dodge2(width = 0.5),
                       vjust = -0.6,
                       size = rel(3))

    # Variable color and name
    p <- p + scale_fill_manual(values = setNames(colors, label),
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

clv.fitted.transactions.plot.barplot.pmf.get.data <- function(x, trans.bins, calculate.remaining, label.remaining, transactions){
  actual.num.customers <- num.customers <- expected.customers <- i.expected.customers <- char.num.transactions <- num.transactions <- pmf.x <- pmf.value <- NULL

  # also done in plot.clv.data and pmf() but do explicitly
  trans.bins <- sort(unique(trans.bins))

  # Always work with the actuals from plot() as basis to have ordered factors
  #   Collect actual transactions
  dt.actuals <- plot(x@clv.data, which="frequency", plot=FALSE, verbose=FALSE,
                     sample="estimation",
                     trans.bins=trans.bins, count.repeat.trans=TRUE,
                     count.remaining=calculate.remaining, label.remaining = label.remaining)

  # Collect pmf values
  #   are per customer, aggregate per x
  dt.pmf <- pmf(x, x=trans.bins)

  # Calculate pmf of remaining (not explicitly calculated x)
  # P(remaining) is leftover probability (1-all others)
  if(calculate.remaining){
    dt.pmf[, (label.remaining) := 1 - rowSums(.SD), .SDcols = !"Id"]
  }

  dt.pmf <- melt(dt.pmf, id.vars = "Id", variable.factor = FALSE,
                 variable.name="pmf.x", value.name="pmf.value")
  dt.pmf <- dt.pmf[, list(expected.customers = sum(pmf.value)), by="pmf.x"]

  # Add num expected trans for each num.transactions
  #   match on separate char representation of num.transactions to keep it as ordered factor
  dt.pmf[,           char.num.transactions := gsub(x=pmf.x, pattern="pmf.x.", replacement="")]
  dt.actuals[,       char.num.transactions := as.character(num.transactions)]
  dt.actuals[dt.pmf, expected.customers    := i.expected.customers, on = "char.num.transactions"]
  dt.actuals[, char.num.transactions := NULL]

  dt.actuals[, num.customers := as.numeric(num.customers)] # integer leads to melt warning
  setnames(dt.actuals, "num.customers", "actual.num.customers")

  if(!transactions){
    # Drop actuals if not needed
    dt.actuals[, actual.num.customers := NULL]
  }


  return(melt(dt.actuals, id.vars="num.transactions", variable.factor=FALSE))
}



clv.fitted.transactions.plot.multiple.models.prepare.label <- function(label, main.model, other.models){

  # create labels from model's name if missing
  if(length(label)==0){
    label <- c(main.model@clv.model@name.model,
               sapply(other.models, function(m){
                 m@clv.model@name.model
               }))

    # postfix duplicate names because it breaks data stored in long-format
    label <- make.unique(label, sep = "_")
  }

  return(label)
}

clv.fitted.transactions.plot.multiple.models.prepare.othermodels <- function(other.models){

  if(is.null(names(other.models))){
    names(other.models) <- rep_len("", length.out = length(other.models))
  }

  # replace missing names with colors
  other.models.not.named <- nchar(names(other.models)) == 0
  if(sum(other.models.not.named) > 0){
    # cbbPalette without black and red
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#CC79A7")
    names(other.models)[other.models.not.named] <- palette[seq(sum(other.models.not.named))]
  }

  return(other.models)
}

#' @importFrom utils modifyList
clv.fitted.transactions.plot.multiple.models.get.data <- function(main.model, other.models, label, l.plot.args){

  # Collect plot data for main model separately because includes actuals if desired
  dt.plot <- do.call(plot, args = modifyList(x = l.plot.args,
                                             val=list(x=main.model, plot=FALSE, label=label[1]),
                                             keep.null = TRUE))

  # Collect plot data for all other models (always without actuals)
  # use for loop instead of lapply because requires access to label (and mapply is cumbersome)
  l.plot.others <- list()
  for(i in seq(other.models)){
    # Have to call plot() generic and not relevant function because there could be newdata which has to be updated in each model
    l.plot.others[[i]] <- do.call(plot, args = modifyList(x=l.plot.args,
                                                          val=list(x=other.models[[i]], plot=FALSE, transactions=FALSE, label= label[i+1]),
                                                          keep.null = TRUE))
  }

  return(rbindlist(c(list(dt.plot), l.plot.others), use.names = T))

}
