#' @importFrom stats predict
#' @importFrom methods extends
#' @include all_generics.R
clv.template.controlflow.predict <- function(object, prediction.end, predict.spending, continuous.discount.factor, verbose, user.newdata){
  Id <- Date <- Price <- DERT <- DECT <- actual.spending <- actual.x <- predicted.CLV <- predicted.Spending <- NULL # cran silence
  period.first <- period.last <- period.length <- cbs.x <- i.x <- cbs.Spending <- i.Spending <- i.actual.x <- i.actual.spending <- NULL

  # Set prediction params -----------------------------------------------------------------------------------
  #   need to be set before adding the newdata as the model might need them (to re-estimate or similar)

  # Set prediction params from coef()
  object <- clv.controlflow.predict.set.prediction.params(obj=object)


  # Process Newdata ----------------------------------------------------------------------------------------------
  # Because many of the following steps refer to the data stored in the fitted model (object),
  #   it first is replaced with newdata before any other steps are done
  if(!is.null(user.newdata)){
    # check newdata
    clv.controlflow.check.newdata(clv.fitted = object, user.newdata = user.newdata, prediction.end=prediction.end)

    # Replace data in model with newdata
    #   Deep copy to not change user input
    object@clv.data <- copy(user.newdata)

    # Do model dependent steps of adding newdata
    object <- clv.model.put.newdata(clv.model = object@clv.model, clv.fitted=object, verbose=verbose)
  }


  # Input checks ----------------------------------------------------------------------------------------
  #   Only after newdata replaced clv.data stored in object because inputchecks use object@clv.data
  clv.controlflow.predict.check.inputs(obj=object, prediction.end=prediction.end, predict.spending=predict.spending,
                                       continuous.discount.factor=continuous.discount.factor,
                                       verbose=verbose)



  # Prediction result table ------------------------------------------------------------------------------
  dt.prediction <- copy(object@cbs[, "Id"])

  # Add information about range of prediction period
  #   tp.prediction.start: Start of prediction, including this timepoint
  #   tp.prediction.end: End of prediction period which includes this timepoint
  #   prediction.length: Length of period for which predictions should be made, in number of periods

  # Could be viewed as part of input checks
  #   but the end of the prediction period cannot be determined until after newdata is set
  dt.prediction.time.table <- clv.time.get.prediction.table(clv.time = object@clv.data@clv.time,
                                                            user.prediction.end = prediction.end)
  # Add information to prediction table
  dt.prediction <- cbind(dt.prediction, dt.prediction.time.table)

  timepoint.prediction.first <- dt.prediction[1, period.first]
  timepoint.prediction.last  <- dt.prediction[1, period.last]
  prediction.period.length   <- dt.prediction[1, period.length]

  if(verbose)
    message("Predicting from ", timepoint.prediction.first, " until (incl.) ",
            timepoint.prediction.last, " (", format(prediction.period.length, digits = 4, nsmall=2)," ",
            object@clv.data@clv.time@name.time.unit,").")


  # Need at least > 2 time units to predict
  if(prediction.period.length <= 2)
    stop("The end of the prediction needs to be at least 3 periods after the end of the estimation period!", call. = FALSE)



  # Model prediction -------------------------------------------------------------------------------------
  dt.prediction <- clv.model.predict.clv(clv.model = object@clv.model, clv.fitted = object,
                                         dt.prediction = dt.prediction,
                                         continuous.discount.factor = continuous.discount.factor,
                                         verbose = verbose)
  setkeyv(dt.prediction, "Id")




  # Actuals data -------------------------------------------------------------------------------------------
  #   Only if:
  #       - there is a holdout
  #       - the prediction is not beyond holdout
  #
  #   Data until prediction end
  #     actual.x:         number of transactions
  #     actual.spending:  $

  has.actuals <- clv.data.has.holdout(object@clv.data) & (timepoint.prediction.last <= object@clv.data@clv.time@timepoint.holdout.end)
  if(has.actuals)
  {
    # only what is in prediction period!

    if(clv.data.has.spending(object@clv.data)){
      dt.actuals    <- object@clv.data@data.transactions[between(x = Date,
                                                                 lower = timepoint.prediction.first,
                                                                 upper = timepoint.prediction.last,
                                                                 incbounds = TRUE),
                                                         list(actual.x        = .N,
                                                              actual.spending = sum(Price)),
                                                         by="Id"]
    }else{
      # No Spending
      dt.actuals    <- object@clv.data@data.transactions[between(x = Date,
                                                                 lower = timepoint.prediction.first,
                                                                 upper = timepoint.prediction.last,
                                                                 incbounds = TRUE),
                                                         list(actual.x        = .N,
                                                              actual.spending = 0),
                                                         by="Id"]
    }


    # add actuals to prediction
    setkeyv(dt.actuals, "Id")
    dt.prediction[dt.actuals,             actual.x  := i.actual.x,  on="Id"]
    dt.prediction[is.na(actual.x),        actual.x  := 0]
    dt.prediction[dt.actuals,             actual.spending := i.actual.spending, on="Id"]
    dt.prediction[is.na(actual.spending), actual.spending := 0]
  }


  # Predict spending ---------------------------------------------------------------------------
  #   Estimate a GG model for this
  #   CLV: DERT * Spending
  #  Input checks already checked whether there is spending data in clv.data
  if(predict.spending){

    # Optimize GG LL
    results <- optimx(par    = c(p=log(1),q=log(1),gamma=log(1)), # will be exp()ed in gg_LL
                      fn     = gg_LL,
                      vX     = object@cbs$x,
                      vM_x   = object@cbs$Spending,
                      upper  = c(log(10000),log(10000),log(10000)),
                      lower  = c(log(0),log(0),log(0)),
                      method = "L-BFGS-B",
                      control=list(trace = 0,
                                   # Do not perform starttests because it checks the scales with max(logpar)-min(logpar)
                                   #   but all standard start parameters are <= 0, hence there are no logpars what
                                   #   produces a warning
                                   starttests = FALSE,
                                   maxit=3000))
    p     <- exp(coef(results)[1,"p"])
    q     <- exp(coef(results)[1,"q"])
    gamma <- exp(coef(results)[1,"gamma"])

    # Predict spending
    #   add data from cbs by Id to ensure matching
    dt.prediction[object@cbs, cbs.x := i.x, on="Id"]
    dt.prediction[object@cbs, cbs.Spending := i.Spending, on="Id"]
    dt.prediction[, predicted.Spending := (gamma + cbs.Spending * cbs.x) * p/(p * cbs.x + q - 1)]
    dt.prediction[, cbs.x        := NULL]
    dt.prediction[, cbs.Spending := NULL]

    # Calculate CLV
    if("DERT" %in% colnames(dt.prediction))
      dt.prediction[, predicted.CLV := DERT * predicted.Spending]
    if("DECT" %in% colnames(dt.prediction))
      dt.prediction[, predicted.CLV := DECT * predicted.Spending]
  }



  # Present cols in desired order ------------------------------------------------------------
  cols <- c("Id", "period.first", "period.last", "period.length")
  if(has.actuals)
    cols <- c(cols, c("actual.x", "actual.spending"))

  if("DERT" %in% colnames(dt.prediction))
    cols <- c(cols, "PAlive", "CET", "DERT")
  if("DECT" %in% colnames(dt.prediction))
    cols <- c(cols, "PAlive", "CET", "DECT")

  if(predict.spending)
    cols <- c(cols, c("predicted.Spending", "predicted.CLV"))
  setcolorder(dt.prediction, cols)

  # data.table does not print when returned because it is returned directly after last [:=]
  # " if a := is used inside a function with no DT[] before the end of the function, then the next
  #   time DT or print(DT) is typed at the prompt, nothing will be printed. A repeated DT or print(DT)
  #   will print. To avoid this: include a DT[] after the last := in your function."
  dt.prediction[]

  return(dt.prediction)
}



# S3 predict for clv.fitted ----------------------------------------------------------------------------------


#' @title Predict CLV from a fitted model
#' @param object A fitted clv model for which prediction is desired.
#' @param newdata A clv data object for which predictions should be made with the fitted model. If none or NULL is given, predictions are made for the data on which the model was fit.
#' @param predict.spending Whether the spending and CLV should be calculated and reported additionally. Only possible if the transaction data contains spending information.
#' @param continuous.discount.factor continuous discount factor to use
#' @template template_param_predictionend
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#' Probabilistic customer attrition models predict in general three expected characteristics for every customer:
#' \itemize{
#' \item "conditional expected transactions" (\code{CET}), which is the number of transactions to expect form a customer
#' during the prediction period,
#' \item "probability of a customer being alive" (\code{PAlive}) at the end of the estimation period and
#' \item "discounted expected residual transactions" (\code{DERT}) for every customer, which is the total number of
#' transactions for the residual lifetime of a customer discounted to the end of the estimation period.
#' In the case of time-varying covariates, instead of \code{DERT}, "discounted expected conditional transactions" (\code{DECT})
#' is predicted. \code{DECT} does only cover a finite time horizon in contrast to \code{DERT}. For \code{continuous.discount.factor=0}, \code{DECT} corresponds to \code{CET}.
#'}
#' If spending information was provided in the \code{clvdata} object, by default a Gamma/Gamma model is fitted
#' to predict spending and calculate the predicted CLV. In this case, the prediction additionally contains the following two columns:
#' \itemize{
#' \item predicted spending estimated by a Gamma/Gamma model
#' \item the customer lifetime value (CLV).
#'}
#'
#'
#'
#'
#'
#' @template template_details_predictionend
#'
#' @details \code{predict.spending} uses a Gamma/Gamma model to predict customer spending. This option is only available
#' if customer spending information was provided when the data object was created.
#'
#' @details \code{continuous.discount.factor} allows to adjust the discount rate used to estimated the discounted expected
#' transactions (\code{DERT}).
#' The default value is \code{0.1} (=10\%). Note that a continuous rate needs to be provided.
#'
#'
#' \subsection{The Gamma-Gamma model to Predict Spending}{
#' Most probabilistic latent customer attrition model capture future customer
#' behavior as a combination of the customer's purchase and attrition process.
#' However, in order to derive a monetary value such as CLV, customer spending
#' also has to be considered. To model customer spending the Gamma/Gamma is a
#' popular choice.
#' }
#'
#'
#' @references
#' Schmittlein DC, Morrison DG, Colombo R (1987). “Counting Your Customers:
#' Who-Are They and What Will They Do Next?” Management Science, 33(1), 1–24.
#'
#' Fader PS, Hardie BGS (2005). “A Note on Deriving the Pareto/NBD Model and
#' Related Expressions.”
#' URL \url{http://www.brucehardie.com/notes/009/pareto_nbd_derivations_2005-11-05.pdf}.
#'
#' Fader PS, Hardie BG (2007). "Incorporating time-invariant covariates into the
#' Pareto/NBD and BG/NBD models."
#' URL \url{http://www.brucehardie.com/notes/019/time_invariant_covariates.pdf}.
#'
#' Colombo R, Jiang W (1999). "A stochastic RFM model."
#' Journal of Interactive Marketing, 13(3), 2–12.
#'
#' Fader PS, Hardie BG, Lee K (2005). "RFM and CLV: Using Iso-Value Curves for Customer Base Analysis."
#' Journal of Marketing Research, 42(4), 415–430.
#'
#' Fader PS, Hardie BG (2013). "The Gamma-Gamma Model of Monetary Value."
#' URL \url{http://www.brucehardie.com/notes/025/gamma_gamma.pdf}.
#'
#'
#' @return
#' An object of class \code{data.table} with each columns containing the predictions:
#' \item{Id}{The respective customer identifier}
#' \item{period.first}{First timepoint of prediction period}
#' \item{period.last}{Last timepoint of prediction period}
#' \item{period.length}{Number of time units covered by the period indicated by \code{period.first} and \code{period.last} (including both ends).}
#' \item{PAlive}{Probability to be alive at the end of the estimation period}
#' \item{CET}{The Conditional Expected Transactions}
#' \item{DERT or DECT}{Discounted Expected Residual Transactions or Discounted Expected Conditional Transactions for dynamic covariates models}
#' \item{actual.x}{Actual number of transactions until prediction.end. Only if there is a holdout period and the prediction ends in it.}
#' \item{actual.Spending}{Actual spending until prediction.end. Only if there is a holdout period and the prediction ends in it, 0 otherwise.}
#' \item{predicted.Spending}{The spending as predicted by the Gamma-Gamma model.}
#' \item{predicted.CLV}{Customer Lifetime Value based on DERT and predicted spending.}
#'
#' @examples
#'
#' \donttest{
#'
#' data("apparelTrans")
#' # Fit pnbd standard model on data, WITH holdout
#' pnc <- pnbd(clvdata(apparelTrans, time.unit="w",
#'                     estimation.split=37, date.format="ymd"))
#'
#' # Predict until the end of the holdout period
#' predict(pnc)
#'
#' # Predict until 10 periods (weeks in this case) after
#' #   the end of the 37 weeks fitting period
#' predict(pnc, prediction.end = 10) # ends on 2010-11-28
#'
#' # Predict until 31th Dec 2016 with the timepoint as a character
#' predict(pnc, prediction.end = "2016-12-31")
#'
#' # Predict until 31th Dec 2016 with the timepoint as a Date
#' predict(pnc, prediction.end = lubridate::ymd("2016-12-31"))
#'
#'
#' # Fit pnbd standard model WITHOUT holdout
#' pnc <- pnbd(clvdata(apparelTrans, time.unit="w", date.format="ymd"))
#'
#' # This fails, because without holdout, a prediction.end is required
#' \dontrun{
#' predict(pnc)
#' }
#'
#' # Now, predict 10 periods from the end of the last transaction
#' #   (end of estimation period)
#' predict(pnc, prediction.end = 10) # ends on 2016-12-17
#'
#' }
#'
#' @method predict clv.fitted
#' @export
predict.clv.fitted <- function(object, newdata=NULL, prediction.end=NULL, predict.spending=clv.data.has.spending(object@clv.data),
                               continuous.discount.factor=0.1, verbose=TRUE, ...){
  # warn if unnecessary input
  if(length(list(...))>0)
    warning("The additional parameters given in '...' are ignored because they are unneded!",
            call. = FALSE)

  clv.template.controlflow.predict(object=object, prediction.end=prediction.end, predict.spending=predict.spending,
                                   continuous.discount.factor=continuous.discount.factor, verbose=verbose, user.newdata=newdata)
}


# S4 predict definitions --------------------------------------------------------------------------------
# S4 method to forward to S3 method
#' @include all_generics.R class_clv_fitted.R
#' @exportMethod predict
#' @rdname predict.clv.fitted
setMethod(f = "predict", signature = signature(object="clv.fitted"), predict.clv.fitted)
