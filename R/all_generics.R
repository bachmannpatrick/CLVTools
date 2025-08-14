# Predict Generics -------------------------------------------------------
# The S4 generic is defined explicitely for clarity, instead of relying on it as a side-effect of
#    only defining the method with setMethod
# As explained in ?Methods_for_S3 and per Martin Morgan's answer (https://stackoverflow.com/questions/32512785/properly-specify-s4-generics)
# Needs:
#   setClass
#   S3 implementation fun.class
#   S4 method setMethod that dispatches to S3 implementation
#   S3method(fun, class)
#   exportMethods(fun)
setGeneric(name = "predict")

# Controlflows -------------------------------------------------------------------------------------------------
# Steps performed by all models but different between base (no cov) and covariate models

# . Estimate ---------------------------------------------------------------------------------------------------
setGeneric("clv.controlflow.estimate.check.inputs", def=function(clv.fitted,  start.params.model, optimx.args, verbose,...)
  standardGeneric("clv.controlflow.estimate.check.inputs"))

setGeneric("clv.controlflow.estimate.put.inputs", def=function(clv.fitted, start.params.model, optimx.args, verbose, ...)
  standardGeneric("clv.controlflow.estimate.put.inputs"))

setGeneric("clv.controlflow.estimate.generate.start.params", def=function(clv.fitted, start.params.model, verbose,...)
  standardGeneric("clv.controlflow.estimate.generate.start.params"))

setGeneric("clv.controlflow.estimate.prepare.optimx.args", def=function(clv.fitted, start.params.all)
  standardGeneric("clv.controlflow.estimate.prepare.optimx.args"))

setGeneric("clv.controlflow.estimate.process.post.estimation", def=function(clv.fitted, res.optimx)
  standardGeneric("clv.controlflow.estimate.process.post.estimation"))


# . Predict -----------------------------------------------------------------------------------------------
setGeneric("clv.controlflow.predict.check.inputs", def = function(clv.fitted, verbose, uncertainty, num.boots, level, ...)
  standardGeneric("clv.controlflow.predict.check.inputs"))

setGeneric("clv.controlflow.predict.set.prediction.params", def = function(clv.fitted)
  standardGeneric("clv.controlflow.predict.set.prediction.params"))

setGeneric("clv.controlflow.predict.build.result.table", def = function(clv.fitted, verbose, ...)
  standardGeneric("clv.controlflow.predict.build.result.table"))

setGeneric(name = "clv.controlflow.predict.get.has.actuals", def = function(clv.fitted, dt.predictions)
  standardGeneric("clv.controlflow.predict.get.has.actuals"))

setGeneric(name = "clv.controlflow.predict.add.actuals", def = function(clv.fitted, dt.predictions, has.actuals, verbose, ...)
  standardGeneric("clv.controlflow.predict.add.actuals"))

setGeneric(name = "clv.controlflow.predict.post.process.prediction.table", def = function(clv.fitted, dt.predictions, has.actuals, verbose, ...)
  standardGeneric("clv.controlflow.predict.post.process.prediction.table"))


# .. Newdata: replace data in existing model -----------------------------------------------------------------
# For plot and predict
setGeneric("clv.controlflow.check.newdata", def = function(clv.fitted, user.newdata, ...)
  standardGeneric("clv.controlflow.check.newdata"))

# .. Prediction params -------------------------------------------------------------------------------------
# Check whether prediction params are ok to predict/plot
setGeneric("clv.controlflow.check.prediction.params", def = function(clv.fitted)
  standardGeneric("clv.controlflow.check.prediction.params"))


# . Plot ----------------------------------------------------------------------------------------------------
# clv.controlflow.plot.check.inputs is needed for fitted.dyncov models only to check dyncov length
setGeneric("clv.controlflow.plot.check.inputs", def = function(obj, prediction.end, cumulative, plot, label.line, verbose)
  standardGeneric("clv.controlflow.plot.check.inputs"))

# . Predict new customer -----------------------------------------------------------------------------------------
setGeneric("clv.controlflow.predict.new.customer", def = function(clv.fitted, clv.newcustomer){
  standardGeneric("clv.controlflow.predict.new.customer")
})

# . Bootstrapping ------------------------------------------------------------------------------------------

# Estimate the model again on new data with its original specification
# pass arguments incl optimx.args and overwrite any existing settings in ...
setGeneric("clv.fitted.estimate.same.specification.on.new.data", function(clv.fitted, newdata, ...){
  standardGeneric("clv.fitted.estimate.same.specification.on.new.data")
})

# Generate many predictions by re-fitting the given model on bootstrapped
# transaction data and predicting on it
setGeneric("clv.fitted.bootstrap.predictions", function(clv.fitted, num.boots, verbose, ...){
  standardGeneric("clv.fitted.bootstrap.predictions")
})



# Model specific steps ------------------------------------------------------------------------------------------------------------

# . For all (base) models -----------------------------------------------------------------------------------------------------------

# .. Estimate ----------------------------------------------------------------------------------------------------------------------
# Perform model specific checks on user inputs to estimate
setGeneric("clv.model.check.input.args", def = function(clv.model, clv.fitted, start.params.model, optimx.args, verbose, ...)
  standardGeneric("clv.model.check.input.args"))

# Store additional arguments potentially given in estimate
setGeneric("clv.model.put.estimation.input", def = function(clv.model, ...)
  standardGeneric("clv.model.put.estimation.input"))

setGeneric(name="clv.model.generate.start.param.cor", def = function(clv.model, start.param.cor, transformed.start.params.model)
  standardGeneric("clv.model.generate.start.param.cor"))

# Finish the arguments to optimx with model specific arguments (mostly LL)
setGeneric(name="clv.model.prepare.optimx.args", def=function(clv.model, clv.fitted, prepared.optimx.args)
  standardGeneric("clv.model.prepare.optimx.args"))

# Transform standard or user given start params to optimizer (prefixed) scale
setGeneric(name="clv.model.transform.start.params.model", def=function(clv.model, original.start.params.model)
  standardGeneric("clv.model.transform.start.params.model"))

# Transform prefixed params to original scale
setGeneric(name="clv.model.backtransform.estimated.params.model", def=function(clv.model, prefixed.params.model)
  standardGeneric("clv.model.backtransform.estimated.params.model"))

# ie post.estimation.steps
setGeneric(name="clv.model.process.post.estimation", def=function(clv.model, clv.fitted, res.optimx)
  standardGeneric("clv.model.process.post.estimation"))

# . Density for spending models
setGeneric(name="clv.model.probability.density", def=function(clv.model, x, clv.fitted)
  standardGeneric("clv.model.probability.density"))


# .. Correlation ---------------------------------------------------------------------------------
# Whether the model in general supports life/trans correlation
setGeneric(name="clv.model.supports.correlation", def = function(clv.model)
  standardGeneric("clv.model.supports.correlation"))

# Whether this fit used correlation
setGeneric(name="clv.model.estimation.used.correlation", def = function(clv.model)
  standardGeneric("clv.model.estimation.used.correlation"))

# Adds the correlation parameter to a given vector of params after reading it from the optimx results
setGeneric(name="clv.model.coef.add.correlation", def = function(clv.model, last.row.optimx.coef, original.scale.params)
  standardGeneric("clv.model.coef.add.correlation"))

setGeneric(name="clv.model.m.to.cor", def = function(clv.model, prefixed.params.model, param.m)
  standardGeneric("clv.model.m.to.cor"))

setGeneric(name="clv.model.cor.to.m", def = function(clv.model, prefixed.params.model, param.cor)
  standardGeneric("clv.model.cor.to.m"))

# .. Predict ----------------------------------------------------------------------------------------------------------------------
# Predict clv per model
setGeneric(name = "clv.model.predict", def = function(clv.model, clv.fitted, dt.predictions, verbose, ...)
  standardGeneric("clv.model.predict"))

setGeneric(name = "clv.model.expectation", def = function(clv.model, clv.fitted, dt.expectation.seq, verbose)
  standardGeneric("clv.model.expectation"))

# .. Generics --------------------------------------------------------------------------------------------------------------------
# return diag matrix to correct for transformations because inv(hessian) != vcov for transformed params
setGeneric(name="clv.model.vcov.jacobi.diag", def=function(clv.model, clv.fitted, prefixed.params)
  standardGeneric("clv.model.vcov.jacobi.diag"))

# .. Newdata ---------------------------------------------------------------------------------------------------------------
# Do the steps necessary to integrate user newdata in the fitted model (ie do cbs etc)
setGeneric(name="clv.model.process.newdata", def=function(clv.model, clv.fitted, user.newdata, verbose)
  standardGeneric("clv.model.process.newdata"))

# .. PMF --------------------------------------------------------------------------------------------------------------------
setGeneric(name="clv.model.pmf", def=function(clv.model, clv.fitted, x)
  standardGeneric("clv.model.pmf"))

# .. New customer prediction -----------------------------------------------------------------------------------------------
setGeneric("clv.model.predict.new.customer", function(clv.model, clv.fitted, clv.newcustomer)
  standardGeneric("clv.model.predict.new.customer"))




# . For covariate models -----------------------------------------------------------------------------------------------------------

# .. Estimate -----------------------------------------------------------------------------------------------------------
# for a single process, because could be that start params for one process were different
setGeneric("clv.model.transform.start.params.cov", def = function(clv.model, start.params.cov)
  standardGeneric("clv.model.transform.start.params.cov"))

# Transform prefixed params to original scale
setGeneric(name="clv.model.backtransform.estimated.params.cov", def=function(clv.model, prefixed.params.cov)
  standardGeneric("clv.model.backtransform.estimated.params.cov"))


# clv.time ----------------------------------------------------------------------------------------------------

setGeneric("clv.time.epsilon", function(clv.time)
  standardGeneric("clv.time.epsilon"))

# convert user given date/datetimes
setGeneric("clv.time.convert.user.input.to.timepoint", function(clv.time, user.timepoint)
  standardGeneric("clv.time.convert.user.input.to.timepoint"))

setGeneric("clv.time.interval.in.number.tu", def = function(clv.time, interv)
  standardGeneric("clv.time.interval.in.number.tu"))

setGeneric("clv.time.number.timeunits.to.timeperiod", function(clv.time, user.number.periods)
  standardGeneric("clv.time.number.timeunits.to.timeperiod"))

setGeneric("clv.time.tu.to.ly", function(clv.time)
  standardGeneric("clv.time.tu.to.ly"))

setGeneric("clv.time.floor.date", function(clv.time, timepoint)
  standardGeneric("clv.time.floor.date"))

# only for pnbd dyncov createwalks
setGeneric("clv.time.ceiling.date", function(clv.time, timepoint)
  standardGeneric("clv.time.ceiling.date"))

setGeneric("clv.time.format.timepoint", function(clv.time, timepoint)
  standardGeneric("clv.time.format.timepoint"))


# clv.data ----------------------------------------------------------------------------------------------------
# Create clv.data object with same config asbut only transactions and covariates of given ids
setGeneric("clv.data.create.bootstrapping.data", def = function(clv.data, ids){
  standardGeneric("clv.data.create.bootstrapping.data")
})



# S3 Generics ---------------------------------------------------------------
#' Coerce to clv.data object
#'
#' Functions to coerce transaction data to a \code{clv.data} object.
#'
#' @param x Transaction data.
#' @templateVar name_param_trans x
#' @template template_params_clvdata
#' @template template_param_dots
#'
#' @details
#' See section "Details" of \link{clvdata} for more details on parameters and usage.
#'
#' @examples
#' \donttest{ # dont test because ncpu=2 limit on cran (too fast)
#' data(cdnow)
#'
#' # Turn data.table of transaction data into a clv.data object,
#' #  using default date format and column names but no holdout period
#' clv.cdnow <- as.clv.data(cdnow)
#' }
#'
#' @export
as.clv.data <- function(x,
                        date.format="ymd",
                        time.unit="weeks",
                        estimation.split = NULL,
                        observation.end = NULL,
                        name.id="Id", name.date="Date", name.price="Price",
                        ...){
  UseMethod("as.clv.data", x)
}

