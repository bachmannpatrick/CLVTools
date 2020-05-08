#' @exportMethod bgnbd
setGeneric("bgnbd", def = function(clv.data, start.params.model=c(), use.cor = FALSE, start.param.cor=c(),
                                  optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("bgnbd"))



#' @name bgnbd
#' @title BG/NBD models
#'
#' @template template_params_estimate
#' @template template_param_verbose
#' @template template_params_estimate_cov
#' @template template_param_dots
#'
#' @references
#' BG/NBD model according to
#' \url{https://github.com/cran/BTYD}
#'
#' @description
#' Fits BG/NBD models on transactional data without covariates.
#'
#' @param use.cor (Not yet supported) Whether the correlation between the transaction and lifetime process should be estimated.
#' @param start.param.cor (Not yet supported) Start parameter for the optimization of the correlation.
#'
#' @details
#' Model parameters for the BG/NBD model are \code{r, alpha, a, and b}. \cr
#' \code{r}: shape parameter of the Gamma distribution of the purchase process.
#' \code{alpha}: scale parameter of the Gamma distribution of the purchase process.
#' \code{a}: shape parameter of the Beta distribution of the dropout process.
#' \code{b}: shape parameter of the Beta distribution of the dropout process.
#'
#' If no start parameters are given, r = 1, alpha = 3, a = 1, b = 3 is used.
#' The model start parameters are required to be > 0.
#'
#' \subsection{The BG/NBD model}{
#' The BG/NBD is an "easy" alternative to the Pareto/NBD model that is easyer to implement. The BG/NBD model slight adapts
#' the beavioral "story" associated with the Pareto/NBD model inorder to simplyfiy the implementation. The BG/NBD model uses a beta-geometric and
#' exponentioal gamma misture distributions to model customer behavior. The key difference to the Pareto/NBD model that a customer can only
#' churn right after a transaction. This simplifies computations signifiantly, however has the drawback that a customer cannot chun until he/she
#' makes a transction. The Pareto/NBD model assumes that a customer can churn any time.
#' }
#'
#' \subsection{BG/NBD model with static covariates}{
#' The standard BG/NBD model captures heterogeneity was solely using Gamma distributions.
#' However, often exogenous knowledge, such as for example customer demographics, is available.
#' The supplementary knowledge may explain part of the heterogeneity among the customers and
#' therefore increase the predictive accuracy of the model. In addition, we can rely on these
#' parameter estimates for inference, i.e. identify and quantify effects of contextual factors
#' on the two underlying purchase and attrition processes. For technical details we refer to
#' the technical note by Fader and Hardie (2007).
#' }
#'
#' @return
#' \code{bgnbd} returns an object of
#' class \code{clv.bgnbd}.
#'
#' The function \code{\link[CLVTools:summary.clv.fitted]{summary}} can be used to obtain and print a summary of the results.
#' The generic accessor functions \code{coefficients}, \code{fitted},
#' \code{residuals}, \code{vcov}, \code{logLik}, \code{AIC}, \code{BIC}, and \code{nobs} are available.
#'
#' @seealso \code{\link[CLVTools:clvdata]{clvdata}} to create a clv data object
#' @seealso \code{\link[CLVTools:predict.clv.fitted]{predict}} to predict expected transactions, probability of being alive, and customer lifetime value for every customer
#' @seealso \code{\link[CLVTools:plot.clv.fitted]{plot}} to plot the unconditional expectation as predicted by the fitted model
#' @seealso The generic functions \code{\link[CLVTools:summary.clv.fitted]{summary}} and \code{\link[CLVTools:fitted.clv.fitted]{fitted}}.
#'
#' @template template_bgnbd_reference
#'
#' @examples
#' \donttest{
#' data("cdnow")
#' clv.data.cdnow <- clvdata(cdnow, date.format = "ymd",
#'                             time.unit = "w", estimation.split = 37)
#'
#' # Fit standard BG/NBD model
#' bgnbd(clv.data.cdnow)
#'
#' # Give initial guesses for the Model parameters
#' bgnbd(clv.data.cdnow,
#'      start.params.model = c(r=1.1, alpha=3.4, a=1, b=3))
#'
#'
#' # pass additional parameters to the optimizer (optimx)
#' #    Use Nelder-Mead as optimization method and print
#' #    detailed information about the optimization process
#' estimation.bgnbd <- bgnbd(clv.data.cdnow,
#'                      optimx.args = list(method="Nelder-Mead",
#'                                         control=list(trace=6)))
#'
#' # estimated coefs
#' coef(estimation.bgnbd)
#'
#' # summary of the fitted model
#' summary(estimation.bgnbd)
#'
#' predict.bgnbd <- predict(estimation.bgnbd, prediction.end = "2011-12-31")
#'
#' plot(estimation.bgnbd)
#' }
#' @aliases bgnbd bgnbd,clv.data-method
#' @include class_clv_data.R class_clv_model_bgnbd_nocov.R
#' @export
setMethod("bgnbd", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                    start.params.model=c(),
                                                                                    use.cor = FALSE,
                                                                                    start.param.cor=c(),
                                                                                    optimx.args=list(),
                                                                                    verbose=TRUE,...){
  cl <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.bgnbd(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted = obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose, ...))
})

#' @rdname bgnbd
#' @include class_clv_data_staticcovariates.R
#' @aliases bgnbd,clv.data.static.covariates-method
#' @export
setMethod("bgnbd", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
                                                                                                      start.params.model=c(),
                                                                                                      use.cor = FALSE,
                                                                                                      start.param.cor=c(),
                                                                                                      optimx.args=list(),
                                                                                                      verbose=TRUE,
                                                                                                      names.cov.life=c(), names.cov.trans=c(),
                                                                                                      start.params.life=c(), start.params.trans=c(),
                                                                                                      names.cov.constr=c(),start.params.constr=c(),
                                                                                                      reg.lambdas = c(), ...){

  cl <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.bgnbd.static.cov(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor, start.param.cor = start.param.cor,
                                           optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas, ...))
})


#' @keywords internal
#' @include class_clv_data_dynamiccovariates.R
#' @aliases bgnbd,clv.data.dynamic.covariates-method.R
setMethod("bgnbd", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                        start.params.model=c(),
                                                                                                        optimx.args=list(),
                                                                                                        verbose=TRUE,
                                                                                                        names.cov.life=c(), names.cov.trans=c(),
                                                                                                        start.params.life=c(), start.params.trans=c(),
                                                                                                        names.cov.constr=c(),start.params.constr=c(),
                                                                                                        reg.lambdas = c(), ...){
  stop("This model cannot be fitted on this type of data!")
})
