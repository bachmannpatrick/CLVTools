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
#'
#' @description
#' Fits BG/NBD models on transactional data without and with static covariates.
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
#' The BG/NBD is an "easy" alternative to the Pareto/NBD model that is easier to implement. The BG/NBD model slight adapts
#' the behavioral "story" associated with the Pareto/NBD model in order to simplify the implementation. The BG/NBD model uses a beta-geometric and
#' exponential gamma mixture distributions to model customer behavior. The key difference to the Pareto/NBD model is that a customer can only
#' churn right after a transaction. This simplifies computations significantly, however has the drawback that a customer cannot churn until he/she
#' makes a transaction. The Pareto/NBD model assumes that a customer can churn at any time.
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
#' Depending on the data object on which the model was fit, \code{bgnbd} returns either an object of
#' class \link[CLVTools:clv.bgnbd-class]{clv.bgnbd} or \link[CLVTools:clv.bgnbd.static.cov-class]{clv.bgnbd.static.cov}.
#'
#' @template template_clvfitted_returnvalue
#'
#' @template template_clvfitted_seealso
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
setMethod("bgnbd", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                        start.params.model=c(),
                                                                                                        use.cor = FALSE,
                                                                                                        start.param.cor=c(),
                                                                                                        optimx.args=list(),
                                                                                                        verbose=TRUE,
                                                                                                        ...){
  stop("This model cannot be fitted on this type of data!")
})
