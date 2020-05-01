#' @exportMethod ggomnbd
setGeneric("ggomnbd", def = function(clv.data, start.params.model=c(), use.cor = FALSE, start.param.cor=c(),
                                     optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("ggomnbd"))


#' @name ggomnbd
#' @title Gamma-Gompertz/NBD models
#'
#' @description Fits Gamma-Gompertz/NBD models on transactional data with static and without covariates.
#' @template template_params_estimate
#' @template template_params_estimate_cov
#' @template template_param_verbose
#' @template template_param_dots
#' @param use.cor Whether the correlation between the transaction and lifetime process should be estimated.
#' @param start.param.cor Start parameter for the optimization of the correlation.
#' @description
#' Fits GGompertz /NBD models on transactional data with and without covariates.
#'
#'
#' @details
#' Model parameters for the GGompertz /NBD model are \code{r, alpha, beta, b and s}. \cr
#' \code{r}: TODO \cr
#' \code{alpha}: TODO \cr
#' \code{beta}: TODO \cr
#' \code{b}: TODO \cr
#' \code{s}: TODO
#'
#' If no start parameters are given, r = 1, alpha = 1, beta = 1, b = 1, s = 1 is used.
#' The model start parameters are required to be > 0.
#'
#' \subsection{The GGompertz / NBD model}{
#' TODO
#' }
#'
#' @return
#' \code{ggomnbd} returns an object of
#' class \code{clv.ggomnbd}.
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
#' @examples
#' \donttest{
#' data("cdnow")
#' clv.data.cdnow <- clvdata(cdnow, date.format = "ymd",
#'                             time.unit = "w", estimation.split = 37)
#'
#' # Fit standard BG/NBD model
#' ggomnbd(clv.data.cdnow)
#'
#' # Give initial guesses for the Model parameters
#' ggomnbd(clv.data.cdnow,
#'      start.params.model = c(r=1, alpha=1, beta=1, b=1, s=1))
#'
#'
#' # pass additional parameters to the optimizer (optimx)
#' #    Use Nelder-Mead as optimization method and print
#' #    detailed information about the optimization process
#' estimation.ggomnbd <- ggomnbd(clv.data = clv.data.cdnow,
#'                                   start.params.model = c(r = 1, alpha = 1, beta = 1, b = 1, s = 1))
#'
#' # estimated coefs
#' coef(estimation.ggomnbd)
#'
#' # summary of the fitted model
#' summary(estimation.ggomnbd)
#'
#' predict.ggomnbd <- predict(estimation.ggomnbd, prediction.end = "2011-12-31")
#'
#' plot(estimation.ggomnbd)
#' }
#' @aliases ggomnbd ggomnbd,clv.data-method
#' @include class_clv_data.R
#' @rdname ggomnbd
setMethod("ggomnbd", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                       start.params.model=c(),
                                                                                       use.cor = FALSE,
                                                                                       start.param.cor=c(),
                                                                                       optimx.args=list(),
                                                                                       verbose=TRUE,...){
  cl        <- sys.call(1)
  obj <- clv.ggomnbd(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose, ...))
})


#' @include class_clv_data_staticcovariates.R
#' @rdname ggomnbd
setMethod("ggomnbd", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
                                                                                                       start.params.model=c(),
                                                                                                       use.cor = FALSE,
                                                                                                       start.param.cor = c(),
                                                                                                       optimx.args=list(),
                                                                                                       verbose=TRUE,
                                                                                                       names.cov.life=c(), names.cov.trans=c(),
                                                                                                       start.params.life=c(), start.params.trans=c(),
                                                                                                       names.cov.constr=c(), start.params.constr=c(),
                                                                                                       reg.lambdas = c(), ...){
  cl        <- sys.call(1)
  obj <- clv.ggomnbd.static(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas, ...))
})



#' @include class_clv_data_dynamiccovariates.R
#' @rdname ggomnbd
setMethod("ggomnbd", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                        start.params.model=c(),
                                                                                                        use.cor = FALSE,
                                                                                                        start.param.cor = c(),
                                                                                                        optimx.args=list(),
                                                                                                        verbose=TRUE,
                                                                                                        names.cov.life=c(), names.cov.trans=c(),
                                                                                                        start.params.life=c(), start.params.trans=c(),
                                                                                                        names.cov.constr=c(),start.params.constr=c(),
                                                                                                        reg.lambdas = c(), ...){
  stop("This model cannot be fitted on this type of data!")
})

