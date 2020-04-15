#' @name pnbd
#'
#' @title Pareto/NBD models
#'
#' @template template_params_estimate
#' @template template_params_estimate_cov
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @description
#' Fits Pareto/NBD models on transactional data with and without covariates.
#'
#'
#' @details
#' Model parameters for the Pareto/NBD model are \code{alpha, r, beta, and s}. \cr
#' \code{s}: shape parameter of the Gamma distribution for the lifetime process.
#' The smaller s, the stronger the heterogeneity of customer lifetimes. \cr
#' \code{beta}: scale parameter for the Gamma distribution for the lifetime process. \cr
#' \code{r}: shape parameter of the Gamma distribution of the purchase process.
#' The smaller r, the stronger the heterogeneity of the purchase process.\cr
#' \code{alpha}: scale parameter of the Gamma distribution of the purchase process.
#'
#' Ideally, the starting parameters for r and s represent your best guess
#' concerning the heterogeneity of customers in their buy and die rate.
#' If covariates are included into the model additionally parameters for the
#' covariates affecting the attrition and the purchase process are part of the model.
#'
#' If no start parameters are given, 1.0 is used for all model parameters and 0.1 for covariate parameters.
#' The model start parameters are required to be > 0.
#'
#' \subsection{The Pareto/NBD model}{
#' The Pareto/NBD was the first model addressing the issue of modeling customer purchases and
#' attrition simultaneously for non-contractual settings. The model uses a Pareto distribution,
#' a combination of an Exponential and a Gamma distribution, to explicitly model customers'
#' (unobserved) attrition behavior in addition to customers' purchase process.\cr
#' In general, the Pareto/NBD model consist of two parts. A first process models the purchase
#' behavior of customers as long as the customers are active. A second process models customers'
#' attrition. Customers live (and buy) for a certain unknown time until they become inactive
#' and "die". Customer attrition is unobserved. Inactive customers may not be reactivated.
#' For technical details we refer to the original paper by Schmittlein, Morrison and Colombo
#' (1987) and the detailed technical note of Fader and Hardie (2005).
#' }
#'
#' \subsection{Pareto/NBD model with static covariates}{
#' The standard Pareto/NBD model captures heterogeneity was solely using Gamma distributions.
#' However, often exogenous knowledge, such as for example customer demographics, is available.
#' The supplementary knowledge may explain part of the heterogeneity among the customers and
#' therefore increase the predictive accuracy of the model. In addition, we can rely on these
#' parameter estimates for inference, i.e. identify and quantify effects of contextual factors
#' on the two underlying purchase and attrition processes. For technical details we refer to
#' the technical note by Fader and Hardie (2007).
#' }
#'
#' \subsection{Pareto/NBD model with dynamic covariates}{
#' In many real-world applications customer purchase and attrition behavior may be
#' influenced by covariates that vary over time. In consequence, the timing of a purchase
#' and the corresponding value of at covariate a that time becomes relevant. Time-varying
#' covariates can affect customer on aggregated level as well as on an individual level:
#' In the first case, all customers are affected simultaneously, in the latter case a
#' covariate is only relevant for a particular customer. For technical details we refer to
#' the paper by Bachmann, Meierer and Näf (2019).
#' }
#'
#' @note
#' Fitting the Pareto/NBD model with dynamic covariates is for the most part implemented using \code{data.table} and to a smaller part further
#' parallelized with the \code{foreach} package. Registering a
#' parallel backend with \code{\link[doFuture:doFuture]{doFuture}} or \code{\link[doParallel:doParallel]{doParallel}} before fitting the
#' models allows to take advantage of this. If no parallel backend is set up, the \code{foreach} package gives a friendly reminder that
#' it is executed sequentially. In case this is desired but no warning should be given, a parallel backend in sequential mode
#' can be set up, for example package \code{doFuture} with \code{\link[future:plan]{plan("sequential")}}.
#'
#' The part executed with \code{foreach} also heavily relies on \code{data.table} which is natively parallelized already. When setting up
#' the parallel backend, great care should be taken to reduce the overhead from this nested parallelism as otherwise it can \emph{increase} runtime.
#' See \code{\link[data.table:setDTthreads]{setDTthreads}}, \code{\link[data.table:getDTthreads]{getDTthreads}},
#' and \code{\link[future:plan]{plan}} for information on how to do this.
#'
#' @return
#' Depending on the data object on which the model was fit, \code{pnbd} returns either an object of
#' class \link[CLVTools:clv.pnbd-class]{clv.pnbd}, \link[CLVTools:clv.pnbd.static.cov-class]{clv.pnbd.static.cov}, or \link[CLVTools:clv.pnbd.dynamic.cov-class]{clv.pnbd.dynamic.cov}.
#'
#' The function \code{\link[CLVTools:summary.clv.fitted]{summary}} can be used to obtain and print a summary of the results.
#' The generic accessor functions \code{coefficients}, \code{fitted},
#' \code{residuals}, \code{vcov}, \code{logLik}, \code{AIC}, \code{BIC}, and \code{nobs} are available.
#'
#' @seealso \code{\link[CLVTools:clvdata]{clvdata}} to create a clv data object
#' @seealso \code{\link[CLVTools:SetStaticCovariates]{SetStaticCovariates}} and \code{\link[CLVTools:SetDynamicCovariates]{SetDynamicCovariates}} to add static or dynamic covariates to an existing clv data object on which then the \code{pnbd} method can be fit
#' @seealso \code{\link[CLVTools:predict.clv.fitted]{predict}} to predict expected transactions, probability of being alive, and customer lifetime value for every customer
#' @seealso \code{\link[CLVTools:plot.clv.fitted]{plot}} to plot the unconditional expectation as predicted by the fitted model
#' @seealso The generic functions \code{\link[CLVTools:summary.clv.fitted]{summary}} and \code{\link[CLVTools:fitted.clv.fitted]{fitted}}.
#' @seealso \code{\link[data.table]{setDTthreads}}, \code{\link[data.table]{getDTthreads}},\code{\link[doParallel]{registerDoParallel}},\code{\link[doFuture]{registerDoFuture}} for setting up paralle exectution.
#'
#' @template template_pnbd_reference
#'
#' @examples
#' \donttest{
#'
#' data("apparelTrans")
#' clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd",
#'                             time.unit = "w", estimation.split = 37)
#'
#' # Fit standard PNBD model
#' pnbd(clv.data.apparel)
#'
#' # Give initial guesses for the Model parameters
#' pnbd(clv.data.apparel,
#'      start.params.model = c(r=0.5, alpha=15, s=0.5, beta=10))
#'
#' # Estimate correlation as well
#' pnbd(clv.data.apparel, use.cor = TRUE)
#'
#' # pass additional parameters to the optimizer (optimx)
#' #    Use Nelder-Mead as optimization method and print
#' #    detailed information about the optimization process
#' apparel.pnbd <- pnbd(clv.data.apparel,
#'                      optimx.args = list(method="Nelder-Mead",
#'                                         control=list(trace=6)))
#'
#' # estimated coefs
#' coef(apparel.pnbd)
#'
#' # summary of the fitted model
#' summary(apparel.pnbd)
#'
#'
#' # To estimate the PNBD model with static covariates,
#' #   add static covariates to the data
#' data("apparelDemographics")
#' clv.data.static.cov <-
#'  SetStaticCovariates(clv.data.apparel,
#'                      data.cov.life = apparelDemographics,
#'                      names.cov.life = "Gender",
#'                      data.cov.trans = apparelDemographics,
#'                      names.cov.trans = "Gender")
#'
#' # Fit PNBD with static covariates
#' pnbd(clv.data.static.cov)
#'
#' # Give initial guesses for both covariate parameters
#' pnbd(clv.data.static.cov, start.params.trans = c(Gender=0.75),
#'                    start.params.life  = c(Gender=0.5))
#'
#' # Use regularization
#' pnbd(clv.data.static.cov, reg.lambdas = c(trans = 5, life=5))
#'
#' # Force the same coefficient to be used for both covariates
#' pnbd(clv.data.static.cov, names.cov.constr = "Gender",
#'                    start.params.constr = c(Gender=0.5))
#'
#' # Use only the covariates named
#' # **TODO: Need data with > 1 cov
#'
#'
#'
#' # Add dynamic covariates data to the data object
#  # To estimate the PNBD model with dynamic covariates,
#' #   add dynamic covariates to the data
#' data("apparelDynCov")
#' \dontrun{
#' clv.data.dyn.cov <-
#'   SetDynamicCovariates(clv.data = clv.data.apparel,
#'                        data.cov.life = apparelDynCov,
#'                        data.cov.trans = apparelDynCov,
#'                        names.cov.life = c("DM", "High.Season", "Gender"),
#'                        names.cov.trans = c("DM", "High.Season", "Gender"),
#'                        name.date = "Cov.Date")
#' \dontrun{
#' # Enable parallel execution of some parts of the dyncov LL
#' library(doFuture)
#' registerDoFuture()
#' # avoid overhead from nested parallelism by setting up
#' # appropriate to _your_ system
#' setDTthreads(threads=8)
#' plan("multisession", workers=2)
#' }
#'
#' # Fit PNBD with dynamic covariates
#' pnbd(clv.data.dyn.cov)
#'
#' # The same fitting options as for the
#' #  static covariate are available
#' pnbd(clv.data.dyn.cov, reg.lambdas = c(trans=10, life=2))
#'
#' }
#' }
#'
NULL


#' @exportMethod pnbd
setGeneric("pnbd", def = function(clv.data, start.params.model=c(), use.cor = FALSE, start.param.cor=c(),
                                  optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("pnbd"))



#' @include class_clv_data.R
#' @rdname pnbd
setMethod("pnbd", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                        start.params.model=c(),
                                                                                        use.cor = FALSE,
                                                                                        start.param.cor=c(),
                                                                                        optimx.args=list(),
                                                                                        verbose=TRUE,...){
  cl  <- sys.call(1)
  obj <- clv.pnbd(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(obj=obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose, ...))
})

#' @include class_clv_data_staticcovariates.R
#' @rdname pnbd
setMethod("pnbd", signature = signature(clv.data="clv.data.static.covariates"), definition = function(clv.data,
                                                                                                      start.params.model=c(),
                                                                                                      use.cor = FALSE,
                                                                                                      start.param.cor=c(),
                                                                                                      optimx.args=list(),
                                                                                                      verbose=TRUE,
                                                                                                      names.cov.life=c(), names.cov.trans=c(),
                                                                                                      start.params.life=c(), start.params.trans=c(),
                                                                                                      names.cov.constr=c(),start.params.constr=c(),
                                                                                                      reg.lambdas = c(), ...){

  cl  <- sys.call(1)
  obj <- clv.pnbd.static.cov(cl=cl, clv.data=clv.data)

  # Do the estimate controlflow / process steps with the static cov object
  return(clv.template.controlflow.estimate(obj=obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor, start.param.cor = start.param.cor,
                                           optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas, ...))
})



#' @include class_clv_data_dynamiccovariates.R
#' @rdname pnbd
setMethod("pnbd", signature = signature(clv.data="clv.data.dynamic.covariates"), definition = function(clv.data,
                                                                                                        start.params.model=c(),
                                                                                                        use.cor = FALSE,
                                                                                                        start.param.cor=c(),
                                                                                                        optimx.args=list(),
                                                                                                        verbose=TRUE,
                                                                                                        names.cov.life=c(), names.cov.trans=c(),
                                                                                                        start.params.life=c(), start.params.trans=c(),
                                                                                                        names.cov.constr=c(),start.params.constr=c(),
                                                                                                        reg.lambdas = c(), ...){
  cl  <- sys.call(1)
  obj <- clv.pnbd.dynamic.cov(cl = cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(obj=obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor, start.param.cor = start.param.cor,
                                           optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas, ...))
})

