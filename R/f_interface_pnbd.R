#' @name pnbd
#'
#' @title Pareto/NBD models
#'
#' @template template_params_estimate
#' @template template_params_estimate_cov
#' @template template_param_verbose
#' @template template_param_dots
#'
#' @param use.cor Whether the correlation between the transaction and lifetime process should be estimated.
#' @param start.param.cor Start parameter for the optimization of the correlation.
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
#' Based on these parameters, the average purchase rate while customers are active
#' is r/alpha and the average dropout rate is s/beta.
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
#' The Pareto/NBD is the first model addressing the issue of modeling customer purchases and
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
#' the paper by Bachmann, Meierer and Näf (2020).
#' }
#'
#' @note
#' Fitting the Pareto/NBD model with dynamic covariates is for the most part implemented using \code{data.table} and to a smaller part further
#' parallelized with the \code{foreach} package. Registering a
#' parallel backend with \code{\link[doFuture]{doFuture}} or \code{\link[doParallel]{doParallel}} before fitting the
#' models allows to take advantage of this. If no parallel backend is set up, the \code{foreach} package gives a friendly reminder that
#' it is executed sequentially. In case this is desired but no warning should be given, a parallel backend in sequential mode
#' can be set up, for example package \code{doFuture} with \code{\link[future:plan]{plan("sequential")}}.
#'
#' The part executed with \code{foreach} also heavily relies on \code{data.table} which is natively parallelized already. When setting up
#' the parallel backend, great care should be taken to reduce the overhead from this nested parallelism as otherwise it can \emph{increase} runtime.
#' See \code{\link[data.table]{setDTthreads}}, \code{\link[data.table]{getDTthreads}},
#' and \code{\link[future]{plan}} for information on how to do this.
#'
#' The Pareto/NBD model with dynamic covariates can currently not be fit with data that has a temporal resolution
#' of less than one day (data that was built with time unit \code{hours}).
#'
#' @return Depending on the data object on which the model was fit, \code{pnbd} returns either an object of
#' class \linkS4class{clv.pnbd}, \linkS4class{clv.pnbd.static.cov}, or \linkS4class{clv.pnbd.dynamic.cov}.
#'
#' @template template_clvfitted_returnvalue
#'
#' @template template_clvfittedtransactions_seealso
#' @seealso \code{\link[CLVTools:SetDynamicCovariates]{SetDynamicCovariates}} to add dynamic covariates on which the \code{pnbd} model can be fit.
#'
#' @seealso \code{\link[data.table]{setDTthreads}}, \code{\link[data.table]{getDTthreads}},\code{\link[doParallel]{registerDoParallel}},\code{\link[doFuture]{registerDoFuture}} for setting up parallel execution.
#'
#' @template template_references_pnbd
#'
#' @templateVar name_model_short pnbd
#' @templateVar vec_startparams_model c(r=0.5, alpha=15, s=0.5, beta=10)
#' @template template_examples_nocovmodelinterface
#' @examples \donttest{
#' # Estimate correlation as well
#' pnbd(clv.data.apparel, use.cor = TRUE)
#' }
#' @templateVar name_model_short pnbd
#' @template template_examples_staticcovmodelinterface
#' @examples
#' # Add dynamic covariates data to the data object
#  # To estimate the PNBD model with dynamic covariates,
#' #   add dynamic covariates to the data
#' \donttest{
#' \dontrun{
#' data("apparelDynCov")
#' clv.data.dyn.cov <-
#'   SetDynamicCovariates(clv.data = clv.data.apparel,
#'                        data.cov.life = apparelDynCov,
#'                        data.cov.trans = apparelDynCov,
#'                        names.cov.life = c("Marketing", "Gender", "Channel"),
#'                        names.cov.trans = c("Marketing", "Gender", "Channel"),
#'                        name.date = "Cov.Date")
#'
#' # Enable parallel execution of some parts of the dyncov LL
#' library(doFuture)
#' registerDoFuture()
#' # avoid overhead from nested parallelism by setting up
#' # appropriate to _your_ system
#' setDTthreads(threads=8)
#' plan("multisession", workers=2)
#'
#' # Fit PNBD with dynamic covariates
#' pnbd(clv.data.dyn.cov)
#'
#' # The same fitting options as for the
#' #  static covariate are available
#' pnbd(clv.data.dyn.cov, reg.lambdas = c(trans=10, life=2))
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

  check_err_msg(check_user_data_emptyellipsis(...))

  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.pnbd(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose))
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

  check_err_msg(check_user_data_emptyellipsis(...))

  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.pnbd.static.cov(cl=cl, clv.data=clv.data)

  # Do the estimate controlflow / process steps with the static cov object
  return(clv.template.controlflow.estimate(clv.fitted=obj, start.params.model = start.params.model, use.cor = use.cor, start.param.cor = start.param.cor,
                                           optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas))
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

  check_err_msg(check_user_data_emptyellipsis(...))

  cl  <- match.call(call = sys.call(-1), expand.dots = TRUE)

  if(is(clv.data@clv.time, "clv.time.datetime")){
    stop("This model currently cannot be fitted with data that has a temporal resolution of less than 1d (ie hours).")
  }

  obj <- clv.pnbd.dynamic.cov(cl = cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted=obj, start.params.model = start.params.model, use.cor = use.cor, start.param.cor = start.param.cor,
                                           optimx.args = optimx.args, verbose=verbose,
                                           names.cov.life=names.cov.life, names.cov.trans=names.cov.trans,
                                           start.params.life=start.params.life, start.params.trans=start.params.trans,
                                           names.cov.constr=names.cov.constr,start.params.constr=start.params.constr,
                                           reg.lambdas = reg.lambdas))
})

