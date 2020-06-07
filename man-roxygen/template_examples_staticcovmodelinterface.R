#' @examples \donttest{
#' # To estimate the <%=name_model_short%> model with static covariates,
#' #   add static covariates to the data
#' data("apparelStaticCov")
#' clv.data.static.cov <-
#'  SetStaticCovariates(clv.data.apparel,
#'                      data.cov.life = apparelStaticCov,
#'                      names.cov.life = c("Gender", "Channel"),
#'                      data.cov.trans = apparelStaticCov,
#'                      names.cov.trans = c("Gender", "Channel"))
#'
#' # Fit <%=name_model_short%> with static covariates
#' <%=name_model_short%>(clv.data.static.cov)
#'
#' # Give initial guesses for both covariate parameters
#' <%=name_model_short%>(clv.data.static.cov, start.params.trans = c(Gender=0.75, Channel=0.7),
#'                    start.params.life  = c(Gender=0.5, Channel=0.5))
#'
#' # Use regularization
#' <%=name_model_short%>(clv.data.static.cov, reg.lambdas = c(trans = 5, life=5))
#'
#' # Force the same coefficient to be used for both covariates
#' <%=name_model_short%>(clv.data.static.cov, names.cov.constr = "Gender",
#'                    start.params.constr = c(Gender=0.5))
#'
#' # Fit model only with the Channel covariate for life but
#' # keep all trans covariates as is
#' <%=name_model_short%>(clv.data.static.cov, names.cov.life = c("Channel"))
#' }
