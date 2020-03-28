#' Transactional and dynamic covariates data to fit CLV models
#'
#' @description
#' Extends the class \code{\link[CLVTools:clv.data.static.covariates-class]{clv.data.static.covariates}},
#' but adds no additional slots to it. The purpose of this class rather is to define different behaviors
#' suitable for dynamic covariates.
#'
#' The \code{data.table}s stored in the slots \code{data.cov.life} and \code{data.cov.trans}
#' each contain an additional column \code{Cov.Date} for the timepoint of the covariate.
#'
#' An object of this class serves as input to fit models with dynamic covariates.
#'
#'
#' @slot data.cov.life Single \code{data.table} with all static covariate data for the lifetime process
#' @slot data.cov.trans Single \code{data.table} with all static covariate data for the transaction process
#' @slot names.cov.data.life Character vector with names of the dynamic lifetime covariates.
#' @slot names.cov.data.trans Character vector with names of the dynamic transaction covariates.
#'
#' @seealso Definition of the parent class \code{\link[CLVTools:clv.data.static.covariates-class]{clv.data.static.covariates}}.
#' @seealso For fitting dynamic covariate models: \code{\link[CLVTools:pnbd]{pnbd}}
#'
# Corresponds to the column names of the \code{data.table} in slot data.cov.trans
#'
#' @keywords internal
#' @importFrom methods setClass
#' @include all_generics.R class_clv_data.R class_clv_data_staticcovariates.R class_clv_time.R
setClass(Class = "clv.data.dynamic.covariates", contains = "clv.data.static.covariates")


# Constructor
#' @importFrom methods new
clv.data.dynamic.covariates <- function(no.cov.obj,
                                        data.cov.life,
                                        data.cov.trans,
                                        names.cov.data.life,
                                        names.cov.data.trans){

  # Ensure its really set
  setkeyv(data.cov.life, cols = c("Id", "Cov.Date"))
  setkeyv(data.cov.trans, cols = c("Id", "Cov.Date"))

  obj.cov <- new(Class = "clv.data.dynamic.covariates",
                 # Do not copy construct from clv.data directly, go through clv.data.static constructor
                 #  Pattern suggested by Martin Morgan
                 #  (https://stackoverflow.com/questions/16247583/inheritance-in-r/16248773)
                 clv.data.static.covariates(no.cov.obj = no.cov.obj,
                                            names.cov.data.life  = names.cov.data.life,
                                            names.cov.data.trans = names.cov.data.trans,

                                            data.cov.life  = data.cov.life,
                                            data.cov.trans = data.cov.trans),

                 names.cov.data.life  = names.cov.data.life,
                 names.cov.data.trans = names.cov.data.trans,
                 name = "CLV Transaction Data with Dynamic Covariates")

  return(obj.cov)
}
