#' @title Summarizing a CLV data object
#' @param object A CLV data object containing transactional data and potentially also contextual factors.
# These are parameters for the print functions whose documentation link to the summary docu
#' @param x An object of class \code{"summary.clv.data"}, usually, a result of a call to \code{summary.clv.data}.
#' @param digits The number of significant digits to use when printing.
#' @param signif.stars Logical. If TRUE, ‘significance stars’ are printed for each coefficient.
#' @param ... Ignored.
#' @description
#'
#' Summary method for CLV data objects that provides information about the estimation and
#' possible holdout sample, and descriptive statistics of the transaction data.
#'
#' @return This function computes and returns summary statistics of the
#' transactional and covariates data given in \code{object}. This is a list of
#' class \code{summary.clv.data} and contains the elements:
#' \item{name}{Human readable description of the type of data.}
#' \item{summary.clv.tim}{Summary information about the stored \code{clv.time} object.}
#' \item{descriptives.transactions}{A \code{data.table} with summary statistics of
#' the transactions overall and in the estimation and holdout sample.}
#'
#' For static covariates data, the list additionally is of class \code{summary.clv.data.static.covariates}
#' and further contains the elements:
#'
#' \item{names.cov.data.trans}{Names of the covariates for the Transaction process.}
#' \item{names.cov.data.life}{Names of the covariates for the Lifetime process.}
#'
#' @seealso \code{\link[CLVTools:plot.clv.data]{plot}} for how to plot a clv data object
#' @seealso \code{\link[CLVTools:clvdata]{clvdata}} for how to create a clv data object
#' @seealso \code{\link[CLVTools:SetStaticCovariates]{SetStaticCovariates}} for how to add static covariates
#' @seealso \code{\link[CLVTools:SetDynamicCovariates]{SetDynamicCovariates}} for how to add dynamic covariates
#'
#' @examples
#'
#' data("apparelTrans")
#' clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd",
#'                             time.unit = "w",
#'                             estimation.split = 37)
#'
#' # summary of transaction data and split
#' summary(clv.data.apparel)
#'
#' # add contextual factors
#' data("apparelDemographics")
#' clv.data.apparel.cov <-
#'  SetStaticCovariates(clv.data.apparel,
#'                      data.cov.life = apparelDemographics,
#'                      names.cov.life = "Gender",
#'                      data.cov.trans = apparelDemographics,
#'                      names.cov.trans = "Gender")
#'
#' # additional info about the covariates
#' summary(clv.data.apparel.cov)
#'
#'
