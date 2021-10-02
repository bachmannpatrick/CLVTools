#' @title Summarizing a CLV data object
#'
#' @param object A CLV data object containing transactional data and potentially also contextual factors.
#' @param Id A character vector of customer ids for which the transaction data is summarized. Defaults to
#' \code{NULL} for all customers.
#'
#' @description
#'
#' Summary method for objects of class \code{clv.data} that provides information about the estimation and
#' possible holdout sample, and descriptive statistics of the transaction data.
#'
#'
#' @return This function computes and returns summary statistics of the
#' transactional and covariates data given in \code{object}. This is a list of
#' class \code{summary.clv.data} and contains the elements:
#' \item{name}{Human readable description of the type of data.}
#' \item{summary.clv.tim}{Summary information about the stored \code{clv.time} object.}
#' \item{descriptives.transactions}{A \code{data.table} with summary statistics of
#' the transactions overall and in the estimation and holdout sample.}
#' \item{selected.ids}{Ids for which the transaction data was summarized. \code{NULL} if for all.}
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
#' \donttest{
#'
#' data("apparelTrans")
#' clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd",
#'                             time.unit = "w",
#'                             estimation.split = 40)
#'
#' # summary of transaction data and split
#' summary(clv.data.apparel)
#'
#' # transaction summary of customer "1219"
#' summary(clv.data.apparel, Id="1219")
#'
#' # transaction summary of customers "1", "10", "100", and "1000"
#' summary(clv.data.apparel, Id=c("1", "10", "100", "1000"))
#'
#' # add contextual factors
#' data("apparelStaticCov")
#' clv.data.apparel.cov <-
#'  SetStaticCovariates(clv.data.apparel,
#'                      data.cov.life = apparelStaticCov,
#'                      names.cov.life = "Gender",
#'                      data.cov.trans = apparelStaticCov,
#'                      names.cov.trans = "Gender")
#'
#' # additional info about the covariates
#' summary(clv.data.apparel.cov)
#' }
#'
