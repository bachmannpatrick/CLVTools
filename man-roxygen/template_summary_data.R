#' @title Summarizing a CLV data object
#'
#' @param object A CLV data object containing transactional data and potentially also contextual factors.
#' @param ids A character vector of customer ids for which the transaction data is summarized. Defaults to
#' \code{NULL} for all customers.
#'
#' @description
#'
#' Summary method for objects of class \code{clv.data} that provides information about the estimation and
#' possible holdout sample, and descriptive statistics of the transaction data.
#'
#'
#' @details
#' If applicable, the summary statistics are provided separately for the estimation and holdout period as well as
#' for the overall time period (estimation + holdout). By using the \code{ids} argument, the summary statistics can
#' be limited to a subset of customers.
#' \describe{
#' \item{\code{Number of customers}}{Count of individual customers.}
#' \item{\code{First Transaction in period}}{Time point of the first transaction occurring in the indicated period.}
#' \item{\code{Last Transaction in period}}{Time point of the last transaction occurring in the indicated period.}
#' \item{\code{Total # Transactions}}{Count of transactions occurring in the indicated period.}
#' \item{\code{Mean # Transactions per cust}}{Average transactions per customer in the indicated period,
#' including standard deviation (SD).}
#' \item{\code{Mean Spending per Transaction}}{Average spending per customer in the indicated period, including
#' standard deviation (SD). Spending statistics are only available if spending data was provided when creating the object.}
#' \item{\code{Total Spending}}{Sum of customer spending during the indicated period.}
#' \item{\code{Total # zero repeaters}}{Number of customers who purchased only once during the estimation period.}
#' \item{\code{Percentage of zero repeaters}}{Percentage of customers who purchased only once during the estimation period.}
#' \item{\code{Mean Interpurchase time}}{Average time (in number of periods) between two transactions of the same customer,
#' including standard deviation (SD).}
#' }
#'
#' @return This function computes and returns summary statistics of the
#' transactional and covariates data given in \code{object}. This is a list of
#' class \code{summary.clv.data} and contains the elements:
#' \item{name}{Human readable description of the type of data.}
#' \item{summary.clv.time}{Summary information about the stored \code{clv.time} object.}
#' \item{descriptives.transactions}{A \code{data.table} with summary statistics of
#' the transactions overall and in the estimation and holdout sample.}
#' \item{selected.ids}{Ids for which the transaction data was summarized. \code{NULL} if all Ids were used.}
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
#' summary(clv.data.apparel, ids="1219")
#'
#' # transaction summary of customers "1", "10", "100", and "1000"
#' summary(clv.data.apparel, ids=c("1", "10", "100", "1000"))
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
