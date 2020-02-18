#' @title CDnow dataset
#' @description
#' A dataset containing the entire purchase history up to the end of June 1998
#' of the cohort of 23,570 individuals who made their first-ever purchase at CDNOW in the first quarter
#' of 1997.
#'
#' @format A \code{data.table} with 6696 rows and 4 variables:
#' \describe{
#'   \item{\code{Id}}{Customer Id}
#'   \item{\code{Date}}{Date of purchase}
#'   \item{\code{CDs}}{Amount of CDs purchased}
#'   \item{\code{Price}}{Price of purchase}
#' }
#' @name cdnow
#' @usage data("cdnow")
#' @docType data
#' @keywords datasets
#' @references
#' Fader, Peter S. and Bruce G.,S. Hardie, (2001), "Forecasting Repeat Sales at CDNOW:
#' A Case Study," Interfaces, 31 (May-June), Part 2 of 2, p94-107.
"cdnow"


#' @name apparelTrans
#' @title Apparel Retailer Dataset
#'
#' @description
#' A dataset containing the entire purchase history of customers made their first purchase at an
#' apparel retailer during January to May 2010. In total the dataset contains 2201 customers who made
#' 19573 transactions between January 2010 and October 2016.
#'
#' @format A \code{data.table} with 19573 rows and 3 variables:
#' \describe{
#'   \item{\code{Id}}{Customer Id}
#'   \item{\code{Date}}{Date of purchase}
#'   \item{\code{Price}}{Price of purchase}
#' }
#'
#' @usage data("apparelTrans")
#' @docType data
#' @keywords datasets
"apparelTrans"


#' @name apparelDemographics
#' @title Customer Demographics for the Apparel Retailer Dataset
#' @description
#' This data contains additional demographic information on the customers in the
#' "apparelTrans" dataset. This information can be used as time-invariant covariates.
#'
#' @format A \code{data.table} with 2201 rows and 2 variables:
#'
#' \describe{
#'   \item{Id}{Customer Id}
#'   \item{Gender}{0=male, 1=female}
#' }
#'
#' @docType data
#' @keywords datasets
#' @usage data("apparelDemographics")
"apparelDemographics"

#' @name apparelDynCov
#' @title Dynamic Covariates for the Apparel Retailer Dataset
#' @description
#' This data contains direct marketing and seasonality information on customers in the "apparelTrans" dataset.
#' This information can be used as time-varying covariates.
#'
#'
#' @format A data.table with 781355 rows and 5 variables
#' \describe{
#'   \item{Id}{Customer Id}
#'   \item{Cov.Date}{Date of contextual factor}
#'   \item{DM}{1 if customer was contacted with direct marketing in this time period}
#'   \item{High-Season}{1 if high season during this time period}
#'   \item{Gender}{0=male, 1=female}
#' }
#'
#' @keywords datasets
#' @usage data("apparelDynCov")
#' @docType data
"apparelDynCov"
