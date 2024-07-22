#' @title CDNOW dataset
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
#'
#' @usage data("cdnow")
#' @docType data
#' @keywords datasets
#' @references
#' Fader, Peter S. and Bruce G.,S. Hardie, (2001), "Forecasting Repeat Sales at CDNOW:
#' A Case Study," Interfaces, 31 (May-June), Part 2 of 2, p94-107.
"cdnow"



#' @title Apparel Retailer Dataset
#'
#' @description
#' This is a simulated dataset containing the entire purchase history of customers made their first purchase at an
#' apparel retailer on January 2nd 2005. In total the dataset contains 600 customers who made
#' 2,647 transactions between January 2005 and end December 2010
#'
#' @format A \code{data.table} with 2,647 rows and 3 variables:
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



#' @name apparelStaticCov
#' @title Time-invariant Covariates for the Apparel Retailer Dataset

#' @description
#' This simulated data contains additional demographic information on all 600 customers in the
#' "apparelTrans" dataset. This information can be used as time-invariant covariates.
#'
#' @format A \code{data.table} with 600 rows and 3 variables:
#'
#' \describe{
#'   \item{Id}{Customer Id}
#'   \item{Gender}{0=male, 1=female}
#'   \item{Channel}{Acquisition channel: 0=online, 1=offline}
#' }
#'
#' @docType data
#' @keywords datasets
#' @usage data("apparelStaticCov")
"apparelStaticCov"


#' @name apparelDynCov
#' @title Time-varying Covariates for the Apparel Retailer Dataset

#' @description
#' This simulated data contains seasonal information and additional covariates on all 400 customers in the "apparelTrans" dataset.
#' This information can be used as time-varying covariates.
#'
#' @format A data.table with 187,800 rows and 5 variables
#' \describe{
#'   \item{Id}{Customer Id}
#'   \item{Cov.Date}{Date of contextual factor}
#'   \item{High.Season}{Seasonal variable: 1 indicating a time-period that is considered "high season".}
#'   \item{Gender}{0=male, 1=female}
#'   \item{Channel}{Acquisition channel: 0=online, 1=offline}
#' }
#'
#' @keywords datasets
#' @usage data("apparelDynCov")
#' @docType data
"apparelDynCov"

#' @name apparelDynCovFuture
#' @title Future Time-varying Covariates for the Apparel Retailer Dataset

#' @description
#' This simulated data contains seasonal information and additional covariates on all 400 customers in the "apparelTrans" after the last transaction in the dataset.
#' This information can be used as time-varying covariates for prediction future customer behavior.
#'
#' @format A data.table with 56,400 rows and 5 variables
#' \describe{
#'   \item{Id}{Customer Id}
#'   \item{Cov.Date}{Date of contextual factor}
#'   \item{High.Season}{Seasonal variable: 1 indicating a time-period that is considered "high season".}
#'   \item{Gender}{0=male, 1=female}
#'   \item{Channel}{Acquisition channel: 0=online, 1=offline}
#' }
#'
#' @keywords datasets
#' @usage data("apparelDynCovFuture")
#' @docType data
"apparelDynCovFuture"
