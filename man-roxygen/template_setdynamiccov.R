#' @name SetDynamicCovariates
#' @title Add Dynamic Covariates to a CLV data object
#'
#' @param clv.data CLV data object to add the covariates data to.
#' @param data.cov.life Dynamic covariate data as \code{data.frame} or \code{data.table} for the lifetime process.
#' @param data.cov.trans Dynamic covariate data as \code{data.frame} or \code{data.table} for the transaction process.
#' @param names.cov.life Vector with names of the columns in \code{data.cov.life} that contain the covariates.
#' @param names.cov.trans Vector with names of the columns in \code{data.cov.trans} that contain the covariates.
#' @param name.id Name of the column to find the Id data for both, \code{data.cov.life} and \code{data.cov.trans}.
#' @param name.date Name of the column to find the Date data for both, \code{data.cov.life} and \code{data.cov.trans}.
#'
#' @description
#' Add dynamic covariate data to an existing data object of class \code{clv.data}.
#' The returned object can be used to fit models with dynamic covariates.
#'
#' No covariate data can be added to a clv data object which already has any covariate set.
#'
#' At least 1 covariate is needed for both processes and no categorical covariate may be of only a single category.
#'
#'
#' @details
#'
#' \code{data.cov.life} and \code{data.cov.trans} are \code{data.frame}s or \code{data.table}s that
#' each contain exactly 1 row for every combination of timepoint and customer.
#' For each customer appearing in the transaction data
#' there needs to be covariate data at every timepoint that marks the start of a period as defined
#' by \code{time.unit}. It has to range from the start of the estimation sample (\code{timepoint.estimation.start})
#' until the end of the period in which the end of the holdout sample (\code{timepoint.holdout.end}) falls.
#' See the the provided data \code{apparelDynCov} for illustration.
#' Covariates of class \code{character} or \code{factor} are converted to k-1 numeric dummies.
#'
#' \code{Date as character}
#' If the Date column in the covariate data is of type \code{character}, the \code{date.format} given when
#' creating the the \code{clv.data} object is used for parsing.
#'
#'
#' @return
#' An object of class \code{clv.data.dynamic.covariates}.
#' See the class definition \linkS4class{clv.data.dynamic.covariates}
#' for more details about the returned object.
#'
#'
#' @examples
#' \dontrun{
#' data("apparelTrans")
#' data("apparelDynCov")
#'
#' # Create a clv data object without covariates
#' clv.data.apparel <- clvdata(apparelTrans, time.unit="w",
#'                             date.format="ymd")
#'
#' # Add dynamic covariate data
#' clv.data.dyn.cov  <-
#'    SetDynamicCovariates(clv.data.apparel,
#'                        data.cov.life  = apparelDynCov,
#'                        names.cov.life = c("High.Season", "Gender", "Channel"),
#'                        data.cov.trans = apparelDynCov,
#'                        names.cov.trans = c("High.Season", "Gender", "Channel"),
#'                        name.id = "Id",
#'                        name.date = "Cov.Date")
#'
#' # summary output about covariates data
#' summary(clv.data.dyn.cov)
#'
#' # fit pnbd model with dynamic covariates
#' pnbd(clv.data.dyn.cov)
#' }
#'
