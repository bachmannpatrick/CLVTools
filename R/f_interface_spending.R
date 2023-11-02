#' Formula Interface for Spending Models
#'
#' @description
#' Fit models for customer spending (currently only the Gamma-Gamma model).
#'
#' @template template_param_optimxargs
#' @template template_param_verbose
#' @param data A \code{clv.data} object
#' @param family A spending model (currently only \code{gg})
#'
#' @seealso Spending models for \code{family}: \link{gg}.
#' @seealso \link{latentAttrition} to fit latent attrition models with a
#' formula interface
#'
#' @return Returns an object of the respective model which was fit.
#'
#' @examples
#' \donttest{
#'
#' data("cdnow")
#' clv.cdnow <- clvdata(data.transactions = cdnow, date.format="ymd",
#'                      time.unit = "weeks")
#'
#' # Fit gg
#' spending(family=gg, data=clv.cdnow)
#'
#' # Fit gg with start params
#' spending(family=gg, data=clv.cdnow,
#'          start.params.model=c(p=0.5, q=15, gamma=2))
#'
#' # Fit gg, do not remove first transaction
#' spending(family=gg, data=clv.cdnow, remove.first.transaction=FALSE)
#'
#'
#' ## No formula may be given to specify covariates because currently
#' ## no spending model uses covariates
#'
#' }
#'
#'
#' @export
spending <- function(family, data, optimx.args=list(), verbose=TRUE, ...){

  cl  <- match.call(call = sys.call(), expand.dots = TRUE)


  check_err_msg(check_userinput_spending_family(family))
  check_err_msg(check_userinput_data(data))
  check_err_msg(check_userinput_spending_dots_family(family=family, ...))
  # TODO: Check dots to only contain remove.first.transaction

  # No formula which could mandate transforming covariates


  # Fit model ---------------------------------------------------------------------------------------------------
  # call args
  #   - from explicitly passed args
  #   - args in dots which includes all additional options such as regularization and constraint covs
  args <- list(clv.data = data, verbose=verbose, optimx.args=optimx.args, ...)

  # Fit model
  obj <- do.call(what = family, args = args)

  # Replace call with call to latentAttrition()
  obj@call <- cl

  return(obj)
}


check_userinput_spending_family <- function(family){
  # not missing
  if(missing(family))
    return("Please provide one of the following inputs for parameter \'family\': gg")

  # has to be exactly one of the methods exported from the package (pnbd, bgnbd, ggomnbd)
  if(!identical(family, gg)){
    return("Please provide one of the following inputs for parameter \'family\': gg")
  }
  return(c())
}

check_userinput_spending_dots_family <- function(family, ...){
  # See explanations in check_userinput_latentattrition_dots_family_data()

  allowed.args <- formalArgs(getMethod(family, signature(clv.data = "clv.data")))
  allowed.args <- setdiff(allowed.args, c("clv.data", "optimx.args", "verbose", "..."))

  names.dots <- names(match.call(expand.dots = TRUE))[c(-1L, -2L)] # remove call, family
  not.allowd.args <- setdiff(names.dots, allowed.args)

  if(length(not.allowd.args)){
    return(paste0("The following arguments may not be passed to '...': ", paste0(not.allowd.args, collapse=', ')))
  }

  return(c())
}
