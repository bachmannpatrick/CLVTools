#' Formula Interface for Spending Models
#'
#' @description
#' Fit latent Gamma-Gamma model for customer spending with a formula interface
#'
#' @template template_param_formulainterface_formula
#' @template template_param_formulainterface_data
#' @template template_param_optimxargs
#' @template template_param_verbose
#'
#' @seealso Spending models for inputs: \link{gg}.
#' @seealso \link{latentAttrition} to fit latent attrition models with a formula interface
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
#' spending(~gg(), data=clv.cdnow)
#'
#' # Fit gg with start params
#' spending(~gg(start.params.model=c(p=0.5, q=15, gamma=2)),
#'          data=clv.cdnow)
#'
#' # Fit gg, do not remove first transaction
#' spending(~gg(remove.first.transaction=FALSE), data=clv.cdnow)
#' # same, abreviate parameters
#' spending(~gg(remo=F), data=clv.cdnow)
#'
#' # Fit gg on given data.frame transaction data, no split
#' spending(data()~gg(), data=cdnow)
#'
#' # Fit gg on given data.frame, split after 39 periods
#' spending(data(split=39)~gg(), data=cdnow)
#' # same but also give date format and period definition
#' spending(data(split=39, format=ymd, unit=w)~gg(), data=cdnow)
#'
#' ## No covariate may be selected or covariate data.frame may be
#' ## given because currently no spending model uses covariates
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
