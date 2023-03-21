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
#' @importFrom Formula as.Formula
#' @importFrom stats terms formula
#' @export
spending <- function(formula, data, optimx.args=list(), verbose=TRUE){

  cl  <- match.call(call = sys.call(), expand.dots = TRUE)

  check_err_msg(check_userinput_formula(formula, name.specials.model = "gg"))
  check_err_msg(check_userinput_formula_data(data))
  check_err_msg(check_userinput_spending_formulavsdata(formula=formula, data=data))

  F.formula <- as.Formula(formula)

  # Turn data.frame/table into data if needed
  if(is.data.frame(data) || is.data.table(data)){
    data <- formulainterface_dataframe_toclvdata(F.formula = F.formula, data=data, cl=cl)
  }

  # No covariates to be transformed


  # default args from explicitly passed args
  args <- list(clv.data = data, verbose=verbose, optimx.args=optimx.args)

  # add model call args
  model <- formula_read_model_name(F.formula)
  l.model.args <- formula_parse_args_of_special(F.formula = F.formula, name.special = model, from.lhs=0, from.rhs = 1)
  args <- modifyList(args, l.model.args, keep.null = TRUE)

  # Fit model
  obj <- do.call(what = model, args)

  # Replace call with call to spending()
  obj@call <- cl

  return(obj)
}


#' @importFrom Formula as.Formula
check_userinput_spending_formulavsdata <- function(formula, data){
  # only verify formula, not data

  err.msg <- c()

  # formula is verified to be basic correct
  F.formula <- as.Formula(formula)

  # always only 1 RHS
  if(length(F.formula)[2] != 1){
    err.msg <- c(err.msg, "The formula may only contain 1 RHS part specifiying the model!")
  }

  if(is.data.frame(data) || is.data.table(data)){
    err.msg <- check_userinput_latentattrition_formulavsdata_LHS1(formula)
  }else{
    # many not have any LHS if not raw data
    if(length(F.formula)[1] != 0){
      err.msg <- c(err.msg, "Please do not specify any LHS if a clv.data object is given for parameter data!")
    }
  }

  return(err.msg)
}



