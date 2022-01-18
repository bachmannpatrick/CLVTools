#' Formula Interface for Spending Models
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



