#' Formula Interface for Spending Models
#'
#' @importFrom Formula as.Formula
#' @importFrom stats terms formula
#' @export
spending <- function(formula, data, optimx.args=list(), verbose=TRUE){

  cl  <- match.call(call = sys.call(), expand.dots = TRUE)

  check_err_msg(check_userinput_formula(formula, name.specials.model = c("gg")))
  check_err_msg(check_userinput_formula_clvdata(data))
  check_err_msg(check_userinput_spending_formulavsdata(formula=formula, data=data))

  F.formula <- as.Formula(formula)

  # default args from explicitly passed args
  args <- list(clv.data = data, verbose=verbose, optimx.args=optimx.args)

  # add model call args
  model <- formula_read_model_name(F.formula)
  l.model.args <- formula_parse_args_of_special(F.formula = F.formula, name.special = model, from.rhs = 1)
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

  return(err.msg)
}



