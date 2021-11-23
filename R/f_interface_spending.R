#' @importFrom Formula as.Formula
#' @importFrom stats terms formula
#' @export
spending <- function(formula, data, optimx.args=list(), verbose=TRUE){
  cl  <- match.call(call = sys.call(), expand.dots = TRUE)

  check_err_msg(check_userinput_formula(formula, name.specials.model = c("gg")))
  check_err_msg(check_userinput_formula_clvdata/data(data))

  # check_err_msg(check_userinput_spending/latentattrition_formula_vs_data())

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
