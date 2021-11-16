#' @importFrom Formula as.Formula
#' @importFrom stats terms formula
#' @export
latentAttrition <- function(formula, data, optimx.args=list(), verbose=TRUE){
# pnbd: use.cor, start.params.cor

  cl  <- match.call(call = sys.call(), expand.dots = TRUE)

  check_err_msg(.check_userinput_formula(formula))

  F.formula <- as.Formula(formula)
  model <- formula_read_model_name(F.formula)
  l.model.args <- formula_parse_args_of_special(F.formula = F.formula, name.special = model, from.rhs = 1)

  # make into clv.data if is not
  # if(!is(data, "clv.data")){
  #   data <- clvdata(data.transactions = data, date.format=, time.unit=,
  #                      estimation.split=, name.id=, name.date=, name.price=)
  # }

  # default args from explicitly passed args
  args <- list(clv.data = data, verbose=verbose, optimx.args=optimx.args)

  # args passed to special functions
  args <- modifyList(args, l.model.args, keep.null = TRUE)

  obj <- do.call(what = model, args)
  obj@call <- cl
  return(obj)
}


#' @importFrom Formula as.Formula is.Formula
#' @importFrom stats terms
#' @importFrom methods is
.check_userinput_formula <- function(formula){
  name.specials.model <- c("pnbd", "bgnbd", "ggomnbd")
  err.msg <- c()

  if(missing(formula))
    return("Please provide a valid formula object as \'formula\' parameter.")
  if(is.null(formula))
    return("Please provide a valid formula object as \'formula\' parameter.")
  # Check if it is a formula
  if(!is(object = formula, class2 = "formula") && !is.Formula(formula))
    return("Please provide a valid formula object as \'formula\' parameter.")

  F.formula <- as.Formula(formula)

  # Check that formula has 1 LHS
  if(length(F.formula)[1] != 0)
    err.msg <- c(err.msg, "Please specify no dependent variable.")

  # Formula too badly structured, unable to do further tests
  if(length(err.msg)>0)
    return(err.msg)

  # Check that has exactly one model special and ..
  F.terms.rhs1 <- terms(F.formula, lhs=0, rhs=1, specials=name.specials.model)
  num.model.specials <- sum(sapply(attr(F.terms.rhs1, "specials"), length))

  # ... no other special function than model in RHS1
  if(num.model.specials !=1 || length(labels(F.terms.rhs1)) != 1)
    err.msg <- c(err.msg, "Please choose exactly one of the following models as the first RHS: pnbd(), bgnbd(), ggomnbd().")

  if(length(err.msg)>0)
    return(err.msg)

  # Verify that can parse all args in model special
  model <- formula_read_model_name(F.formula)
  l.model.args <- formula_parse_args_of_special(F.formula = F.formula, name.special=model, from.rhs=1)
  if(any(sapply(l.model.args, is, "error")))
    err.msg <- c(err.msg, paste0("Please provide only arguments to ",model,"() which can be parsed!"))

  return(err.msg)
}


# only works if special exists once
formula_parse_args_of_special <- function(F.formula, name.special, from.rhs){
  # read out args of "model" special. First element because has special only once
  l.args <- formula_readout_special_arguments(F.formula = F.formula, name.special=name.special,
                                    from.rhs=from.rhs, params.as.chars.only=FALSE)[[1]]

  # Turn args of special function which are chars into objects
  #   error obj if not parse-able
  return(lapply(l.args, function(arg){
    if(is.null(arg)){
      return(arg)
    }else{
      return(tryCatch(eval(parse(text=arg)),
                      error=function(e)e))
    }
  }))

}

#' @importFrom stats terms formula
formula_read_model_name <- function(F.formula){
  F.terms.rhs1 <- terms(formula(F.formula, lhs=0, rhs=1), specials=c("pnbd", "bgnbd", "ggomnbd"))
  return(names(unlist(attr(F.terms.rhs1, "specials"))))
}

# params.as.chars.only=TRUE returns a vector of chars with the parameter names in it.
#                           Used to readout given param name
#                           Example:  continuous(X1, X2)
#                                   => c("X1", "X2")
# params.as.chars.only=FALSE returns a list of list. The inner list is named after the arg
#                            and the element contains the actual input as char. Elements in sub-lists have
#                            to be named so that they do not get matched to unintended args because of their
#                            position (ie X2 to g)
#                            Example:  IIV(g=x2, iiv=gp, X1, X2) + IIV()
#                                   => list(list(g="x2",iiv="gp", X1="X1",X2="X2"), list(..),..)
#' @importFrom stats terms formula
formula_readout_special_arguments <- function(F.formula, name.special, from.rhs, params.as.chars.only = TRUE){

  F.terms   <- terms(formula(F.formula, lhs=0, rhs=from.rhs), specials = name.special)

  # Read out positions in formula
  # NULL if there are no specials
  ind.special <- attr(F.terms, "specials")[[name.special]]

  # End if there are no such special in the formula
  if(is.null(ind.special))
    return(NULL)

  # Read out commands
  # +1 because first is "list"
  lang.special <- attr(F.terms, "variables")[1L+ind.special]

  # Obtain column name is indicated in the function
  #   create function that is exectuted and simply returns the specified inputs as strings

  # create new function named after special which simply returns the input given
  assign(x = paste0("fct.", name.special), value = function(...){ return(match.call()[-1])})

  # Execute this function for each special by adding "fct." at the beginning and evaluating
  names.special  <- lapply(paste0("fct.",lang.special), FUN=function(str){
    eval(expr=parse(text=str))})

  if(params.as.chars.only){
    # Return vector with (unique) parameter names only
    names.special        <- lapply(names.special, function(call){as.character(as.list(call))})
    names.special        <- unique(unlist(names.special))
    names(names.special) <- names.special
  }else{
    # return list with names=args, entry = arg data
    # here:list of language object
    names.special <- lapply(names.special, as.list)
    # here: nested list of arg-lists
    names.special <- lapply(names.special, lapply, deparse) # make char representation
    # replace the "", "NULL" as actuall NULLs
    names.special <- lapply(names.special, lapply, function(char.arg){if(char.arg%in%c("NULL", "", "NA")) NULL else char.arg})
    # Name unnamed elements (=args) in sublists after the elements
    names.special <- lapply(names.special, function(subl){
      unnamed.ind <- which(names(subl)=="")
      names(subl)[unnamed.ind] <- subl[unnamed.ind]
      return(subl)})
    # Does not need be unique per list, will not mixup entries if regressors are named same as g/iiv
    #   rather it is caught that they may not be twice
  }

  return(names.special)
}
