#' Formula Interface for Latent Attrition Models
#' @template template_param_formulainterface_data
#' @template template_param_formulainterface_formula
#' @template template_param_optimxargs
#' @template template_param_verbose
#' @param cov Optional \code{data.frame} or \code{data.table} of covariate data for the lifetime and transaction process. See Details.
#'
#' @description
#' Fit latent attrition models for transaction with a formula interface
#'
#' @details
#' \subsection{Formula}{
#' A multi-part formula describing how to prepare data and fit the model.
#'
#' Formula left hand side (LHS) specifies the data preparation which depends on the provided argument \code{data}.
#' \itemize{
#' \item{If \code{data} is \code{clvdata}: }{Nothing, LHS is required to be empty.}
#' \item{If \code{data} is a \code{data.frame}: }{Data preparation using formula special \code{clvdata(time.unit, date.format, split)}. The formula is required to have a LHS.}
#' }
#'
#' Formula right hand side (RHS) specifies the model fitting and follows a multi-part notation.
#' \itemize{
#' \item{1st part (required): }{The model to fit. One of either \code{\link{pnbd}}, \code{\link{bgnbd}}, or \code{\link{ggomnbd}}. Depending on the model additional arguments may be given. See the respective model functions for details.}
#' }
#'
#' If the model is fit with covariates, further parts separated by \code{|} are required:
#' \itemize{
#' \item{2nd part (required): }{Which covariates to include for the lifetime process, potentially transforming them and adding interactions. The dot ('.') refers to all columns in the data except the identifier variables.}
#' \item{3rd part (required): }{Which covariates to include for the transaction process, potentially transforming them and adding interactions. The dot ('.') refers to all columns in the data except the identifier variables.}
#' \item{4th part (optional): }{Formula special \code{regularization(trans=, life=)} to specify the lambdas for regularization and \code{constraint(...)} to specify parameters to be equal on both processes.
#' Both specials separated by \code{+} may be given.}
#' }
#'
#' See the example section for illustrations on how to specify the formula parameter.
#' }
#'
#' \subsection{Covariate Data}{
#'
#' For time-invariant covariates the data contains exactly one single row of covariate data for every customer appearing in the transaction data.
#' Requires a column \code{Id} of customer identifiers.
#' See \code{\link[CLVTools:SetStaticCovariates]{SetStaticCovariates}} for details.
#'
#' For time-varying covariates the data contains exactly 1 row for every combination of timepoint and customer.
#' Requires a column \code{Id} of customer identifiers and a column \code{Cov.Date} of dates.
#' For each customer appearing in the transaction data there needs to be covariate data at every timepoint that marks the start of a period as defined
#' by time.unit. It has to range from the start of the estimation sample (timepoint.estimation.start) until the end of
#' the period in which the end of the holdout sample (timepoint.holdout.end) falls.
#' Covariates of class character or factor are converted to k-1 numeric dummies.
#' See \code{\link[CLVTools:SetDynamicCovariates]{SetDynamicCovariates}} and the the provided dataset \code{\link{apparelDynCov}} for illustration.
#' }
#'
#'
#' @seealso Models for inputs to: \link{pnbd}, \link{ggomnbd}, \link{bgnbd}.
#' @seealso \link{spending} to fit spending models with a formula interface
#'
#' @examples
#' \donttest{
#'
#' data("apparelTrans")
#' data("apparelStaticCov")
#'
#' clv.nocov <-
#'     clvdata(apparelTrans, time.unit="w", date.format="ymd")
#'
#' # Create static covariate data with 2 covariates
#' clv.staticcov  <-
#'   SetStaticCovariates(clv.nocov,
#'                       data.cov.life  = apparelStaticCov,
#'                       names.cov.life = c("Gender", "Channel"),
#'                       data.cov.trans = apparelStaticCov,
#'                       names.cov.trans = c("Gender", "Channel"))
#'
#' # Fit pnbd without covariates
#' latentAttrition(~pnbd(), data=clv.nocov)
#' # Fit bgnbd without covariates
#' latentAttrition(~bgnbd(), data=clv.nocov)
#' # Fit ggomnbd without covariates
#' latentAttrition(~ggomnbd(), data=clv.nocov)
#'
#' # Fit pnbd with start parameters and correlation
#' latentAttrition(~pnbd(start.params.model=c(r=1, alpha=10, s=2, beta=8),
#'                       use.cor=TRUE),
#'                 data=clv.nocov)
#'
#' # Fit pnbd with all present covariates
#' latentAttrition(~pnbd()|.|., clv.staticcov)
#'
#' # Fit pnbd with selected covariates
#' latentAttrition(~pnbd()|Gender|Channel+Gender, data=clv.staticcov)
#'
#' # Fit pnbd with start parameters for covariates
#' latentAttrition(~pnbd(start.params.life = c(Gender = 0.6, Channel = 0.4),
#'                       start.params.trans = c(Gender = 0.6, Channel = 0.4))|.|., data=clv.staticcov)
#'
#' # Fit pnbd with transformed covariate data
#' latentAttrition(~pnbd()|Gender|I(log(Channel+2)), data=clv.staticcov)
#'
#' # Fit pnbd with all covs and regularization
#' latentAttrition(~pnbd()|.|.|regularization(life=3, trans=8), clv.staticcov)
#'
#' # Fit pnbd with all covs and constraint parameters for Channel
#' latentAttrition(~pnbd()|.|.|constraint(Channel), clv.staticcov)
#'
#' # Fit pnbd on given data.frame, no split
#' latentAttrition(data()~pnbd(), data=apparelTrans)
#'
#' # Fit pnbd, split data after 39 periods
#' latentAttrition(data(split=39)~pnbd(), data=apparelTrans)
#' # Same but also give date format and period definition
#' latentAttrition(data(split=39, format=ymd, unit=w)~pnbd(), data=apparelTrans)
#'
#' # Fit pnbd on given data.frames w/ all covariates
#' latentAttrition(data()~pnbd()|.|., data=apparelTrans, cov=apparelStaticCov)
#'
#' # Fit pnbd on given data.frames w/ selected covariates
#' latentAttrition(data()~pnbd()|Channel+Gender|Gender,
#'                 data=apparelTrans, cov=apparelStaticCov)
#'
#' }
#'
#'
#' @importFrom Formula as.Formula
#' @importFrom stats terms formula
#' @export
latentAttrition <- function(formula, data, cov, optimx.args=list(), verbose=TRUE){

  cl  <- match.call(call = sys.call(), expand.dots = TRUE)

  check_err_msg(check_userinput_formula(formula, name.specials.model = c("pnbd", "bgnbd", "ggomnbd")))
  check_err_msg(check_userinput_formula_data(data))
  check_err_msg(check_userinput_formula_cov(cov=cov))
  check_err_msg(check_userinput_latentattrition_formulavsdata(formula=formula, data=data, cov=cov))

  F.formula <- as.Formula(formula)

  # Turn data.frame/table into data if needed
  if(is.data.frame(data) || is.data.table(data)){
    # Verified to be data.frame/table
    data <- formulainterface_dataframe_toclvdata(F.formula = F.formula, data=data, cl=cl)

    # Add covariate data if needed
    #   if indicated in formula RHS2&3 or cov data given
    if(!missing(cov)){
      data <-formulainterface_create_clvdataobj(F.formula = F.formula, clv.data.nocov = data,
                                                create.dyncov = ("Cov.Date" %in% colnames(cov)),
                                                dt.cov.life = cov, dt.cov.trans = cov)
    }
  }else{
    # data is clv.data object
    # if it has covariates, they need to be transformed
    if(is(data, "clv.data.static.covariates")){
      data <- formulainterface_create_clvdataobj(F.formula = F.formula, clv.data.nocov = as(data, "clv.data"),
                                                 create.dyncov = is(data, "clv.data.dynamic.covariates"),
                                                 dt.cov.life = data@data.cov.life, dt.cov.trans = data@data.cov.trans)
    }
  }



  # Fit model ---------------------------------------------------------------------------------------------------
  # default args from explicitly passed args
  args <- list(clv.data = data, verbose=verbose, optimx.args=optimx.args)

  # add model call args
  model <- formula_read_model_name(F.formula)
  l.model.args <- formula_parse_args_of_special(F.formula = F.formula, name.special = model, from.lhs = 0, from.rhs = 1)
  args <- modifyList(args, l.model.args, keep.null = TRUE)

  # args passed to model special functions
  #   if any given
  if(is(data, "clv.data.static.covariates") & length(F.formula)[2] == 4){
    l.args.reg <- formula_parse_args_of_special(F.formula = F.formula, name.special = "regularization", from.lhs=0, from.rhs = 4)
    if(length(l.args.reg)){
      args <- modifyList(args, list(reg.lambdas = c(life=l.args.reg[["life"]], trans=l.args.reg[["trans"]])), keep.null = TRUE)
    }

    # read char vec of variables to constrain
    #   do not need to concat multiple separate constraint() if params.as.chars.only=TRUE
    names.constr <- formula_readout_special_arguments(F.formula = F.formula, name.special = "constraint", from.lhs = 0, from.rhs = 4,
                                                      params.as.chars.only = TRUE)
    if(length(names.constr)){
      # To have names match covariate data names if there are any transformations applied or special symbols in the cov name
      # spaces in operations are handled by
      # Will make empty names.constr (NULL) to character(0) which is illegal input. Therefore have to wrap in if(length)
      names.constr <- make.names(names.constr)
      args <- modifyList(args, list(names.cov.constr=unname(names.constr)), keep.null = TRUE)
    }

  }

  # Fit model
  obj <- do.call(what = model, args)

  # Replace call with call to latentAttrition()
  obj@call <- cl

  return(obj)
}


# Create clv.data object if needed
# Apply needed transformations to given data
formulainterface_dataframe_toclvdata <- function(F.formula, data, cl){
  # Verified to be data.frame/table

  # std args to create data object with
  l.data.args <- list(data.transactions = data, date.format="ymd", time.unit="w",
                      name.id ="Id", name.date="Date", name.price="Price")

  # Overwrite with what is given in data() special
  l.data.special.args <- formula_parse_args_of_data(F.formula)
  # rename names in formula specials to data parameters
  names(l.data.special.args) <- sapply(names(l.data.special.args), function(x){
    switch(EXPR = x, "unit"="time.unit", "split"="estimation.split", "format"="date.format")})

  l.data.args <- modifyList(l.data.args, l.data.special.args, keep.null = TRUE)

  data <- do.call(what = clvdata, args = l.data.args)
  data@call <- cl

  return(data)
}

#' @importFrom stats update
formulainterface_create_clvdataobj <- function(F.formula, create.dyncov, clv.data.nocov, dt.cov.life, dt.cov.trans){

  if(create.dyncov){
    cov.id.vars <- c("Id", "Cov.Date")
  }else{
    cov.id.vars <- "Id"
  }

  # Have to use model.matrix() in order to build interactions from given data
  # model.frame() would otherwise be more desirable as it creates the relevant cols (incl transformations) but without dummifying
  # model.matrix() also creates the intercept and expands the dot to include Id and Cov.Date which are dummified. Therefore need to remove ids vars and intercept from formula by subtracting with '-Id-1'
  # update.formula() requires expanding the dot '.' with the names in the given data.
  # use terms() so subset Formula to relevant rhs and expand dot.

  # Considered alternatives:
  #   - reformulate(terms(F, data), intercept=F) but remove Id, Cov.Date lables from returned terms object. Good option but manipulating formula seems more natural.
  #   - use terms() but remove columns Id and Cov.Date from data to not expand dot to include these. May incur substantial overhead if data is large.

  # f.remove: formula to remove intercept and covariate ids
  f.remove <- eval(parse(text=paste0('~ . - 1 - ', paste0(cov.id.vars, collapse = '-'))))
  f.formula.life  <- update(terms(F.formula, lhs=0, rhs=2, data=dt.cov.life),  f.remove)
  f.formula.trans <- update(terms(F.formula, lhs=0, rhs=3, data=dt.cov.trans), f.remove)

  # Apply formula on cov data
  mm.cov.life  <- as.data.table(model.matrix(f.formula.life,  data=dt.cov.life ))
  mm.cov.trans <- as.data.table(model.matrix(f.formula.trans, data=dt.cov.trans))

  # Add Id vars to data
  mm.cov.life  <- cbind(mm.cov.life,  dt.cov.life[,  .SD, .SDcols=cov.id.vars])
  mm.cov.trans <- cbind(mm.cov.trans, dt.cov.trans[, .SD, .SDcols=cov.id.vars])

  # Create new cov data object
  #   from given clvdata object, is copy-ed in Set*Cov()
  if(create.dyncov){
    data <- SetDynamicCovariates(clv.data = clv.data.nocov,
                                 data.cov.life = mm.cov.life, names.cov.life = setdiff(colnames(mm.cov.life), cov.id.vars),
                                 data.cov.trans = mm.cov.trans, names.cov.trans = setdiff(colnames(mm.cov.trans), cov.id.vars),
                                 name.id = "Id", name.date = "Cov.Date")
  }else{
    data <- SetStaticCovariates(clv.data = clv.data.nocov,
                                data.cov.life  = mm.cov.life,  names.cov.life = setdiff(colnames(mm.cov.life), cov.id.vars),
                                data.cov.trans = mm.cov.trans, names.cov.trans = setdiff(colnames(mm.cov.trans), cov.id.vars),
                                name.id = "Id")
  }
  return(data)
}


#' @importFrom Formula as.Formula is.Formula
#' @importFrom stats terms formula
#' @importFrom methods is
check_userinput_formula <- function(formula, name.specials.model){
  err.msg <- c()

  if(missing(formula))
    return("Please provide a valid formula object as \'formula\' parameter.")
  if(is.null(formula))
    return("Please provide a valid formula object as \'formula\' parameter.")
  # Check if it is a formula
  if(!is(object = formula, class2 = "formula") && !is.Formula(formula))
    return("Please provide a valid formula object as \'formula\' parameter.")

  F.formula <- as.Formula(formula)



  # Verify LHS ------------------------------------------------------------------------------------------
  # Check that formula has maximum 1 LHS
  if(length(F.formula)[1] > 1)
    return("Please specify maximum 1 dependent variable (data()).")

  # Only verify if there is LHS 1
  if(length(F.formula)[1] == 1){

    # if(length(all.vars(formula(F.formula, lhs=1, rhs=0))) > 0)
    #   err.msg <- c(err.msg, "Please specify nothing else but data() in the LHS.")

    # Check that has exactly one special and nothing else in LHS
    num.lhs1.specials <- formula_num_specials(F.formula, lhs=1, rhs=0, specials = "data")
    # should exactly be list(data(...))
    num.variables.content <- length(attr(terms(F.formula, lhs=1, rhs=0, specials="data"), "variables"))
    if(num.lhs1.specials != 1 || num.variables.content != 2)
      return("Please specify exactly data() in the LHS.")


    # Verify can parse inputs to special
    l.data.args <- formula_parse_args_of_data(F.formula)
    if(any(sapply(l.data.args, is, "error")))
      err.msg <- c(err.msg, paste0("Please provide only arguments to data() which can be parsed!"))

    # **TODO: check that also fails data()+1 ~ pnbd() and pnbd()+1

    # ... no other special function than model in LHS1
    # ***TODO: There is a bug in Formula: labels() is inconsistent in recognizing specials in the LHS
    #
    # Does not recognize abc() special in LHS1:
    #   # only special in lhs1
    #   labels(terms(as.Formula(abc()~n), lhs=1, rhs=0, specials="abc"))
    #   # also var in rhs1
    #   labels(terms(as.Formula(abc()~n), lhs=1, rhs=1, specials="abc"))
    #   # also special in rhs1
    #   labels(terms(as.Formula(abc()~xyz()), lhs=1, rhs=1, specials=c("abc", "xyz")))
    #   # also special and var in rhs1
    #   labels(terms(as.Formula(abc()~n+xyz()), lhs=1, rhs=1, specials=c("abc", "xyz")))
    #
    # Recognizes abc() in LHS1
    #   # when adding another variable in LHS1 besides special
    #   labels(terms(as.Formula(abc()+k~n), lhs=1, rhs=0, specials="abc"))
    #   # when adding another special in LHS1
    #   labels(terms(as.Formula(abc()+xyz()~n), lhs=1, rhs=0, specials=c("abc", "xyz")))
    #
    # Seems related that there is only a single special in LHS1. A single special in RHS1 is however always recognized
    # labels(terms(as.Formula(~abc()), lhs=1, rhs=1, specials="abc"))

  }

  # Verify RHS ------------------------------------------------------------------------------------------
  if("." %in% all.vars(formula(F.formula, rhs=1)))
    return(paste0("Please choose exactly one of the following models as the first RHS: ",
                  paste0(paste0(name.specials.model, "()"), collapse = ", "),"."))

  # Check that has exactly one model special and ..
  num.model.specials <- formula_num_specials(F.formula, rhs=1, lhs=0, specials=name.specials.model)
  F.terms.rhs1 <- terms(F.formula, lhs=0, rhs=1, specials=name.specials.model)

  # ... no other special function than model in RHS1
  if(num.model.specials !=1 || length(labels(F.terms.rhs1)) != 1)
    err.msg <- c(err.msg, paste0("Please choose exactly one of the following models as the first RHS: ",
                                 paste0(paste0(name.specials.model, "()"), collapse = ", "),"."))

  # Formula too badly structured, unable to do further tests
  if(length(err.msg)>0)
    return(err.msg)

  # Verify that can parse all args in model special ...
  model <- formula_read_model_name(F.formula)
  l.model.args <- formula_parse_args_of_special(F.formula = F.formula, name.special=model, from.lhs=0, from.rhs=1)
  if(any(sapply(l.model.args, is, "error")))
    err.msg <- c(err.msg, paste0("Please provide only arguments to ",model,"() which can be parsed!"))

  # ... and none of the explicit args
  if(any(c("verbose","optimx.args") %in% names(l.model.args)))
    err.msg <- c(err.msg, paste0("Please do not specify arguments 'verbose' and 'optimx.args' in ",model,"()!"))

  return(err.msg)
}

check_userinput_formula_data <- function(data){
  if(missing(data))
    return("Please provide an object of class clv.data, data.frame, or data.table for parameter 'data'!")

  if(!is(data, "clv.data") && !is.data.frame(data) && !is.data.table(data)){
    return("Please provide an object of class clv.data, data.frame, or data.table for parameter 'data'!")
  }else{
    return(c())
  }
}

check_userinput_formula_cov <- function(cov){
  if(missing(cov)){
    return(c())
  }

  if(!is.data.frame(cov) && !is.data.table(cov)){
    return("Please provide an object of class data.frame or data.table for parameter 'cov'!")
  }

  # if(!("Id"%in%colnames(cov))){
  #   return("Please provide covariate data with a column named 'Id'.")
  # }
}

check_userinput_latentattrition_formulavsdata_LHS1 <- function(F.formula){
  err.msg <- c()

  # always requires LHS1
  if(length(F.formula)[1] == 0){
    return("Please specify a LHS with data() in the formula when supplying a data.frame/table for parameter data.")
  }

  # requires to have data special in LHS1
  if(formula_num_specials(F.formula, lhs=1, rhs=0, specials="data") != 1){
    return("Please specify a LHS with data() in the formula when supplying a data.frame/table for parameter data.")
  }

  # Verify only allowed args are given to data()
  names.params.data <- names(formula_readout_special_arguments(F.formula = F.formula, from.lhs = 1, from.rhs = 0,
                                                               name.special = "data", params.as.chars.only = FALSE)[[1]])

  if("" %in% names.params.data){
    err.msg <- c(err.msg, "Please specify all inputs to data() as named parameters.")
  }

  # remove unnamed args because cannot be verified
  names.params.data <- names.params.data[names.params.data != ""]

  for(n in names.params.data){
    if(!(n %in% c("unit", "split", "format"))){
      err.msg <- c(err.msg, paste0("The parameter ",n," is not valid input to data()!"))
    }
  }

  # already verified in formula alone that all args to data() are parsable
  return(err.msg)
}

check_userinput_latentattrition_formulavsdata_RHS23 <- function(F.formula, names.cov.data.life, names.cov.data.trans){
  err.msg <- c()
  # verify the specified cov data is in clv.data
  #   "." is by definition always in the data but remove from names
  vars.life  <- setdiff(all.vars(formula(F.formula, lhs=0, rhs=2)), ".")
  vars.trans <- setdiff(all.vars(formula(F.formula, lhs=0, rhs=3)), ".")
  # may be character(0) if only "."
  if(length(vars.life)){
    if(!all(vars.life %in% names.cov.data.life)){
      err.msg <- c(err.msg, "Not all lifetime covariates specified in the formula could be found in the data!")
    }
  }
  if(length(vars.trans)){
    if(!all(vars.trans %in% names.cov.data.trans)){
      err.msg <- c(err.msg, "Not all transaction covariates specified in the formula could be found in the data!")
    }
  }
  return(err.msg)
}

check_userinput_latentattrition_formulavsdata_RHS4 <- function(F.formula){
  err.msg <- c()

  # If has RHS4, may only be allowed ones
  #   "regularization" or "constraint"

  # Check that has only allowed specials and nothing else allowed
  if("." %in% all.vars(formula(F.formula, lhs=0, rhs=4))){
    # no suitable data argument to terms() as required to resolve "."
    err.msg <- c(err.msg, "Please do not use <.> in the fourth RHS.")
  }else{
    F.terms.rhs4 <- terms(F.formula, lhs=0, rhs=4, specials=c("regularization", "constraint"))
    num.rhs4.specials <- formula_num_specials(F.formula, lhs=0, rhs=4, specials=c("regularization", "constraint"))

    if(length(labels(F.terms.rhs4)) != num.rhs4.specials)
      err.msg <- c(err.msg, "Please choose only from the following for the fourth RHS: regularization(), constraint().")

    # if has regularization(), check that only once, with allowed args and are parsable
    if(!is.null(attr(F.terms.rhs4, "specials")[["regularization"]])){

      if(length(attr(F.terms.rhs4, "specials")[["regularization"]]) > 1)
        err.msg <- c(err.msg, "Please specify regularization() only once!")

      l.reg.args <- formula_parse_args_of_special(F.formula=F.formula, name.special="regularization", from.lhs=0, from.rhs=4)
      names.reg.args <- names(l.reg.args)

      if(!setequal(names.reg.args, c("life", "trans"))){
        err.msg <- c(err.msg, "Please give specify arguments life and trans in regularization()! (ie regularization(trans=5, life=7) )")
      }
      # and each only given once
      if(length(names.reg.args) != length(unique(names.reg.args))){
        err.msg <- c(err.msg, "Please specify every argument in regularization() exactly once!")
      }
      if(any(sapply(l.reg.args, is, "error")) | !all(sapply(l.reg.args, is.numeric))){
        err.msg <- c(err.msg, "Please specify every argument in regularization() as number!")
      }
    }

    # if has constraint(), check that only names (not named arguments) and parsable
    if(!is.null(attr(F.terms.rhs4, "specials")[["constraint"]])){
      l.constr.args <- formula_readout_special_arguments(F.formula = F.formula, name.special="constraint",
                                                         from.lhs = 0, from.rhs=4, params.as.chars.only=FALSE)
      # concat args in multiple constraint() specials
      l.constr.args <- do.call(c, l.constr.args)

      if(any(names(l.constr.args) != "")){
        err.msg <- c(err.msg, "Please provide only unnamed arguments to constraint()!")
      }
    }
  }

  return(err.msg)
}

#' @importFrom Formula as.Formula
#' @importFrom stats terms
check_userinput_latentattrition_formulavsdata <- function(formula, data, cov){
  err.msg <- c()

  # formula is verified to be basic correct
  F.formula <- as.Formula(formula)

  # raw data
  if(is.data.frame(data) || is.data.table(data)){
    err.msg <- check_userinput_latentattrition_formulavsdata_LHS1(formula)
    if(length(err.msg)){
      return(err.msg)
    }
  }else{
    if(!missing(cov)){
      return("Please do not give covariate data if an object of clv.data is given for parameter data!")
    }
    if(length(F.formula)[1] != 0){
      err.msg <- c(err.msg, "Please do not specify any LHS if a clv.data object is given for parameter data!")
    }
  }

  # No covariate data given
  #   either clv.data w/o cov OR data.frame and no cov object

  # nocov data:  only 1 RHS
  #   excludes static and dyn cov
  if(is(data, "clv.data") && !is(data, "clv.data.static.covariates")){
    if(length(F.formula)[2] != 1){
      err.msg <- c(err.msg, "The formula may only contain 1 part specifiying the model for data without covariates!")
    }
  }
  if((is.data.frame(data)|| is.data.table(data)) && missing(cov)){
    if(length(F.formula)[2] != 1){
      err.msg <- c(err.msg, "The formula may only contain 1 part specifiying the model if no covariate data is given!")
    }
  }


  # Covariate data
  #   either clv.data obj w/ cov OR data.frame/table with cov

  # cov data: requires at least 3 RHS (model, trans, life), max 4
  #   includes static and dyn covs
  if(is(data, "clv.data.static.covariates") || !(missing(cov))){
    if(length(F.formula)[2] < 3){
      err.msg <- c(err.msg, "The formula needs to specify the model as well as the transaction and the lifetime covariates for data with covariates!")
      return(err.msg)
    }
    if(length(F.formula)[2] > 4){
      err.msg <- c(err.msg, "The formula may consist of a maximum of 4 parts!")
    }


    # Verify RHS2&3
    if(is(data, "clv.data.static.covariates")){
      err.msg <- c(err.msg, check_userinput_latentattrition_formulavsdata_RHS23(F.formula, names.cov.data.life = data@names.cov.data.life,
                                                                                names.cov.data.trans = data@names.cov.data.trans))
    }else{
      cov.names <- setdiff(colnames(cov), c("Id", "Cov.Date"))
      err.msg <- c(err.msg, check_userinput_latentattrition_formulavsdata_RHS23(F.formula, names.cov.data.life = cov.names, names.cov.data.trans = cov.names))
    }
    if(length(err.msg)){
      return(err.msg)
    }

    # Verify RHS4
    if(length(F.formula)[2] == 4){
      err.msg <- c(err.msg, check_userinput_latentattrition_formulavsdata_RHS4(F.formula))
    }
  }
  return(err.msg)
}

# only works if special exists once
formula_parse_args_of_special <- function(F.formula, name.special, from.lhs, from.rhs){
  # read out args of "model" special. First element because has special only once
  l.args <- formula_readout_special_arguments(F.formula = F.formula, name.special=name.special,
                                              from.lhs = from.lhs, from.rhs=from.rhs, params.as.chars.only=FALSE)[[1]]

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

#' @importFrom stats terms
formula_num_specials <- function(F.formula, lhs, rhs, specials){
  F.terms <- terms(F.formula, lhs=lhs, rhs=rhs, specials=specials)
  return(sum(sapply(attr(F.terms, "specials"), length)))
}

#' @importFrom stats terms formula
formula_read_model_name <- function(F.formula){
  F.terms.rhs1 <- terms(formula(F.formula, lhs=0, rhs=1), specials=c("pnbd", "bgnbd", "ggomnbd", "gg"))
  return(names(unlist(attr(F.terms.rhs1, "specials"))))
}



# Needed to parse inputs to data() special because parameters needs to be parsed differently
formula_parse_args_of_data <- function(F.formula){
  # list of chars
  l.args <- formula_readout_special_arguments(F.formula = F.formula, name.special = "data",
                                              from.lhs = 1, from.rhs = 0, params.as.chars.only = FALSE)[[1]]
  if("split" %in% names(l.args)){
    user.split <- l.args[["split"]]
    # leave NULL if is NULL
    if(!is.null(user.split)){
      # Parse split to numeric
      l.args[["split"]] <- tryCatch(eval(parse(text=user.split)), error=function(e)e)
    }
  }
  # unit and format are left as os (characters or NULL)
  return(l.args)
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
formula_readout_special_arguments <- function(F.formula, name.special, from.lhs, from.rhs, params.as.chars.only = TRUE){

  F.terms   <- terms(formula(F.formula, lhs=from.lhs, rhs=from.rhs), specials = name.special)

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
    # replace the "", "NULL" as actual NULLs
    names.special <- lapply(names.special, lapply, function(char.arg){if(char.arg%in%c("NULL", "", "NA")) NULL else char.arg})
    # Name unnamed elements (=args) in sublists after the elements
    # names.special <- lapply(names.special, function(subl){
    #   unnamed.ind <- which(names(subl)=="")
    #   names(subl)[unnamed.ind] <- subl[unnamed.ind]
    #   return(subl)})
    # Does not need be unique per list, will not mixup entries if regressors are named same as g/iiv
    #   rather it is caught that they may not be twice
  }

  return(names.special)
}


