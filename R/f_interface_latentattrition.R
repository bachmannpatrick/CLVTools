#' Formula Interface for Latent Attrition Models
#'
#' @template template_param_optimxargs
#' @template template_param_verbose
#' @param family A latentAttrition model. One of \code{pnbd},  \code{bgnbd}, or \code{ggomnbd}.
#' @param formula Formula to select and transform covariates in \code{data}. Has to be left empty if \code{data} contains no covariates. See Details.
#' @param data A \code{clv.data} object
#'
#' @description
#' Fit latent attrition models for transaction behavior, using a formula to specify the covariates.
#'
#' @details
#' A two-part formula is used to select and transform the covariates stored in \code{data} before the model is estimated on it.
#' May not be given if \code{data} contains no covariates.
#'
#' The formula left hand side (LHS) has to remain empty and may never be specified.
#'
#' The formula right hand side (RHS) follows a two-part notation using \code{|} as separator.
#' \itemize{
#' \item 1st part: Which covariates to include for the lifetime process, potentially transforming them and adding interactions. The dot ('.') refers to all lifetime covariates.
#' \item 2nd part: Which covariates to include for the transaction process, potentially transforming them and adding interactions. The dot ('.') refers to all transaction covariates
#' }
#'
#' e.g: \code{ ~ covlife | covtrans}
#'
#' See the example section for illustrations on how to specify the formula parameter.
#'
#'
#' @seealso Models for inputs to family: \link{pnbd}, \link{ggomnbd}, \link{bgnbd}.
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
#' # Fit models without covariates.
#' # Note that NO formula may be specified in this case
#' latentAttrition(formula =, family=pnbd, data=clv.nocov)
#' latentAttrition(formula =, family=bgnbd, data=clv.nocov)
#' latentAttrition(formula =, family=ggomnbd, data=clv.nocov)
#'
#' # Fit pnbd with start parameters and correlation
#' # required args are passed as part of '...'
#' latentAttrition(formula =, family=pnbd, data=clv.nocov,
#'                 use.cor=TRUE,
#'                 start.params.model=c(r=1, alpha=10, s=2, beta=8))
#'
#'
#' # Fit pnbd with all present covariates
#' latentAttrition(formula=~.|., family=pnbd, data=clv.staticcov)
#'
#' # Fit pnbd with selected covariates
#' latentAttrition(formula=~Gender|Channel+Gender, family=pnbd,
#'                 data=clv.staticcov)
#'
#' # Fit pnbd with start parameters for covariates
#' latentAttrition(formula=~Gender|., family=pnbd,
#'                 data=clv.staticcov,
#'                 start.params.life = c(Gender = 0.6),
#'                 start.params.trans = c(Gender = 0.6, Channel = 0.4))
#'
#' # Fit pnbd with transformed covariate data
#' latentAttrition(formula=~Gender|I(log(Channel+2)), family=pnbd,
#'                 data=clv.staticcov)
#'
#' # Fit pnbd with all covs and regularization
#' latentAttrition(formula=~.|., family=pnbd, data=clv.staticcov,
#'                 reg.lambdas = c(life=3, trans=8))
#'
#' # Fit pnbd with all covs and constraint parameters for Channel
#' latentAttrition(formula=~.|., family=pnbd, data=clv.staticcov,
#'                 names.cov.constr='Channel')
#'
#' }
#'
#'
#' @importFrom Formula as.Formula
#' @importFrom stats terms formula
#' @importFrom methods is
#' @export
latentAttrition <- function(formula, family, data, optimx.args=list(), verbose=TRUE, ...){

  cl  <- match.call(call = sys.call(), expand.dots = TRUE)

  check_err_msg(check_userinput_latentattrition_family(family=family))
  check_err_msg(check_userinput_data(data=data))
  check_err_msg(check_userinput_formula_data(formula=formula, data = data))
  check_err_msg(check_userinput_latentattrition_dots_family_data(family=family, data=data, ...))

  # if data has covariates, they need to be transformed
  if(is(data, "clv.data.static.covariates")){
    # only has formula if is cov data
    F.formula <- as.Formula(formula)

    if(is(data, "clv.data.dynamic.covariates")){
      # better to remove tp.cov.x columns here than trying to figure out whether
      # there are any in formulainterface_create_clvdataobj()
      data <- formulainterface_create_clvdataobj(F.formula = F.formula, clv.data.nocov = as(data, "clv.data"),
                                                 create.dyncov = TRUE,
                                                 dt.cov.life = data@data.cov.life[, !c("tp.cov.lower", "tp.cov.upper")],
                                                 dt.cov.trans = data@data.cov.trans[, !c("tp.cov.lower", "tp.cov.upper")])
    }else{
      # Dont need to remove Id column
      data <- formulainterface_create_clvdataobj(F.formula = F.formula, clv.data.nocov = as(data, "clv.data"),
                                                 create.dyncov = FALSE,
                                                 dt.cov.life = data@data.cov.life,
                                                 dt.cov.trans = data@data.cov.trans)
    }
  }


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


#' @importFrom stats update
formulainterface_create_clvdataobj <- function(F.formula, create.dyncov, clv.data.nocov, dt.cov.life, dt.cov.trans){

  if(create.dyncov){
    cov.id.vars <- c("Id", "Cov.Date")
  }else{
    cov.id.vars <- "Id"
  }

  # Have to use model.matrix() in order to build interactions from given data
  # model.frame() would otherwise be more desirable as it creates the relevant cols (incl transformations) but without dummifying
  # model.matrix() also creates the intercept and expands the dot to include Id and Cov.Date which are dummified.
  # Therefore need to remove ids vars and intercept from formula by subtracting with '-Id-1'
  # update.formula() requires expanding the dot '.' with the names in the given data.
  # use terms() so subset Formula to relevant rhs and expand dot.

  # Considered alternatives:
  #   - reformulate(terms(F, data), intercept=F) but remove Id, Cov.Date lables from returned terms object. Good option but manipulating formula seems more natural.
  #   - use terms() but remove columns Id and Cov.Date from data to not expand dot to include these. May incur substantial overhead if data is large.

  # f.remove: formula to remove intercept and covariate ids
  f.remove <- eval(parse(text=paste0('~ . - 1 - ', paste0(cov.id.vars, collapse = '-'))))
  f.formula.life  <- update(terms(F.formula, lhs=0, rhs=1, data=dt.cov.life),  f.remove)
  f.formula.trans <- update(terms(F.formula, lhs=0, rhs=2, data=dt.cov.trans), f.remove)

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


check_userinput_latentattrition_family <- function(family){
  # not missing
  if(missing(family))
    return("Please provide one of the following inputs for parameter \'family\': pnbd, bgnbd, ggomnbd")

  # has to be exactly one of the methods exported from the package (pnbd, bgnbd, ggomnbd)
  if(!any(identical(family, pnbd), identical(family, bgnbd), identical(family, ggomnbd))){
    return("Please provide one of the following inputs for parameter \'family\': pnbd, bgnbd, ggomnbd")
  }
  return(c())
}

#' @importFrom methods is
check_userinput_data <- function(data){
  if(missing(data)){
    return("Please provide a 'clv.data' object as input for \'data\'.")
  }

  if(!is(data, class2 = "clv.data")){
    return("Please provide a 'clv.data' object as input for \'data\'.")
  }

  # all further input checks are done in family(), i.e. gg()
  return(c())
}


#' @importFrom Formula as.Formula is.Formula
#' @importFrom stats terms formula
#' @importFrom methods is
check_userinput_formula_data <- function(formula, data){

  # has to be missing if data has no covariates
  if(!is(data, class2 = "clv.data.static.covariates")){
    if(!missing(formula)){
      return("Parameter 'formula' may not be specified if the data has no covariates.")
    }

  }else{
    # static or dyn cov data: formula is required

    if(missing(formula)){
      return("Please provide a valid formula object as \'formula\' parameter.")
    }

    # Check if it is a formula
    if(!is(object = formula, class2 = "formula") && !is.Formula(formula)){
      return("Please provide a valid formula object as \'formula\' parameter.")
    }

    F.formula <- as.Formula(formula)

    # there may not be any LHS
    if(length(F.formula)[1] > 0)
      return("Please specify no dependent variable (left-hand side) in the formula.")

    # there have to be exactly 2 RHS
    if(length(F.formula)[2] != 2){
      return("Please specify lifetime and transaction covariates on the right-hand of the formula using a two-part notation with '|' as separator (e.g. ~ covlife | covtrans).")
    }

    # all covs specified in RHS1 and RHS2 have to be in data
    #   "." is by definition always in the data but remove from names
    vars.life  <- setdiff(all.vars(formula(F.formula, lhs=0, rhs=1)), ".")
    vars.trans <- setdiff(all.vars(formula(F.formula, lhs=0, rhs=2)), ".")

    # may be character(0) if only "."
    if(length(vars.life)){
      if(!all(vars.life %in% data@names.cov.data.life)){
        return("Not all lifetime covariates specified in the formula could be found in the data!")
      }
    }
    if(length(vars.trans)){
      if(!all(vars.trans %in% data@names.cov.data.trans)){
        return("Not all transaction covariates specified in the formula could be found in the data!")
      }
    }
  }
  return(c())
}


#' @importFrom methods getMethod signature formalArgs is
check_userinput_latentattrition_dots_family_data <- function(family, data, ...){

  # find for which generic it will dispatch
  method.def <- getMethod(family, signature(clv.data = class(data)))
  # read the allowed inputs from its args definition
  allowed.args <- formalArgs(method.def)

  # unfortunately, this does not list the additional args listed for static and dyncov data objects
  # it defines a function .local within and this .local has the full signature and is then called
  # the following code works to extract the args from this nested method
  # formalArgs(eval(body(getMethod(family, signature(clv.data = class(data))))[[2]]))
  # however, this may not work in the future. therefore rather define manually

  if(is(data, "clv.data.static.covariates")){
    # static or dyncov methods may take more
    allowed.args <- c(allowed.args,
                      # dont allow names of covariates which are specified in the formula already
                      c(#"names.cov.life",
                        #"names.cov.trans",
                        "start.params.life",
                        "start.params.trans",
                        "names.cov.constr",
                        "start.params.constr",
                        "reg.lambdas"))
  }

  # these are specified already
  allowed.args <- setdiff(allowed.args, c("clv.data", "optimx.args", "verbose", "..."))


  # use alist() to not evaluate args in ...
  # do not use ...names() as it was only introduced in R 4.1.0 which is later
  # than the min required R version
  # use one of the proposed implementations of ...names() from https://bugs.r-project.org/show_bug.cgi?id=17705
  # names.dots <- names(substitute(alist(...)))
  names.dots <- names(match.call(expand.dots = TRUE))[c(-1L, -2L, -3L)] # remove call, family, data
  not.allowd.args <- setdiff(names.dots, allowed.args)

  if(length(not.allowd.args)){
    return(paste0(
      "The following arguments may not be passed to '...': ", paste0(not.allowd.args, collapse=', ')))
  }

  return(c())
}
