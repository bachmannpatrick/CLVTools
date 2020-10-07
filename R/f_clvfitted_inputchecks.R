.check_user_data_single_numeric <- function(n, var.name){
  err.msg <- c()
  if(!is.numeric(n))
    return(paste0(var.name, " has to be numeric!"))
  if(length(n)!=1)
    err.msg <- c(err.msg, paste0(var.name," has to be exactly 1 single number!"))
  if(anyNA(n))
    err.msg <- c(err.msg, paste0(var.name," may not be NA!"))
  return(err.msg)
}


# Can be
#     - NULL (= all in holdout)
#     - numeric (=this many periods)
#     - char (convertable!)
#     - Date (posixt, Date)
#' @importFrom lubridate is.Date is.POSIXt
#' @importFrom methods is
check_user_data_predictionend <- function(clv.fitted, prediction.end){
  # NULL is valid input to prediction end
  if(is.null(prediction.end))
    return(c())

  if(length(prediction.end) > 1)
    return("prediction.end can only be of length 1!")

  if(anyNA(prediction.end))
    return("prediction.end may not contain any NAs!")

  if(!is.numeric(prediction.end))
    if(!is.character(prediction.end))
      if(!is.Date(prediction.end))
        if(!is.POSIXt(prediction.end))
          return("prediction.end needs to be either of type character, numeric, or Date (Date or POSIXct/lt)!")

  # dont check for conversion with date.format because not needed and
  #   will often fail (no time in date.format because only daily data)
  if(is.POSIXt(prediction.end))
    return(c())

  # clv.fitted or clv.data object (to check prediction.end in plot())
  if(is(clv.fitted, "clv.fitted"))
    clv.time <- clv.fitted@clv.data@clv.time
  else
    clv.time <- clv.fitted@clv.time

  # if its a char or Date -> see if can convert
  if(is.character(prediction.end))
    if(anyNA( parse_date_time(x=prediction.end, orders = clv.time@time.format)))
      return("Please provide prediction.end in a format that can be parsed with the set date.format when creating the object!")

  # Whether the date itself is ok will be checked when converting!
  return(c())
}

check_user_data_startparams <- function(start.params, vector.names, param.names){
  err.msg <- c()

  if(is.null(start.params))
    return(err.msg)

  if(anyNA(start.params))
    err.msg <- c(err.msg, paste0("There may be no NAs in the ", param.names,"s!"))

  if(!is.numeric(start.params))
    err.msg <- c(err.msg, paste0("Please provide a numeric vector as ",param.names,"s!"))

  if(is.null(names(start.params)))
    err.msg <- c(err.msg, paste0("Please provide a named vector as ",param.names,"s!"))

  # check that every name is not only "" (ie some are unnamed)
  for(n in names(start.params))
    if(nchar(n) < 1)
      err.msg <- c(err.msg, paste0("Please provide names for every parameter in the ",param.names, "s!"))

  # compare to parameters required by model/covariate
  if(length(start.params) != length(vector.names))
    err.msg <- c(err.msg, paste0("Please provide exactly ", length(vector.names), " ", param.names, "s named ", paste(vector.names, collapse = ", "), "!"))

  # check that every required parameter is present as name
  for(n in vector.names)
    if(!(n %in% names(start.params)))
      err.msg <- c(err.msg, paste0("Please provide the ",param.names, " for ", n, "!"))

  return(err.msg)
}

check_user_data_startparamscov <- function(start.params.cov, names.params.cov, name.of.cov){
  return(check_user_data_startparams(start.params = start.params.cov, vector.names = names.params.cov,
                               param.names = paste0(name.of.cov, " covariate start parameter")))
}



check_user_data_namescov_reduce <- function(names.cov, data.cov.dt, name.of.cov){
  err.msg <- c()

  if(is.null(names.cov))
    return(c()) #return("Covariate names may not be NULL")

  if(!is.character(names.cov))
    return(paste0("Covariate names for ", name.of.cov," covariates must be an unnamed character vector!"))

  if(!is.null(names(names.cov)))
    err.msg <- c(err.msg, paste0("Covariate names for ", name.of.cov," covariates should be provided as named vector!"))

  if(anyNA(names.cov))
    err.msg <- c(err.msg, paste0("There may be no NAs in the covariate names in the ", name.of.cov," covariates!"))

  for(n in names.cov)
    if(!(n %in% colnames(data.cov.dt)))
      err.msg <- c(err.msg, paste0("The column named ", n, " could not be found in the ",name.of.cov," covariate data!"))

  if(length(names.cov) != length(unique(names.cov)))
    err.msg <- c(err.msg, "Every covariate name for ",name.of.cov," covariates may only appear once!")

  return(err.msg)
}


check_user_data_reglambdas <- function(reg.lambdas){
  err.msg <- c()

  if(is.null(reg.lambdas))
    return(err.msg)

  if(!is.numeric(reg.lambdas))
    return("The regularization lambdas have to be a numeric vector!")

  if(anyNA(reg.lambdas))
    return("The regularization lambdas may not contain any NAs!")

  if(any(reg.lambdas < 0))
    err.msg <- c(err.msg, "The regularization lambdas have to be positive or 0!")

  if(length(reg.lambdas) != 2)
    err.msg <- c(err.msg, "Exactly 2 regularization lambdas need to be given!")

  if(!all(c("life", "trans") %in% names(reg.lambdas)))
     err.msg <- c(err.msg, "The regularization lambdas need to be named \"life\" and \"trans\"!")

  return(err.msg)
}




#' @importFrom optimx optimx
#' @importFrom methods formalArgs
#' @importFrom utils getFromNamespace
check_user_data_optimxargs <- function(optimx.args){
  err.msg <- c()
  if(is.null(optimx.args))
    return("The parameter \"optimx.args\" may not be NULL!")

  if(!is.list(optimx.args) | is.data.frame(optimx.args))
    return("Please provide \"optimx.args\" as a list!")

  # further checks only if not empty list (ie no argument passed)
  if(length(optimx.args) > 0){

    if(is.null(names(optimx.args)))
      err.msg <- c(err.msg, "Please provide a named list for \"optimx.args\"!")

    for(n in names(optimx.args))
      if(nchar(n) < 1){
        err.msg <- c(err.msg, "Please provide names for every element in \"optimx.args\"!")
        break
      }

    # the names need to match the inputs to optimx
    optimx.allowed <- formalArgs(getFromNamespace("optimx",ns="optimx"))
    # optimx.allowed <- c("gr", "hess", "lower", "upper", "method", "itnmax", "hessian")
    for(n in names(optimx.args))
      if(!(n %in% optimx.allowed))
        err.msg <- c(err.msg, paste0("The element ",n," in optimx.args is not a valid input to optimx()!"))

    # Now, multiple optimization methods are allowed and the last one is used
    # only one method allowed
    # if("method" %in% names(optimx.args))
    #   if(length(optimx.args$method) > 1)
    #     err.msg <- c(err.msg, paste0("Only a single method can be used at a time!"))

  }
  return(err.msg)
}


check_user_data_continuousdiscountfactor <- function(continuous.discount.factor){
  if(is.null(continuous.discount.factor))
    return("continuous.discount.factor cannot be NULL!")

  err.msg <- .check_user_data_single_numeric(n = continuous.discount.factor,
                                             var.name = "continuous.discount.factor")

  if(!(continuous.discount.factor < 1 & continuous.discount.factor >= 0))
    return("continuous.discount.factor needs to be in the interval [0,1)")
  return(c())
}


check_user_data_startparamcorm <- function(start.param.cor, use.cor){
  err.msg <- c()
  # Null implies that start params need to be generated
  if(is.null(start.param.cor))
    return(err.msg)

  if(!is.null(use.cor))
    if((!anyNA(use.cor)) & use.cor == FALSE)
      err.msg <- c(err.msg, "start.param.cor cannot be set if no correlation shall be used!")

  err.msg <- c(err.msg, .check_user_data_single_numeric(n = start.param.cor,
                                                        var.name ="start.param.cor"))

  if(length(err.msg)==0)
    if(start.param.cor > 1 | start.param.cor < -1)
      err.msg <- c(err.msg, "start.param.cor has to be in the interval [-1, 1]!")

  # no names needed
  return(err.msg)
}


check_user_data_startparamconstr <- function(start.params.constr, names.cov.constr){
  if(missing(start.params.constr))
    return(c())
  if(is.null(start.params.constr))
    return(c())

  if(missing(names.cov.constr) | is.null(names.cov.constr))
    return("Start parameters for constraint covariates should only be provided if the names for the constraint parameters are provided as well!")

  # Re-use as exactly the same checks are needed
  return(check_user_data_startparamscov(start.params.cov = start.params.constr,
                                       names.params.cov  = names.cov.constr,
                                       name.of.cov       = "Constraint"))
}

check_user_data_namesconstr <- function(clv.fitted, names.cov.constr){
  err.msg <- c()

  if(is.null(names.cov.constr))
    return(err.msg) #return("Covariate names may not be NULL")

  if(!is.character(names.cov.constr))
    return("Covariate names for Constraint covariates must be an unnamed character vector!")

  if(!is.null(names(names.cov.constr)))
    err.msg <- c(err.msg, "Covariate names for constraint covariates should be provided as unnamed vector!")

  if(anyNA(names.cov.constr))
    err.msg <- c(err.msg, "There may be no NAs in the covariate names for Constraint covariates!")

  # Check that every name is in both data
  for(n in names.cov.constr){
    if(!(n %in% colnames(clv.fitted@clv.data@data.cov.life)))
      err.msg <- c(err.msg, paste0("The Constraint covariate named ", n, " could not be found in the Lifetime covariate data!"))

    if(!(n %in% colnames(clv.fitted@clv.data@data.cov.trans)))
      err.msg <- c(err.msg, paste0("The Constraint covariate named ", n, " could not be found in the Transaction covariate data!"))
  }

  # Found only once
  if(length(names.cov.constr) != length(unique(names.cov.constr)))
    err.msg <- c(err.msg, "Every covariate name for Constraint covariates may only appear exactly once!")

  return(err.msg)
}

check_user_data_emptyellipsis <- function(...){

  err.msg <- c()
  if(...length() > 0)
    err.msg <- c(err.msg, "Any further parameters passed in ... are ignored because they are not needed!")

  return(err.msg)
}


check_user_data_containsspendingdata <- function(clv.data){
  err.msg <- c()
  if(!clv.data.has.spending(clv.data))
    err.msg <- c(err.msg, "The data object is required to contain spending data!")

  return(err.msg)
}
