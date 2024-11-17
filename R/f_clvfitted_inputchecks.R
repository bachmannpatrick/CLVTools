.check_user_data_single_numeric <- function(n, var.name){
  err.msg <- c()
  if(!is.numeric(n))
    return(paste0(var.name, " has to be numeric!"))
  if(length(n)!=1)
    err.msg <- c(err.msg, paste0(var.name," has to be exactly 1 single number!"))
  if(anyNA(n))
    err.msg <- c(err.msg, paste0(var.name," may not be NA!"))
  if(any(!is.finite(n)))
    err.msg <- c(err.msg, paste0(var.name," may not contain any non-finite items!"))
  return(err.msg)
}

check_user_data_integer_vector_greater0 <- function(vec, var.name){
  err.msg <- .check_userinput_integer_vector(vec.int=vec, var.name=var.name)
  if(length(err.msg))
    return(err.msg)

  if(any(vec < 0)){
    return(paste0(var.name, " has to be a vector of all positive integer numbers (>=0)!"))
  }
  return(c())
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

  if(length(vector.names) == 0 & length(start.params) > 0)
    return(paste0("There may be no ", param.names, "!"))

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


check_user_data_numboots <- function(num.boots){
  if(missing(num.boots)){
    return("Parameter num.boots cannot be missing!")
  }

  err.msg <- .check_user_data_single_numeric(n = num.boots, var.name = "num.boots")
  if(length(err.msg) > 0){
    return(err.msg)
  }

  if(!(num.boots %% 1 == 0)){
    err.msg <- c(err.msg, "Parameter num.boots needs to be an integer!")
  }

  if(num.boots < 1){
    err.msg <- c(err.msg, "Parameter num.boots needs to be at least 1!")
  }

  return(err.msg)
}

check_user_data_fnbootapply <- function(fn.boot.apply){
  if(missing(fn.boot.apply)){
    return("Parameter fn.boot.apply cannot be missing!")
  }

  if(!is.function(fn.boot.apply)){
    return("Parameter fn.boot.apply has to be a function!")
  }
  return(c())
}


check_user_data_fnsample <- function(fn.sample){
  # may be NULL
  if(is.null(fn.sample)){
    return()
  }

  if(missing(fn.sample)){
    return("Parameter fn.sample cannot be missing!")
  }

  if(!is.function(fn.sample)){
    return("Parameter fn.sample has to be a function!")
  }
  return(c())
}



check_user_data_level <- function(level){
  if(missing(level)){
    return("Parameter level cannot be missing!")
  }

  err.msg <- .check_user_data_single_numeric(n = level, var.name = "level")
  if(length(err.msg) > 0){
    return(err.msg)
  }

  if(!(level > 0 & level < 1)){
    err.msg <- c(err.msg, "Parameter level needs to be in the interval (0,1)!")
  }

  return(err.msg)
}


check_user_data_uncertainty <- function(uncertainty){
  if(missing(uncertainty)){
    return("Parameter uncertainty cannot be missing!")
  }

  return(.check_userinput_matcharg(char=uncertainty, choices=c("none", "boots"), var.name="uncertainty"))
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
    return("Covariate names for constraint covariates must be an unnamed character vector!")

  if(!is.null(names(names.cov.constr)))
    err.msg <- c(err.msg, "Covariate names for constraint covariates have to be provided as unnamed vector!")

  if(length(names.cov.constr) == 0)
    err.msg <- c(err.msg, "The names for constraint covariates may not be empty!")

  if(anyNA(names.cov.constr))
    err.msg <- c(err.msg, "There may be no NAs in the covariate names for constraint covariates!")

  # Check that every name is in both data
  for(n in names.cov.constr){
    if(!(n %in% colnames(clv.fitted@clv.data@data.cov.life)))
      err.msg <- c(err.msg, paste0("The constraint covariate named ", n, " could not be found in the Lifetime covariate data!"))

    if(!(n %in% colnames(clv.fitted@clv.data@data.cov.trans)))
      err.msg <- c(err.msg, paste0("The constraint covariate named ", n, " could not be found in the Transaction covariate data!"))
  }

  # Found only once
  if(length(names.cov.constr) != length(unique(names.cov.constr)))
    err.msg <- c(err.msg, "Every covariate name for constraint covariates may only appear exactly once!")

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

check_user_data_othermodels <- function(other.models){
  if(!is.list(other.models)){
    return("Parameter other.models has to be a list of fitted transaction models!")
  }

  # each element in list is a transaction model

  if(!all(sapply(other.models, is, class2 = "clv.fitted.transactions"))){
    return("All elements in 'other.models' have to be fitted transaction models, e.g the output of pnbd(), bgnbd(), or ggomnbd()!")
  }

  return(c())
}


check_user_data_label <- function(label, other.models){
  if(is.null(label)){
    # null is allowed = std. model name(s)
    return(c())
  }
  if(length(other.models)==0){
      return(.check_userinput_charactervec(char=label, var.name="label", n=1))
  }else{

    # requires names for main and all other models
    err.msg <- .check_userinput_charactervec(char=label, var.name="label", n=1+length(other.models))
    if(length(err.msg)){
      return(err.msg)
    }

    if(any(duplicated(label))){
      err.msg <- c(err.msg, "Parameter label may not contain any duplicates!")
    }
    return(err.msg)
  }
}



check_user_data_predict_newcustomer_numperiods <- function(num.periods){
  err.msg <- .check_user_data_single_numeric(n=num.periods, var.name='num.periods')
  if(length(err.msg)){
    return(err.msg)
  }

  if(num.periods < 0){
    err.msg <- c(err.msg, c("Parameter num.periods has to be positive (>=0)!"))
  }

  return(err.msg)
}


check_user_data_predict_newcustomer_staticcov <- function(clv.fitted, clv.newcustomer){

  # is exactly "clv.newcustomer.static.cov"
  if(!is(clv.newcustomer, "clv.newcustomer.static.cov")){
    return("Parameter newdata has to be output from calling `newcustomer.static()`!")
  }

  # only need to check if right columns are here, all other things were checked when creating newcustomer
  required.names.cov <- names(clv.fitted@prediction.params.life)
  if(!identical(sort(colnames(clv.newcustomer@data.cov.life)), sort(required.names.cov))){
    return(paste0("The Lifetime covariate data has to contain exactly the following columns: ", paste(required.names.cov, collapse = ", "), "!"))
  }

  required.names.cov <- names(clv.fitted@prediction.params.trans)
  if(!identical(sort(colnames(clv.newcustomer@data.cov.trans)), sort(required.names.cov))){
    return(paste0("The Transaction covariate data has to contain exactly the following columns: ", paste(required.names.cov, collapse = ", "), "!"))
  }

  return(c())
}

check_user_data_predict_newcustomer_dyncov <- function(clv.fitted, clv.newcustomer){

  if(!is(clv.newcustomer, "clv.newcustomer.dynamic.cov")){
    stop("Parameter newdata has to be output from calling `newcustomer.dynamic()`")
  }

  # Has to contain exactly the same columns as the covariate data used when fitting
  names.col <- c(names(clv.fitted@prediction.params.life), "Cov.Date")
  if(!identical(sort(colnames(clv.newcustomer@data.cov.life)), sort(names.col))){
    return(paste0("The Lifetime covariate data has to contain exactly the following columns: ", paste(names.col, collapse = ", "), "!"))
  }

  names.col <- c(names(clv.fitted@prediction.params.trans), "Cov.Date")
  if(!identical(sort(colnames(clv.newcustomer@data.cov.trans)), sort(names.col))){
    return(paste0("The Transaction covariate data has to contain exactly the following columns: ", paste(names.col, collapse = ", "), "!"))
  }

  return(c())
}

check_user_data_newcustomer_dyncovspecific <- function(clv.time, dt.cov.life, dt.cov.trans, tp.first.transaction, tp.prediction.end){
  Cov.Date <- NULL

  # It is per-se not required that life and trans covs start and end on same
  # timepoint. Neither for the prediction nor for tp.min.cov.date/tp.max.cov.data.
  # It is however still verified by comparing both, life and trans covs against
  # the same sequence of cov timepoints (`dt.required.cov.dates`).


  # Required covariate dates are from first tranasction until prediction end
  #   They have to be from the period containing the first transaction until the period
  #   containing the prediction end

  # The covariates may also be longer or than the prediction end
  # We therefore do not enforce that the required cov dates end at the prediction end
  tp.max.cov.data <- max(
    tp.prediction.end,
    dt.cov.life[, max(Cov.Date)],
    dt.cov.trans[, max(Cov.Date)]
  )

  # The covariates may also start earlier than the first transaction
  # We therefore do not enforce that the required cov dates start at the first transaction
  # Alternatively, could cut off the covariates before the first required cov date
  # but then these are not compared against dt.required.cov.dates and could be wrong
  tp.min.cov.date <- min(
    tp.first.transaction,
    dt.cov.life[, min(Cov.Date)],
    dt.cov.trans[, min(Cov.Date)]
  )

  dt.required.cov.dates <- clv.time.sequence.of.covariate.timepoints(
    clv.time = clv.time,
    tp.start = tp.min.cov.date,
    tp.end   = tp.max.cov.data,
    require.min.3.timepoints = FALSE
  )

  err.msg.required.cov.dates <- paste0(
    "The covariate data has to be spaced ",tolower(clv.time.tu.to.ly(clv.time))," and start at ",
    clv.time.format.timepoint(clv.time=clv.time,timepoint=clv.time.floor.date(clv.time=clv.time, timepoint=tp.first.transaction)),
    " (the period containing the first transaction) and end on ",
    clv.time.format.timepoint(clv.time=clv.time, timepoint=dt.required.cov.dates[, max(Cov.Date)]),
    " (the period containing the prediction end).")

  # By comparing the dates actually in the covariates with the required covariate dates, we can check
  # - no dates are missing
  # - covs are spaced correctly
  # - covs contain first transaction and prediction end
  # - both covs have the same min(Cov.Date) and max(Cov.Date)

  # Check that Cov.Date in dt.cov.life corresponds exactly to dt.required.cov.dates
  if(!isTRUE(all.equal(dt.cov.life[order(Cov.Date), "Cov.Date"], dt.required.cov.dates[order(Cov.Date), "Cov.Date"]))){
    return(err.msg.required.cov.dates)
  }

  if(!isTRUE(all.equal(dt.cov.trans[order(Cov.Date), "Cov.Date"], dt.required.cov.dates[order(Cov.Date), "Cov.Date"]))){
    return(err.msg.required.cov.dates)
  }

  return(c())
}

