#' @template template_setstaticcov
#' @export
SetStaticCovariates <- function(clv.data, data.cov.life, data.cov.trans, names.cov.life, names.cov.trans, name.id="Id"){

  Id <- NULL

  # Do not use S4 generics to catch other classes because it creates confusing documentation entries
  #   suggesting that there are legitimate methods for these
  if(!is(clv.data, "clv.data"))
    stop("Covariate data can only be added to objects of class clv.data!")
  if(is(clv.data, "clv.data.static.covariates") | is(clv.data, "clv.data.dynamic.covariates"))
    stop("Cannot set static covariates because this object has covariates set already!", call. = FALSE)


  # Basic inputchecks ---------------------------------------------------------------------
  #   for parameters

  # Check if data has basic properties, otherwise cannot process column names
  if(!is.data.frame(data.cov.life) | !is.data.frame(data.cov.trans))
    check_err_msg("Only covariate data of type data.frame or data.table can be processed!")

  if(nrow(data.cov.life) == 0 | nrow(data.cov.trans) == 0)
    check_err_msg("Covariate data may not be empty!")

  err.msg <- c()
  err.msg <- c(err.msg, check_userinput_datanocov_namescov(names.cov=names.cov.life,  data.cov.df=data.cov.life,  name.of.covariate="Lifetime"))
  err.msg <- c(err.msg, check_userinput_datanocov_namescov(names.cov=names.cov.trans, data.cov.df=data.cov.trans, name.of.covariate="Transaction"))
  # name id in both covariate data
  err.msg <- c(err.msg, check_userinput_datanocov_columnname(name.col=name.id, data=data.cov.life))
  err.msg <- c(err.msg, check_userinput_datanocov_columnname(name.col=name.id, data=data.cov.trans))
  check_err_msg(err.msg)


  # Convert covariate data to data.table and check -----------------------------------------
  #   to better process in the check and convert function *_datacov
  #   Copy data as it will be manipulated by reference

  data.cov.life  <- copy(data.cov.life)
  data.cov.trans <- copy(data.cov.trans)

  if(!is.data.table(data.cov.life))
    setDT(data.cov.life)
  if(!is.data.table(data.cov.trans))
    setDT(data.cov.trans)

  setkeyv(data.cov.life,  cols = name.id)
  setkeyv(data.cov.trans, cols = name.id)

  # make Id to char before comparing it to Id in data.transaction

  err.msg <- c(err.msg, check_userinput_data_id(dt.data=data.cov.life,  name.id=name.id, name.var="Lifetime covariate data"))
  err.msg <- c(err.msg, check_userinput_data_id(dt.data=data.cov.trans, name.id=name.id, name.var="Transaction covariate data"))
  check_err_msg(err.msg)

  # need to subset to relevant columns only in case there is already a columns Id in the data
  #   otherwise renaming leads to 2 columns with the same name
  data.cov.life  <- data.cov.life[,  .SD, .SDcols = c(name.id, names.cov.life)]
  data.cov.trans <- data.cov.trans[, .SD, .SDcols = c(name.id, names.cov.trans)]
  setnames(data.cov.life,  old = name.id, new = "Id")
  setnames(data.cov.trans, old = name.id, new = "Id")
  data.cov.life[,  Id := .convert_userinput_dataid(Id)]
  data.cov.trans[, Id := .convert_userinput_dataid(Id)]

  # Make static cov specific checks on covariate data
  #   only after if is DT because heavily relies on it for efficiency
  #   only after Id is character because needed to compare to data.transaction Id
  check_err_msg(check_userinput_datanocov_datastaticcov(clv.data = clv.data, dt.data.static.cov = data.cov.life,  names.cov = names.cov.life, name.of.covariate="Lifetime"))
  check_err_msg(check_userinput_datanocov_datastaticcov(clv.data = clv.data, dt.data.static.cov = data.cov.trans,  names.cov = names.cov.trans, name.of.covariate="Transaction"))


  # Make cov data --------------------------------------------------------------------------
  #   keep numbers, char/factors to dummies
  #   input data.cov.X is renamed by ref to legal names
  l.covs.life  <- convert_userinput_covariatedata(dt.cov.data=data.cov.life,  names.cov=names.cov.life)
  l.covs.trans <- convert_userinput_covariatedata(dt.cov.data=data.cov.trans, names.cov=names.cov.trans)

  data.cov.life  <- l.covs.life$data.cov
  data.cov.trans <- l.covs.trans$data.cov
  names.cov.data.life  <- l.covs.life$final.names.cov
  names.cov.data.trans <- l.covs.trans$final.names.cov

  setkeyv(data.cov.life,  cols = "Id")
  setkeyv(data.cov.trans, cols = "Id")

  # Create object ---------------------------------------------------------------------------
  # Create from given clv.data obj
  return(clv.data.static.covariates(no.cov.obj     = clv.data,
                                    data.cov.life  = data.cov.life,
                                    data.cov.trans = data.cov.trans,
                                    names.cov.data.life  = names.cov.data.life,
                                    names.cov.data.trans = names.cov.data.trans))
}
