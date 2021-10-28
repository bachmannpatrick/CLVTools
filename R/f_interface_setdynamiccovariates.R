#' @template template_setdynamiccov
#' @include class_clv_data.R
#' @include class_clv_data_staticcovariates.R
#' @include class_clv_data_dynamiccovariates.R
#' @export
SetDynamicCovariates <- function(clv.data, data.cov.life, data.cov.trans, names.cov.life, names.cov.trans, name.id="Id", name.date="Date"){

  Cov.Date <- Id <- NULL

  # Do not use S4 generics to catch other classes because it creates confusing documentation entries
  #   suggesting that there are legitimate methods for these
  if(!is(clv.data, "clv.data"))
    stop("Covariate data can only be added to objects of class clv.data!")
  if(is(clv.data, "clv.data.static.covariates") | is(clv.data, "clv.data.dynamic.covariates"))
    stop("Cannot set dynamic covariates because this object has covariates set already!", call. = FALSE)


  # Basic inputchecks ---------------------------------------------------------------------
  #   for parameters name

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
  # name date in both covariate data
  err.msg <- c(err.msg, check_userinput_datanocov_columnname(name.col=name.date, data=data.cov.life))
  err.msg <- c(err.msg, check_userinput_datanocov_columnname(name.col=name.date, data=data.cov.trans))
  check_err_msg(err.msg)

  if(any(name.date %in% names.cov.life) | any(name.date %in% names.cov.trans))
    check_err_msg("The name for Date cannot also be used as a Covariate.")


  # Convert covariate data to data.table to do more sophisticated checks -----------------------------------------
  data.cov.life  <- copy(data.cov.life)
  data.cov.trans <- copy(data.cov.trans)

  if(!is.data.table(data.cov.life))
    setDT(data.cov.life)
  if(!is.data.table(data.cov.trans))
    setDT(data.cov.trans)

  # Check and convert Id and Date ---------------------------------------------------------------------------------

  # Id is correct datatype
  err.msg <- c(err.msg, check_userinput_data_id(dt.data = data.cov.life,  name.id = name.id, name.var="Lifetime covariate"))
  err.msg <- c(err.msg, check_userinput_data_id(dt.data = data.cov.trans, name.id = name.id, name.var="Transaction covariate"))
  # Date is correct datatype
  err.msg <- c(err.msg, check_userinput_data_date(dt.data = data.cov.life,  name.date = name.date, name.var="Lifetime covariate"))
  err.msg <- c(err.msg, check_userinput_data_date(dt.data = data.cov.trans, name.date = name.date, name.var="Transaction covariate"))
  check_err_msg(err.msg)

  # need to subset to only the relevant columns here already in case there is already a columns Id in the data
  #   otherwise renaming leads to 2 columns with the same name
  data.cov.life  <- data.cov.life[,  .SD, .SDcols = c(name.id, name.date, names.cov.life)]
  data.cov.trans <- data.cov.trans[, .SD, .SDcols = c(name.id, name.date, names.cov.trans)]

  # Cannot proceed if there are any NAs (conversion + if(), ..)
  if(anyNA(data.cov.life))
    err.msg <- c(err.msg, paste0("The Lifetime covariate data may not contain any NAs!"))
  if(anyNA(data.cov.trans))
    err.msg <- c(err.msg, paste0("The Transaction covariate data may not contain any NAs!"))
  check_err_msg(err.msg)

  setnames(data.cov.life,  old = name.id, new = "Id")
  setnames(data.cov.trans, old = name.id, new = "Id")
  setnames(data.cov.life,  old = name.date, new = "Cov.Date")
  setnames(data.cov.trans, old = name.date, new = "Cov.Date")

  data.cov.life[,  Id := .convert_userinput_dataid(id.data = Id)]
  data.cov.trans[, Id := .convert_userinput_dataid(id.data = Id)]

  data.cov.life[,  Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.data@clv.time, user.timepoint = Cov.Date)]
  data.cov.trans[, Cov.Date  := clv.time.convert.user.input.to.timepoint(clv.data@clv.time, user.timepoint = Cov.Date)]

  setkeyv(data.cov.life, cols = c("Id", "Cov.Date"))
  setkeyv(data.cov.trans, cols = c("Id", "Cov.Date"))

  # Required dates for cov datea -------------------------------------------------------------------------
  # Cut covariate data to range
  #   allows for more data than the required range
  #   speeds up tests

  # The maximum cov period is not fixed by holdout or estimation end but rather
  #   only needs to be at least this long. The user might supply longer cov data.
  #   It is then verified in the input checks (datadyncov_datadyncovspecific), that
  #   there are cov dates of the same length up to dt.required.dates[, max(Cov.Date)] for all users
  tp.max.cov.date <- max(clv.data@clv.time@timepoint.holdout.end,
                         clv.data@clv.time@timepoint.estimation.end,
                         data.cov.life[,  max(Cov.Date)],
                         data.cov.trans[, max(Cov.Date)])


  # all required covariate dates in range  (from floor_tu to floor_tu and spaced by time.unit)
  #   estimation.start is always required lower end
  dt.required.dates <- clv.time.sequence.of.covariate.timepoints(clv.time = clv.data@clv.time,
                                                                 tp.start = clv.data@clv.time@timepoint.estimation.start,
                                                                 tp.end   = tp.max.cov.date)

  # Cut to range, if needed
  #   Only timepoints lower than min(Cov.Date) are not needed and can be cut
  #   The dt.required.dates[, max(Cov.Date)] are the longest possible anyway (see
  #     definition of tp.max.cov.date), and no cov would be cut
  timepoint.cut.lower <- dt.required.dates[, min(Cov.Date)]

  if(data.cov.life[, any(Cov.Date < timepoint.cut.lower)]){

    # if(verbose)
    message("The Lifetime covariate data before ",timepoint.cut.lower," (period of estimation start) is cut off.")
    data.cov.life  <- data.cov.life[Cov.Date >= timepoint.cut.lower]
  }

  if(data.cov.trans[, any(Cov.Date < timepoint.cut.lower)]){

    # if(verbose)
    message("The Transaction covariate data before ",timepoint.cut.lower," (period of estimation start) is cut off.")
    data.cov.trans <- data.cov.trans[Cov.Date >= timepoint.cut.lower]
  }

  # should not be required, but be sure
  setkeyv(data.cov.life,  c("Id", "Cov.Date"))
  setkeyv(data.cov.trans, c("Id", "Cov.Date"))

  # Dyncov specific checks ---------------------------------------------------------------------------------------

  # Dynamic cov specific checks on covariate data
  #   only after if is DT because heavily relies on it for efficency
  #   only after Id is character because needed to compare to data.transaction Id
  #   only on the range to which it was cut
  dt.required.ids <- unique(clv.data@data.transactions[, "Id"])
  err.msg <- c(err.msg, check_userinput_datadyncov_datadyncovspecific(dt.data.dyn.cov = data.cov.life,
                                                                      clv.time = clv.data@clv.time,
                                                                      names.cov = names.cov.life,
                                                                      dt.required.dates = dt.required.dates,
                                                                      dt.required.ids = dt.required.ids,
                                                                      name.of.covariate = "Lifetime"))

  err.msg <- c(err.msg, check_userinput_datadyncov_datadyncovspecific(dt.data.dyn.cov = data.cov.trans,
                                                                      clv.time = clv.data@clv.time,
                                                                      names.cov = names.cov.trans,
                                                                      dt.required.dates = dt.required.dates,
                                                                      dt.required.ids = dt.required.ids,
                                                                      name.of.covariate = "Transaction"))
  check_err_msg(err.msg)
  # All checks passed



  # Convert the covariate data to dummies ---------------------------------------------------------------
  #   keep numbers, char/factors to dummies
  l.covs.life  <- convert_userinput_covariatedata(dt.cov.data = data.cov.life,  names.cov=names.cov.life)
  l.covs.trans <- convert_userinput_covariatedata(dt.cov.data = data.cov.trans, names.cov=names.cov.trans)

  # The cov names now are different because of the dummies!
  data.cov.life  <- l.covs.life$data.cov
  data.cov.trans <- l.covs.trans$data.cov
  names.cov.life  <- l.covs.life$final.names.cov
  names.cov.trans <- l.covs.trans$final.names.cov

  setkeyv(data.cov.life,  cols = c("Id", "Cov.Date"))
  setkeyv(data.cov.trans, cols = c("Id", "Cov.Date"))

  # Create and return dyncov data object -----------------------------------------------------------------
  return(clv.data.dynamic.covariates(no.cov.obj = clv.data,
                                     data.cov.life = data.cov.life,
                                     data.cov.trans = data.cov.trans,
                                     names.cov.data.life = names.cov.life,
                                     names.cov.data.trans = names.cov.trans))
}
