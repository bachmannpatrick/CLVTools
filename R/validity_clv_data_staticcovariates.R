setValidity(Class = "clv.data.static.covariates", method = function(object){
    # do not callNext

    # Unfortunately, these validity checks here are also executed for subclasses, such as for dynamic covariates
    #   the id columns that identify a unique covariate, differ:
    #     static cov - Id, dynamic cov - Id, Cov.Date
    # One test cannot be adapted and is therefore only executed for static cov

    err.msg <- c()
    # *** STILL FAILS BECAUSE OF new(data.static.cov) in new dyncov!
    return(T)

    if(is(object, "clv.data.dynamic.covariates"))
      names.id.cols <- c("Id", "Cov.Date")
    else
      names.id.cols <- "Id"

    message("names.id.cols: ",names.id.cols)
    message("name: ", object@name)

    # data.cov.life ------------------------------------------------------------------------------------

    if(ncol(object@data.cov.life) < 2)
      err.msg <- c(err.msg, "There needs to be at least a column Id and one containing covariate data in data.cov.life")

    if(!"Id" %in% colnames(object@data.cov.life))
      err.msg <- c(err.msg, "There needs to be a column named Id in data.cov.life")

    if(!"Id" %in% key(object@data.cov.life))
      err.msg <- c(err.msg, "Column Id in data.cov.life needs to be key")

    if(!object@data.cov.life[, all(sapply(.SD, is.character)), .SDcols="Id"])
      err.msg <- c(err.msg, "Column Id in data.cov.life needs to be of class character")


    # Only if not dyncov
    if(!is(object, "clv.data.dynamic.covariates"))
      if(object@data.cov.life[, uniqueN(.SD), .SDcols="Id"] != nrow(object@data.transactions[,unique(.SD), .SDcols="Id"]))
        err.msg <- c(err.msg, "There needs to be an Id in data.cov.life for every customer.")

    if(object@data.cov.life[, uniqueN(.SD), .SDcols="Id"] != nrow(object@data.cov.life))
      err.msg <- c(err.msg, "There may be only one covariate row in data.cov.life for every customer.")

    if(uniqueN(colnames(object@data.cov.life)) != length(colnames(object@data.cov.life)))
      err.msg <- c(err.msg, "Columns in data.cov.life must all have distinct names")

    if(!all(colnames(object@data.cov.life) %in% c(names.id.cols, object@names.cov.data.life)))
      err.msg <- c(err.msg, "Columns in data.cov.life must be named after covariates (+Id)")

    if(object@data.cov.life[, sum(sapply(.SD, is.numeric)), .SDcols = !names.id.cols] != ncol(object@data.cov.life)-1)
      err.msg <- c(err.msg, "Columns in data.cov.life may only be numeric!")

    # data.cov.trans ------------------------------------------------------------------------------------
    if(ncol(object@data.cov.trans) < 2)
      err.msg <- c(err.msg, "There needs to be at least a column Id and one containing covariate data in data.cov.life")

    if(!"Id" %in% colnames(object@data.cov.trans))
      err.msg <- c(err.msg, "There needs to be a column named Id in data.cov.trans")

    if(!"Id" %in% key(object@data.cov.trans))
      err.msg <- c(err.msg, "Column Id in data.cov.trans needs to be key")

    if(!object@data.cov.trans[, all(sapply(.SD, is.character)), .SDcols="Id"])
      err.msg <- c(err.msg, "Column Id in data.cov.trans needs to be of class character")

    if(object@data.cov.trans[, uniqueN(.SD), .SDcols="Id"] != nrow(object@data.transactions[,unique(.SD), .SDcols="Id"]))
      err.msg <- c(err.msg, "There needs to be an Id in data.cov.trans for every customer.")

    # Only if not dyncov
    if(!is(object, "clv.data.dynamic.covariates"))
      if(object@data.cov.trans[, uniqueN(.SD), .SDcols="Id"] != nrow(object@data.cov.trans))
        err.msg <- c(err.msg, "There may be only one covariate row in data.cov.trans for every customer.")

    if(uniqueN(colnames(object@data.cov.trans)) != length(colnames(object@data.cov.trans)))
      err.msg <- c(err.msg, "Columns in data.cov.trans must all have distinct names")

    if(!all(colnames(object@data.cov.trans) %in% c(names.id.cols, object@names.cov.data.trans)))
      err.msg <- c(err.msg, "Columns in data.cov.trans must be named after covariates (+Id)")

    if(object@data.cov.trans[, sum(sapply(.SD, is.numeric)), .SDcols = !names.id.cols] != ncol(object@data.cov.trans)-1)
      err.msg <- c(err.msg, "Columns in data.cov.trans may only be numeric!")

    # names.cov.data.life -------------------------------------------------------------------------------
    if(!all(object@names.cov.data.life %in% colnames(object@data.cov.life)))
      err.msg <- c(err.msg, "All names.cov.data.life have to be column names in data.cov.life")

    # names.cov.data.trans -------------------------------------------------------------------------------
    if(!all(object@names.cov.data.trans %in% colnames(object@data.cov.trans)))
      err.msg <- c(err.msg, "All names.cov.data.trans have to be column names in data.cov.trans")

    if(length(err.msg)>0)
      return(err.msg)
    else
      return(TRUE)
})
