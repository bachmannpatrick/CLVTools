#' @include class_clv_model_basestrategy.R
setValidity(Class = "clv.model", method = function(object){
  err.msg <- c()
  # name.model ---------------------------------------------------------------------------
  if(length(object@name.model) != 1)
    err.msg <- c(err.msg, "The model may not specify more than 1 name!")

  if(nchar(object@name.model) == 0)
    err.msg <- c(err.msg, "The model needs to have a name!")

  if(anyNA(object@name.model))
    err.msg <- c(err.msg, "The model name may not contain any NA!")

  # names.original.params.model -----------------------------------------------------------
  if(length(object@names.original.params.model) == 0)
    err.msg <- c(err.msg, "names.original.params.model needs to contain at least a single parameter name!")

  if(length(object@names.original.params.model) != length(object@names.prefixed.params.model))
    err.msg <- c(err.msg, "names.original.params.model and names.prefixed.params.model need to be of same length!")

  # names same as content - not needed because name never used

  # names.prefixed.params.model ------------------------------------------------------------
  if(length(object@names.prefixed.params.model) == 0)
    err.msg <- c(err.msg, "names.prefixed.params.model needs to contain at least a single parameter name!")

  if(length(object@names.prefixed.params.model) != length(object@start.params.model))
    err.msg <- c(err.msg, "names.original.params.model and start.params.model need to be of same length!")

  # names same as content - not needed because name never used

  # start.params.model ---------------------------------------------------------------------
  if(length(object@start.params.model) == 0)
    err.msg <- c(err.msg, "start.params.model needs to contain at least a single start parameter!")

  if(!all(names(object@start.params.model)  %in% object@names.original.params.model))
    err.msg <- c(err.msg, "start.params.model need to be named after original names!")

  if(!all(object@names.original.params.model %in% names(object@start.params.model)))
    err.msg <- c(err.msg, "There need to be start params for every parameter!")

  # optimx.defaults ------------------------------------------------------------------------
  if(!all(names(object@optimx.defaults) %in% formalArgs(getFromNamespace("optimx", ns="optimx"))))
    err.msg <- c(err.msg, "optimx.defaults may only contain elements that are inputs to optimx()")

  if(length(err.msg)>0)
    return(err.msg)
  else
    return(TRUE)
})
