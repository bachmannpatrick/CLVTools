#' @name clv.init.new.no.cov.model.object
#' @title Init new object of a no covariate model
#' @param new.model.obj Newly created object to be initialized
#' @template template_addtransactions
#' @description Function used to properly create a new object of a NO covariate model.
#' The object itself needs to be created with new and then passed as new.model.obj.
#' The slots of the base object will be first initilaized and then the object is dispatched to the model-specific init functions to set the model-specific slots.
#' Transaction data is processed and added as part of the base model initialization, no more proceessing should be needed for the model specifc initialization.
#' @details new.model.obj needs to be a new empty object created with new()
#' @keywords internal
