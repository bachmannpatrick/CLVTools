#' @importFrom optimx coef<-
setMethod(f = "clv.controlflow.estimate.put.optimx", signature = signature(obj="clv.fitted"),
          definition = function(obj, res.optimx){

  # save output (is of class "optimx" and "data.frame"), all attributes will be lost!
  #   Because for some reason S4 validity checks dont recognize res.optimx as a data.frame
  #   although it is one, as a workaround the class names are switched which makes it recognized
  # class(res.optimx) <- c("data.frame", "optimx")
  obj@optimx.estimation.output <- res.optimx

  optimx.last.row <- tail(obj@optimx.estimation.output, n=1)

  if(anyNA(coef(optimx.last.row)))
    warning("Estimation failed with NA coefs. The returened object contains results but further usage is restricted.",
            immediate. = TRUE, call. = FALSE)

  # extract hessian from "details" attribute which is a list (if more then 1 method given)
  #   name it the same as the coefs for reading out later on
  obj@optimx.hessian           <- as.matrix(tail(attr(optimx.last.row, "details")[, "nhatend"], n=1)[[1]])

  if(length(obj@optimx.hessian)==1 & all(is.na(obj@optimx.hessian))){
    obj@optimx.hessian <- matrix(data = NA_real_, nrow = ncol(coef(optimx.last.row)),
                                 ncol = ncol(coef(optimx.last.row)))
    warning("Hessian could not be derived. Setting all entries to NA.",
            call. = FALSE, immediate. = TRUE)
  }

  colnames(obj@optimx.hessian) <- rownames(obj@optimx.hessian) <- colnames(tail(coef(res.optimx), n=1))

  return(obj)
})
