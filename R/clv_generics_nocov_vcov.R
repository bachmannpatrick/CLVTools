#' @export
vcov.clv.fitted <- function(object, complete = TRUE, ...){

  if(any(!is.finite(object@optimx.hessian)))
    stop("The vcov matrix cannot be calulated because the hessian contains non-finite values!", call. = FALSE)

  # Get the vcov as the inverse of hessian
  #   log(thetahat) ~ N(theta,Hinv)!
  #
  #   Jeff:
  #   negative hessian or hessian?!
  #     -> Depends on optimization! If we have the loglikelihood we need the negative Hessian.
  #   However we have the negative (!) Log likelihood, so I guess we need to take the Hessian directly

  # Inverse of hessian
  #   try regular inverse first and only if fails use nearPD
  #   Do not use sechol() of package accuracy because not available on cran
  hessian.inv <- tryCatch(solve(object@optimx.hessian), error = function(e) return(e))
  if(inherits(hessian.inv, 'error')){
    warning("Failed to invert hessian, making it positive-definite first.")
    hessian.inv <<- solve(Matrix::nearPD(object@optimx.hessian)$mat)
  }

  # Apply Jeff's delta method to account for the transformations of the parameters
  #   See Jeff's Note on how to derive p-values
  #   The variances are for the transformed parameters (ie log-scale)

  # Get the numbers to put in diag() for back transformation from the model
  #   Jeff: Apply the transformation on optimizer-scale parameters
  prefixed.params  <- tail(coef(object@optimx.estimation.output), n=1)[1, ,drop = TRUE]
  m.delta.diag     <- clv.model.vcov.jacobi.diag(clv.model=object@clv.model, clv.fitted=object,
                                                 prefixed.params=prefixed.params)

  stopifnot(all(colnames(hessian.inv) == colnames(m.delta.diag)))
  stopifnot(all(rownames(hessian.inv) == rownames(m.delta.diag)))
  m.vcov <- m.delta.diag %*% hessian.inv %*% m.delta.diag

  # Naming and sorting
  #   Sorting:  Correct because directly from optimx hessian and for delta.diag from coef(optimx)
  #   Naming:   Has to match coef(). model + cor: original
  #                                  any cov:     leave prefixed
  #             Change the names of model+cor to display name because vcov now is in original scale as well

  # Set all names of vcov to these of hessian
  rownames(m.vcov) <- colnames(m.vcov) <- colnames(object@optimx.hessian)

  # prefixed names to replace with original names
  names.prefixed.all <- object@clv.model@names.prefixed.params.model
  names.original.all <- object@clv.model@names.original.params.model
  if(object@estimation.used.correlation){
    names.prefixed.all <- c(names.prefixed.all, object@name.prefixed.cor.param.m)
    names.original.all <- c(names.original.all, object@name.correlation.cor)
  }

  # position of these prefixed names
  pos.prefixed.names <- match(x = names.prefixed.all, table=rownames(m.vcov))
  # replace with original names
  rownames(m.vcov)[pos.prefixed.names] <- names.original.all
  colnames(m.vcov)[pos.prefixed.names] <- names.original.all

  # **TODO: cannot easily sort to same order as coef because
  #   optimx hessian is named after optimx names but report original display names
  # dimnames(hessian.inv) <- dimnames(object@optimx.hessian)
  # rownames(m.vcov) <- colnames(m.vcov) <- names(all.coefs)

  return(m.vcov)
}
