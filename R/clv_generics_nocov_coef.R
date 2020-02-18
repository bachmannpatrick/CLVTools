# Do not overwrite for model classes!
#' @importFrom stats coef
#' @importFrom optimx coef<-
#' @include class_clv_model_basestrategy.R class_clv_fitted.R all_generics.R
#' @export
coef.clv.fitted <- function(object, complete=TRUE, ...){

  last.row.optimx.coef <- tail(coef(object@optimx.estimation.output),n=1)

  # model params from clv.model (backtransform) --------------------------------------------------------
  #   Backtransform estimated model params from opimizer to original scale
  prefixed.params.model       <- last.row.optimx.coef[1, object@clv.model@names.prefixed.params.model,drop=TRUE]
  original.scale.model.params <- clv.model.backtransform.estimated.params.model(clv.model = object@clv.model,
                                                                                prefixed.params.model = prefixed.params.model)
  # Set original scale names and ensure order
  original.scale.model.params <- setNames(original.scale.model.params[object@clv.model@names.prefixed.params.model],
                                    object@clv.model@names.original.params.model)
  original.scale.params <- original.scale.model.params

  # Correlation param ---------------------------------------------------------------------------------------
  if(object@estimation.used.correlation & complete==TRUE){
    last.row.optimx.coef   <- tail(coef(object@optimx.estimation.output),n=1)
    param.m                <- last.row.optimx.coef[1, object@name.prefixed.cor.param.m, drop=TRUE]
    param.cor              <- clv.model.m.to.cor(clv.model = object@clv.model, prefixed.params.model=prefixed.params.model,
                                                 param.m = param.m)
    names(param.cor)       <- object@name.correlation.cor
    original.scale.params  <- c(original.scale.params, param.cor)
  }

  return(original.scale.params)
}


