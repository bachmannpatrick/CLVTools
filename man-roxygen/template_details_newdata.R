#' @details The \code{newdata} argument has to be a clv data object of the exact same class as the data object
#' on which the model was fit. In case the model was fit with covariates, \code{newdata} needs to contain identically
#' named covariate data.
#'
#' The use case for \code{newdata} is mainly two-fold: First, to estimate model parameters only on a
#' sample of the data and then use the fitted model object to predict or plot for the full data set provided through \code{newdata}.
#' Second, for models with dynamic covariates, to provide a clv data object with longer covariates than contained in the data
#' on which the model was estimated what allows to predict or plot further. When providing \code{newdata}, some models
#' might require additional steps that can significantly increase runtime.
