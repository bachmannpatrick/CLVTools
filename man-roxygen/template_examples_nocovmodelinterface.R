#' @examples \donttest{
#' data("apparelTrans")
#' clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd",
#'                             time.unit = "w", estimation.split = 52)
#'
#' # Fit standard <%=name_model_short%> model
#' <%=name_model_short%>(clv.data.apparel)
#'
#' # Give initial guesses for the model parameters
#' <%=name_model_short%>(clv.data.apparel,
#'      start.params.model = <%=vec_startparams_model%>)
#'
#'
#' # pass additional parameters to the optimizer (optimx)
#' #    Use Nelder-Mead as optimization method and print
#' #    detailed information about the optimization process
#' apparel.<%=name_model_short%> <- <%=name_model_short%>(clv.data.apparel,
#'                      optimx.args = list(method="Nelder-Mead",
#'                                         control=list(trace=6)))
#'
#' # estimated coefs
#' coef(apparel.<%=name_model_short%>)
#'
#' # summary of the fitted model.
#' # Note that the significance indicators are set to NA on purpose because all
#' # model parameters are by definition strictly positive. A hypothesis test
#' # relative to a null of 0 therefore does not make sense.
#' summary(apparel.<%=name_model_short%>)
#'
#' # predict CLV etc for holdout period
#' predict(apparel.<%=name_model_short%>)
#'
#' # predict CLV etc for the next 15 periods
#' predict(apparel.<%=name_model_short%>, prediction.end = 15)
#' }


