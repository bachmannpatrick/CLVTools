#' @examples \donttest{
#' data("apparelTrans")
#' clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd",
#'                             time.unit = "w", estimation.split = 40)
#'
#' # Fit the <%=name_model_short%> model
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
#' # summary of the fitted model
#' summary(apparel.<%=name_model_short%>)
#'
#' # Plot model vs empirical distribution
#' plot(apparel.<%=name_model_short%>)
#'
#' # predict mean spending and compare against
#' #    actuals in the holdout period
#' predict(apparel.<%=name_model_short%>)
#' }


