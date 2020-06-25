#' @importFrom graphics plot
#' @importFrom ggplot2 ggplot geom_density aes
#' @method plot clv.fitted.spending
#' @export
plot.clv.fitted.spending <- function (x, newdata=NULL, verbose=TRUE,...) {
  # Check inputs -----------------------------------------------------------------------------------------------------
  err.msg <- c()

  # Gather actual mean spending data by customer ---------------------------------------------------------------------
  dt.customer.mean.spending <- x@cbs[, c("x", "Spending")]

  # Plot customer's mean spending as density -------------------------------------------------------------------------
  p <- ggplot(data = dt.customer.mean.spending) + geom_density(mapping = aes(Spending))

  return(p)

  # # Overlay plot with model pdf function -----------------------------------------------------------------------------
  #
  # clv.model.probability.density()
  #
  # y <- seq(dt.customer.mean.spending[, max(Spending)])
  # gg.density <- function(zbar, p, q, gam, x){
  #   a1 <- gammaln(p*x+q)-gammaln(p*x)-gammaln(q)
  #   a2 <- q*log(gam)
  #   a3 <- (p*x-1)*log(zbar)
  #   a4 <- (p*x)*log(x)
  #   a5 <- (p*x+q)*log(gam+x*zbar)
  #   g1 <- exp(a1+a2+a3+a4-a5)
  #   g1*this_zbar_nx / sum(nx)
  # }
  #
  # gg.coefs <- coef(clv.fitted.spending)
  #
  # p <- p + stat_function(fun = gg.density,
  #                        args = list(p = gg.coefs["p"], q = gg.coefs["q"], gamma = gg.coefs["gamma"]))
  #


}

#' @exportMethod plot
#' @export
#' @include class_clv_fitted_spending.R
setMethod("plot", signature(x="clv.fitted.spending"), definition = plot.clv.fitted.spending)
