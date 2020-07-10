#' @importFrom graphics plot
#' @importFrom ggplot2 ggplot geom_density aes
#' @method plot clv.fitted.spending
#' @export
plot.clv.fitted.spending <- function (x, verbose=TRUE, plot.interpolation.points = 250,...) {
   # Check inputs -----------------------------------------------------------------------------------------------------
   err.msg <- c()

   clv.fitted <- x

   # Gather actual mean spending data by customer ---------------------------------------------------------------------
   dt.customer.mean.spending <- clv.fitted@cbs[x>0, c("x", "Spending")]

   # Plot customer's mean spending as density -------------------------------------------------------------------------
   p <- ggplot(data = dt.customer.mean.spending) + geom_density(mapping = aes(Spending))

   # Overlay plot with model pdf function -----------------------------------------------------------------------------
   model.density <- function(x, clv.fitted){
      return(clv.model.probability.density(clv.model = clv.fitted@clv.model, x = x, clv.fitted = clv.fitted))
   }

   p <- p + stat_function(fun = model.density,
                           args = list(clv.fitted = clv.fitted),
                           colour = "red",
                           n = plot.interpolation.points)

   return(p)
}

#' @exportMethod plot
#' @export
#' @include class_clv_fitted_spending.R
setMethod("plot", signature(x="clv.fitted.spending"), definition = plot.clv.fitted.spending)
