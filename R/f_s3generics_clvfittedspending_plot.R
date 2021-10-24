#' @title Plot expected and actual mean spending per transaction
#' @param x The fitted spending model to plot
#' @param n Number of points at which the empirical and model density are calculated. Should be a power of two.
#' @template template_param_verbose
#' @template template_params_densityngeomdots
#'
#' @description
#' Compares the density of the observed average spending per transaction (empirical distribution) to the
#' model's distribution of mean transaction spending (weighted by the actual number of transactions).
#'
#' @seealso \code{\link[CLVTools:plot.clv.fitted.transactions]{plot}} for transaction models
#' @seealso \code{\link[CLVTools:plot.clv.data]{plot}} for transaction diagnostics of \code{clv.data} objects
#'
#' @return
#' An object of class \code{ggplot} from package \code{ggplot2} is returned by default.
#'
#' @examples
#' \donttest{
#' data("cdnow")
#'
#' clv.cdnow <- clvdata(cdnow,
#'   date.format="ymd",
#'   time.unit = "week",
#'   estimation.split = "1997-09-30")
#'
#' est.gg <- gg(clv.data = clv.cdnow)
#'
#' # Compare empirical to theoretical distribution
#' plot(est.gg)
#'
#' \dontrun{
#' # Modify the created plot further
#' library(ggplot2)
#' gg.cdnow <- plot(est.gg)
#' gg.cdnow + ggtitle("CDnow Spending Distribution")
#' }
#' }
#'
#' @template template_references_gg
#'
#' @importFrom graphics plot
#' @importFrom ggplot2 ggplot aes stat_density geom_line labs theme scale_colour_manual guide_legend element_text element_rect element_blank element_line rel
#' @method plot clv.fitted.spending
#' @export
plot.clv.fitted.spending <- function (x, n = 256, geom="line", verbose=TRUE, ...) {
   Spending <- NULL

   # Check inputs -----------------------------------------------------------------------------------------------------
   err.msg <- c()
   err.msg <- c(err.msg, check_user_data_emptyellipsis(...))
   err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))
   err.msg <- c(err.msg, .check_user_data_single_numeric(n=n, var.name="n"))
   check_err_msg(err.msg = err.msg)

   clv.fitted <- x


   # Plot customer's mean spending as density -------------------------------------------------------------------------
   dt.customer.mean.spending <- clv.fitted@cbs[x>0, "Spending"]
   p <- clv.data.make.density.plot(dt.data = dt.customer.mean.spending,
                                   mapping = aes(x = Spending, colour = "Actual Mean Value per Transaction"),
                                   labs_x = "Average Value per Transaction",
                                   title = "Density of Average Transaction Value",
                                   n = n, geom = geom, ...)

   # Overlay with model density ---------------------------------------------------------------------------------------
   p <- p + geom_line(stat = "function",
                      mapping = aes(x = Spending, colour = clv.fitted@clv.model@name.model),
                      fun = clv.model.probability.density,
                      args = list(clv.model = clv.fitted@clv.model, clv.fitted = clv.fitted),
                      n = n,
                      na.rm = FALSE)

   # Add legend
   columns <- setNames(c("black", "red"), c("Actual Mean Value per Transaction", clv.fitted@clv.model@name.model))
   p <- p + scale_colour_manual(name = "Legend", values = columns)

   return(p)
}

#' @exportMethod plot
#' @include class_clv_fitted_spending.R
#' @rdname plot.clv.fitted.spending
setMethod("plot", signature(x="clv.fitted.spending"), definition = plot.clv.fitted.spending)
