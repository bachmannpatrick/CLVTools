#' @title Plot expected and actual mean spending per transaction
#' @param x The fitted spending model to plot
#' @param n Number of points at which the empirical and model density are calculated. Should be a power of two.
#' @template template_param_verbose
#' @template template_param_dots
#'
#'
#' @description
#' Compares the density of the observed average spending per transaction (empirical distribution) to the
#' model's distribution of mean transaction spending (weighted by the actual number of transactions).
#'
#' @seealso \code{\link[CLVTools:plot.clv.fitted.transactions]{plot}} for transaction models
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
plot.clv.fitted.spending <- function (x, n = 256, verbose=TRUE, ...) {
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
   p <- ggplot(data = dt.customer.mean.spending) + stat_density(mapping = aes(x = Spending, colour = "Actual Mean Value per Transaction"), n = n, geom = "line")

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

   # Axis and title
   p <- p + labs(x = "Average Value per Transaction", y= "Density", title= "Density of Average Transaction Value")

   p <- p + theme(
      plot.title = element_text(face = "bold", size = rel(1.5)),
      text = element_text(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.background  = element_rect(colour = NA),
      axis.title   = element_text(face = "bold",size = rel(1)),
      axis.title.y = element_text(angle=90,vjust =2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line = element_line(colour="black"),
      axis.ticks = element_line(),
      panel.grid.major = element_line(colour="#d2d2d2"),
      panel.grid.minor = element_blank(),
      legend.key = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face="italic"),
      strip.background=element_rect(colour="#d2d2d2",fill="#d2d2d2"),
      strip.text = element_text(face="bold", size = rel(0.8)))

   return(p)
}

#' @exportMethod plot
#' @include class_clv_fitted_spending.R
#' @rdname plot.clv.fitted.spending
setMethod("plot", signature(x="clv.fitted.spending"), definition = plot.clv.fitted.spending)
