#' @title Plot expected and actual spending density
#' @param x The fitted spending model to plot
#' @param plot.interpolation.points Number of interpolation points in density graph
#' @template template_param_verbose
#' @template template_param_dots
#'
#'
#' @description
#' Plot the spending density (actual vs. model-based)
#'
#' @return
#' An object of class \code{ggplot} from package \code{ggplot2} is returned by default.
#'
#' @examples
#' \donttest{
#'
#' library("CLVTools")
#' library("ggplot2")
#'
#' data("cdnow")
#'
#' clv.cdnow <- clvdata(cdnow,
#'   date.format="ymd",
#'   time.unit = "week",
#'   estimation.split = "1997-09-30",
#'   name.id = "Id",
#'   name.date = "Date",
#'   name.price = "Price")
#'
#' est.gg <- gg(clv.data = clv.cdnow, remove.first.transaction = FALSE)
#'
#' plot(est.gg, plot.interpolation.points = 1000)
#'
#' }
#'
#' @template template_references_gg
#'
#' @importFrom graphics plot
#' @importFrom ggplot2 ggplot aes geom_density labs theme scale_colour_manual guide_legend element_text element_rect element_blank element_line rel
#' @method plot clv.fitted.spending
#' @export
plot.clv.fitted.spending <- function (x, verbose=TRUE, plot.interpolation.points = 256,...) {
   Spending <- NULL

   # Check inputs -----------------------------------------------------------------------------------------------------
   err.msg <- c()

   clv.fitted <- x

   # Gather actual mean spending data by customer ---------------------------------------------------------------------
   dt.customer.mean.spending <- clv.fitted@cbs[x>0, c("x", "Spending")]

   # Plot customer's mean spending as density -------------------------------------------------------------------------
   p <- ggplot(data = dt.customer.mean.spending) + stat_density(mapping = aes(x = Spending, colour = "Average spending"), n = plot.interpolation.points, geom = "line")

   p <- p + geom_line(stat = "function",
                           mapping = aes(x = Spending, colour = clv.fitted@clv.model@name.model),
                           fun = clv.model.probability.density,
                           args = list(clv.model = clv.fitted@clv.model, clv.fitted = clv.fitted),
                           n = plot.interpolation.points,
                           na.rm = FALSE)

   # Add legend
   columns <- c("black", "red")
   setNames(columns, c("Average spending", clv.fitted@clv.model@name.model))

   p <- p + scale_colour_manual(name = "Legend", values = columns)

   # Axis and title
   p <- p + labs(x = "Spending", y= "Density", title= paste0("Spending density plot"),
                 subtitle = paste0("Estimation end: ",  clv.time.format.timepoint(clv.time=clv.fitted@clv.data@clv.time, timepoint=clv.fitted@clv.data@clv.time@timepoint.estimation.end)))

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