pmf.clv.fitted.transactions <- function(object, x=0:5){
  return(clv.template.controlflow.pmf(clv.fitted=object, x=x, plot=plot))
}

#' @examples {
#' library("CLVTools")
#' data("cdnow")
#'
#'  clv.cdnow <- clvdata(cdnow,
#'                       date.format="ymd",
#'                       time.unit = "week",
#'                       estimation.split = "1997-09-30",
#'                       name.id = "Id",
#'                       name.date = "Date",
#'                      name.price = "Price")
#'
#'  est.pnbd <- pnbd(clv.data = clv.cdnow, start.params.model = c(r = 1, alpha = 1, s= 1, beta = 1),
#'                   optimx.args = list(control=list(trace=5) ))
#'
#'
#'
#'  res <- pmf(clv.fitted = est.pnbd, x = 1:10, plot = FALSE)
#'
#' }
#'
#' Results vs. CBS:
#'
#' x    pmf             actual percentage of x
#' 1    0.163130227     439/2357 = 0.18625371
#' 2    0.079697124     214/2357 = 0.09079338
#' 3    0.046111155     100/2357 = 0.04242681
#' 4    0.028749294     62/2357  = 0.02630462
#' 5    0.018591868     38/2357  = 0.01612219
#' 6    0.012213620     29/2357  = 0.01230378
#' 7    0.008029474     23/2357  = 0.00975817
#' 8    0.005206442     7/2357   = 0.00296988
#' 9    0.003266914     5/2357   = 0.00212134
#' 10   0.001919501     5/2357   = 0.00212134
#'
#' @exportMethod pmf
setMethod(f = "pmf", signature = signature(object="clv.fitted.transactions"), definition = pmf.clv.fitted.transactions)
