#' Coerce to a <%=name_data_text%>
#'
#' Extract a copy of the transaction data stored in the given clv.data object
#' into a <%=name_data_code%>.
#'
#' @param x An object of class \code{clv.data}.
#' @param Ids Character vector of customer ids for which transactions should be extracted. \code{NULL} extracts all.
#' @param sample Name of sample for which transactions should be extracted,
#' either "estimation", "holdout", or "full" (default).
#'
#' @return
#' A \code{<%=name_data_code%>} with columns \code{Id}, \code{Date}, and \code{Price} (if present).
#'
#' @examples
#'
#' data("cdnow")
#' clv.data.cdnow <- clvdata(data.transactions = cdnow,
#'                           date.format="ymd",
#'                           time.unit = "w",
#'                           estimation.split = 37)
#'
#' # Extract all transaction data (all Ids, estimation and holdout period)
#' <%=name_res%> <- as.<%=name_data_code%>(clv.data.cdnow)
#'
#' # Extract transaction data of estimation period
#' <%=name_res%> <- as.<%=name_data_code%>(clv.data.cdnow, sample="estimation")
#'
#' # Extract transaction data of Ids "1", "2", and "999"
#' #  (estimation and holdout period)
#' <%=name_res%> <- as.<%=name_data_code%>(clv.data.cdnow, Ids = c("1", "2", "999"))
#'
#' # Extract transaction data of Ids "1", "2", and "999" in estimation period
#' <%=name_res%> <- as.<%=name_data_code%>(clv.data.cdnow, Ids = c("1", "2", "999"),
#'                           sample="estimation")
#'
