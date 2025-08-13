#' @param date.format Character string that indicates the format of the date variable in the data used. See details.
#' @param time.unit What time unit defines a period. May be abbreviated, capitalization is ignored. See details.
#' @param observation.split The end of the observation period, beyond the last recorded transaction in \code{<%=name_param_trans%>}. See details.
#' @param estimation.split Indicates the length of the estimation period. See details.
#' @param name.id Column name of the customer id in \code{<%=name_param_trans%>}.
#' @param name.date Column name of the transaction date in \code{<%=name_param_trans%>}.
#' @param name.price Column name of price in \code{<%=name_param_trans%>}. NULL if no spending data is present.
