#' @title <%=name_model_full%>: Probability Mass Function (PMF)
#'
#' @description Calculate P(X(t)=x), the probability that a randomly selected customer
#' makes exactly x transactions in the interval (0, t].
#'
#' @param x The number of transactions to calculate the probability for (unsigned integer).
#' @param vT_i Number of periods since the customer came alive.
#'
#' @return Returns a vector of probabilities.
#'
