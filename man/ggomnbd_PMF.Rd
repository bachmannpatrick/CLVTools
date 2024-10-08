% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{ggomnbd_PMF}
\alias{ggomnbd_PMF}
\alias{ggomnbd_nocov_PMF}
\alias{ggomnbd_staticcov_PMF}
\title{GGompertz/NBD: Probability Mass Function (PMF)}
\usage{
ggomnbd_nocov_PMF(r, alpha_0, b, s, beta_0, x, vT_i)

ggomnbd_staticcov_PMF(
  r,
  alpha_0,
  b,
  s,
  beta_0,
  x,
  vCovParams_trans,
  vCovParams_life,
  mCov_life,
  mCov_trans,
  vT_i
)
}
\arguments{
\item{r}{shape parameter of the Gamma distribution of the purchase process.
The smaller r, the stronger the heterogeneity of the purchase process.}

\item{alpha_0}{scale parameter of the Gamma distribution of the purchase process.}

\item{b}{scale parameter of the Gompertz distribution (constant across customers)}

\item{s}{shape parameter of the Gamma distribution for the lifetime process
The smaller s, the stronger the heterogeneity of customer lifetimes.}

\item{beta_0}{scale parameter for the Gamma distribution for the lifetime process}

\item{x}{The number of transactions to calculate the probability for (unsigned integer).}

\item{vT_i}{Number of periods since the customer came alive.}

\item{vCovParams_trans}{Vector of estimated parameters for the transaction covariates.}

\item{vCovParams_life}{Vector of estimated parameters for the lifetime covariates.}

\item{mCov_life}{Matrix containing the covariates data affecting the lifetime process. One column for each covariate.}

\item{mCov_trans}{Matrix containing the covariates data affecting the transaction process. One column for each covariate.}
}
\value{
Returns a vector of probabilities.
}
\description{
Calculate P(X(t)=x), the probability that a randomly selected customer
makes exactly x transactions in the interval (0, t].
}
\details{
\code{mCov_trans} is a matrix containing the covariates data of
the time-invariant covariates that affect the transaction process.
Each column represents a different covariate. For every column a gamma parameter
needs to added to \code{vCovParams_trans} at the respective position.

\code{mCov_life} is a matrix containing the covariates data of
the time-invariant covariates that affect the lifetime process.
Each column represents a different covariate. For every column a gamma parameter
needs to added to \code{vCovParams_life} at the respective position.
}
\references{
Bemmaor AC, Glady N (2012). \dQuote{Modeling Purchasing Behavior with Sudden \dQuote{Death}: A Flexible Customer
Lifetime Model} Management Science, 58(5), 1012-1021.

Adler J (2022).  \dQuote{Comment on “Modeling Purchasing Behavior with Sudden \dQuote{Death}: A Flexible Customer
Lifetime Model} Management Science 69(3):1929-1930.

The expression for the PMF was derived by Adler J (2024). (unpublished)
}
