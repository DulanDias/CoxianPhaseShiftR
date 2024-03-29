% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coxianCdf.R
\name{coxianCdf}
\alias{coxianCdf}
\title{Compute the Cumulative Distribution Function (CDF) for a Coxian Distribution}
\usage{
coxianCdf(t, lambda, mu)
}
\arguments{
\item{t}{A numeric value where the CDF is evaluated.}

\item{lambda}{Numeric vector representing the transition rates.
The length of lambda determines the number of phases in the Coxian distribution.}

\item{mu}{Numeric vector representing the absorption rates for each phase.}
}
\value{
The value of the CDF of the Coxian distribution at t.
}
\description{
This function calculates the CDF of a Coxian distribution for a given value of t,
given transition rates (lambda) and absorption rates (mu).
}
\examples{
lambda_vec <- c(0.5, 0.3)
mu_vec <- c(0.2, 0.4)
coxianCdf(1, lambda_vec, mu_vec)

}
