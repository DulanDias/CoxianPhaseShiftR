
coxianCdf <- function(t, lambda, mu) {
  integrate(coxianPdf, -Inf, t, lambda = lambda, mu = mu)$value
}
