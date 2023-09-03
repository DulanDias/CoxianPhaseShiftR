
coxianCdf <- function(t, lambda, mu) {
  integrate(coxian_pdf, -Inf, t, lambda = lambda, mu = mu)$value
}
