#' Plot Histogram with Coxian PDF Overlay
#'
#' Plots a histogram of the observed data and overlays the Coxian phase type distribution's PDF.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values for the Coxian distribution.
#' @param mu A numeric vector of mu values for the Coxian distribution.
#' @param breaks The number of breaks for the histogram. Default is 30.
#' @param main The title of the plot. Default is "Fit of Coxian PH".
#' @param xlab The x-axis label. Default is "Value".
#' @param ylab The y-axis label. Default is "Density".
#' @param curve_col The color of the Coxian PDF curve. Default is "blue".
#' @param curve_lwd The line width of the Coxian PDF curve. Default is 2.
#'
#' @return A plot showing the histogram with the Coxian PDF overlay.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' plotHistogramPdf(data_sample, lambda, mu, main = "Custom Title", curve_col = "red")
#'
#' @export
plotHistogramPdf <- function(data, lambda, mu, breaks = 30, main = "Fit of Coxian PH",
                             xlab = "Value", ylab = "Density", curve_col = "blue", curve_lwd = 2) {
  hist(data, probability = TRUE, breaks = breaks, main = main, xlab = xlab, ylab = ylab)
  curve(coxian_pdf(x, lambda, mu), add = TRUE, col = curve_col, lwd = curve_lwd)
}
