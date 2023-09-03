#' Interpolate Density from Kernel Density Estimation
#'
#' This function interpolates the density value for a given point `x` based on
#' the provided kernel density estimation (KDE).
#'
#' @param x A numeric value or vector for which the density is to be interpolated.
#' @param kde A kernel density estimation object, typically obtained from the `density` function.
#'
#' @return A numeric value or vector representing the interpolated density at `x`.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' kde <- density(data_sample)
#' interpolate_density(0.5, kde)
#'
interpolate_density <- function(x, kde) {
  # Check if kde object has the necessary components
  if (!all(c("x", "y") %in% names(kde))) {
    stop("Invalid KDE object. It should have 'x' and 'y' components.")
  }

  # Handle values outside the range of the KDE
  out_of_range <- x < min(kde$x) | x > max(kde$x)
  if (any(out_of_range)) {
    if (length(x) == 1) {
      return(NA)
    } else {
      return(ifelse(out_of_range, NA, approx(x = kde$x, y = kde$y, xout = x)$y))
    }
  }

  approx(x = kde$x, y = kde$y, xout = x)$y
}
