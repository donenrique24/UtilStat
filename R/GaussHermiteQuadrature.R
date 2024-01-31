#'
#' Compute Gauss-Hermite quadrature.
#'
#' The function relies on a five-point quadrature. It assumes the variable that follows the quadrature is
#' normally distribution with mean 0 and variance s2.
#'
#' @param f a function whose first argument is the normal deviate
#' @param s2 the variance of the normal deviate
#' @param ... other arguments to be passed to the function
#' @author Mathieu Fortin - January 2024
#'
#' @export
getHermiteQuadratureApproximation <- function(f, s2, ...) {
  if (!methods::is(f, "function")) {
    stop("The f argument must be a function whose argument is the quadrature point!")
  }
  w <- c(0.199532420590459E-1, 0.393619323152241, 0.945308720482942, 0.393619323152241, 0.199532420590459E-1)
  v <- c(-0.202018287045609E1, -0.958572464613819, 0, 0.958572464613819, 0.202018287045609E1)
  meanPA <- 0;
  factor <- (2 * s2) **.5;
  piFactor <- 1 / 3.14159 ** .5;
  for (i in 1:length(w)) {
    meanPA = meanPA + w[i] * piFactor * f(factor * v[i], ...);
  }
  return(meanPA)
}
