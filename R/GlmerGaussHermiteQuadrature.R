
#'
#' Approximate the population-average predictions from a
#' mixed-effects logistic regression  using Gauss-Hermite quadrature.
#'
#' The current implementation only supports a single random effect
#' on the intercept.
#'
#' @param merObject a glmerMod instance as produced by the glmer function
#' @param alpha the probability of type I error (alpha, by default set to 0.05)
#' @return a data.frame object
#'
#' @export
glmmPA <- function(merObject, alpha = 0.05) {
  if (!methods::is(merObject, "glmerMod")) {
    stop("The merObject parameter must be a glmerMod instance!")
  }
  omega <- stats::vcov(merObject) # extracting the estimated variance-covariance of beta hat
  X <- merObject@pp$X # extracting the design matrix of the fixed effects
  beta.hat <- merObject@beta
  randomStd <- merObject@theta
  s2u <- randomStd * randomStd
  xBeta <- X %*% beta.hat
  stdErrXBeta <- matrix(0, nrow=nrow(X), ncol=1)
  for (i in 1:nrow(X)) {
    stdErrXBeta[i,1] <- as.numeric(t(X[i,]) %*% omega %*% X[i,])
  }

  lowerXBeta <- xBeta - stats::qnorm(1 - alpha * .5) * stdErrXBeta
  upperXBeta <- xBeta + stats::qnorm(1 - alpha * .5) * stdErrXBeta

  w <- c(0.199532420590459E-1, 0.393619323152241, 0.945308720482942, 0.393619323152241, 0.199532420590459E-1)
  v <- c(-0.202018287045609E1, -0.958572464613819, 0, 0.958572464613819, 0.202018287045609E1)

  factor <- (2 * s2u)^.5
  piFactor <- 1/ pi^.5

  meanPA <- 0
  lowerMeanPA <- 0
  upperMeanPA <- 0
  linkFunction <- merObject@resp$family$linkinv
  for (i in 1:length(w)) {
    meanPA <- meanPA + w[i] * piFactor * linkFunction(xBeta + factor * v[i])
    lowerMeanPA <- lowerMeanPA + w[i] * piFactor * linkFunction(lowerXBeta + factor * v[i])
    upperMeanPA <- upperMeanPA + w[i] * piFactor * linkFunction(upperXBeta + factor * v[i])
  }

  return(data.frame(xBeta, fixedOnly = linkFunction(xBeta), meanPA, lowerMeanPA, upperMeanPA, alpha))
}
