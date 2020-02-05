#' Estimate mean rewards \eqn{\mu} for each treatment \eqn{a}
#'
#' \eqn{\mu_a = m(x) + (1-e_a(x))\tau_a(x)}
#'
#' @param object An appropriate causal forest type object
#' @param ... Additional arguments
#'
#' @return A matrix of estimated mean rewards
#' @examples
#' \donttest{
#' n <- 500
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' W <- sample(c("A", "B", "C"), n, replace = TRUE)
#' Y <- X[, 1] + X[, 2] * (W == "B") + X[, 3] * (W == "C") + runif(n)
#' forests <- multi_causal_forest(X = X, Y = Y, W = W)
#' head(conditional_means(forests))
#' }
#' @importFrom stats predict
#' @export
conditional_means <- function(object, ...) {
  UseMethod("conditional_means")
}

#' Matrix \eqn{\Gamma} of scores for each treatment \eqn{a}
#'
#' Computes a matrix of double robust scores
#' \eqn{\Gamma_{ia} = \mu_a(x) + \frac{1}{e_a(x)} (Y_i - \mu_a(x)) 1(A_i=a)}
#'
#' This is the matrix used for CAIPWL (Cross-fitted Augmented Inverse Propensity Weighted Learning)
#'
#' @param object An appropriate causal forest type object
#' @param ... Additional arguments
#'
#' @return A matrix of scores for each treatment
#' @examples
#' \donttest{
#' n <- 500
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' W <- sample(c("A", "B", "C"), n, replace = TRUE)
#' Y <- X[, 1] + X[, 2] * (W == "B") + X[, 3] * (W == "C") + runif(n)
#' forests <- multi_causal_forest(X = X, Y = Y, W = W)
#' head(double_robust_scores(forests))
#' }
#' @export
double_robust_scores <- function(object, ...) {
  UseMethod("double_robust_scores")
}
