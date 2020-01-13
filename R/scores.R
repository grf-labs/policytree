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
#' d <- 3
#' X <- matrix(runif(n * p), n, p)
#' Y <- runif(n)
#' W <- sample(1:d, n, replace = TRUE)
#' forests <- multi_causal_forest(X = X, Y = Y, W = W)
#' conditional_means(forests)
#' }
#' @importFrom stats predict
#' @keywords internal
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
#' d <- 3
#' X <- matrix(runif(n * p), n, p)
#' Y <- runif(n)
#' W <- sample(1:d, n, replace = TRUE)
#' forests <- multi_causal_forest(X = X, Y = Y, W = W)
#' double_robust_scores(forests)
#' }
#' @export
double_robust_scores <- function(object, ...) {
  UseMethod("double_robust_scores")
}
