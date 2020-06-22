#' @describeIn conditional_means Mean rewards \eqn{\mu} for each treatment \eqn{a}
#' @method conditional_means multi_causal_forest
#' @export
conditional_means.multi_causal_forest <- function(object, ...) {
  n.treatments <- ncol(object$W.hat)
  tau.hat <- predict(object, ...)$predictions
  mu.matrix <- object$Y.hat + (1 - object$W.hat) * tau.hat

  mu.matrix
}


#' @describeIn double_robust_scores Matrix \eqn{\Gamma} of scores for each treatment \eqn{a}
#' @method double_robust_scores multi_causal_forest
#' @export
double_robust_scores.multi_causal_forest <- function(object, ...) {
  mu.matrix <- conditional_means(object, ...)
  n.obs <- nrow(object$W.hat)
  n.treatments <- ncol(object$W.hat)
  treatment.names <- colnames(object$W.hat)

  # The treatment data structures are ordered columnwise from 1..n.treatments,
  # but the original W vector may be encoded arbitrarily (eg "0, 1, 2" , "A, B, C", etc.)
  observed.treatment <- match(object$W.orig, treatment.names)
  observed.treatment.idx <- cbind(1:n.obs, observed.treatment)

  YY <- matrix(0, n.obs, n.treatments)
  IPW <- matrix(0, n.obs, n.treatments)
  YY[observed.treatment.idx] <- object$Y.orig
  IPW[observed.treatment.idx] <- 1 / object$W.hat[observed.treatment.idx]
  Gamma.matrix <- (YY - mu.matrix) * IPW + mu.matrix

  Gamma.matrix
}
