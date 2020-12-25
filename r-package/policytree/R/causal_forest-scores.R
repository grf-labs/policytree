#' @describeIn conditional_means Mean rewards \eqn{\mu} for control/treated
#' @method conditional_means causal_forest
#' @export
conditional_means.causal_forest <- function(object, ...) {
  tau.hat <- predict(object, ...)$predictions
  Y.hat.0 <- object$Y.hat - object$W.hat * tau.hat
  Y.hat.1 <- object$Y.hat + (1 - object$W.hat) * tau.hat

  cbind("control" = Y.hat.0, "treated" = Y.hat.1)
}


#' @describeIn double_robust_scores Scores \eqn{(\Gamma_0, \Gamma_1)}
#' @method double_robust_scores causal_forest
#' @export
double_robust_scores.causal_forest <- function(object, ...) {
  mu.matrix <- conditional_means(object, ...)
  W.hat.matrix <- cbind(1 - object$W.hat, object$W.hat) # [control, treated]
  n.obs <- nrow(W.hat.matrix)
  observed.treatment.idx <- cbind(1:n.obs, object$W.orig + 1)

  YY <- matrix(0, n.obs, 2)
  IPW <- matrix(0, n.obs, 2)
  YY[observed.treatment.idx] <- object$Y.orig
  IPW[observed.treatment.idx] <- 1 / W.hat.matrix[observed.treatment.idx]
  Gamma.matrix <- (YY - mu.matrix) * IPW + mu.matrix

  Gamma.matrix
}
