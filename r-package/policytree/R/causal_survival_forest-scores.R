#' @describeIn conditional_means Mean rewards \eqn{\mu} for control/treated
#' @method conditional_means causal_survival_forest
#' @export
conditional_means.causal_survival_forest <- function(object, ...) {
  if (!all(object$W.orig %in% c(0, 1))) {
    stop("policytree currently only supports causal forest with binary treatment.")
  }
  tau.hat <- predict(object, ...)$predictions
  Y.hat.0 <- object$Y.hat - object$W.hat * tau.hat
  Y.hat.1 <- object$Y.hat + (1 - object$W.hat) * tau.hat

  cbind("control" = Y.hat.0, "treated" = Y.hat.1)
}


#' @describeIn double_robust_scores Scores \eqn{(\Gamma_0, \Gamma_1)}
#' @method double_robust_scores causal_survival_forest
#' @export
double_robust_scores.causal_survival_forest <- function(object, ...) {
  scores <- grf::get_scores(object)

  cbind("control" = -scores, "treated" = scores)
}
