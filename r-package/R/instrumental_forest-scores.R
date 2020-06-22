#' @describeIn conditional_means Mean rewards \eqn{\mu} for control/treated
#' @method conditional_means instrumental_forest
#' @export
conditional_means.instrumental_forest <- function(object, ...) {
  tau.hat <- predict(object, ...)$predictions
  Y.hat.0 <- object$Y.hat - object$W.hat * tau.hat
  Y.hat.1 <- object$Y.hat + (1 - object$W.hat) * tau.hat

  cbind("control" = Y.hat.0, "treated" = Y.hat.1)
}


#' @describeIn double_robust_scores Scores \eqn{(-\Gamma, \Gamma)}
#' @note For instrumental_forest this method returns \eqn{(-\Gamma_i, \Gamma_i)} where \eqn{\Gamma_i}
#'  is the double robust estimator of the treatment effect as in eqn. (52) in Athey and Wager (2017).
#'
#' @param compliance.score An estimate of the causal effect of Z on W.
#'  i.e., Delta(X) = E(W | X, Z = 1) - E(W | X, Z = 0), for each sample i = 1, ..., n. If NULL (default)
#'  then this is estimated with a causal forest.
#'
#' @references Athey, Susan, and Stefan Wager. "Efficient policy learning." arXiv preprint arXiv:1702.02896 (2017).
#'
#' @method double_robust_scores instrumental_forest
#' @export
double_robust_scores.instrumental_forest <- function(object, compliance.score = NULL, ...) {
  if (is.null(compliance.score)) {
    compliance.forest <- grf::causal_forest(
      X = object$X.orig,
      Y = object$W.orig,
      W = object$Z.orig,
      Y.hat = object$W.hat,
      W.hat = object$Z.hat
    )
    compliance.score <- predict(compliance.forest)$predictions
  }

  Z.orig <- object$Z.orig
  Z.hat <- object$Z.hat
  Y.orig <- object$Y.orig
  Y.hat <- object$Y.hat
  W.orig <- object$W.orig
  W.hat <- object$W.hat
  tau.hat <- predict(object)$predictions

  g.hat <- 1 / compliance.score * (Z.orig - Z.hat) / (Z.hat * (1 - Z.hat))
  dr.correction <- g.hat * (Y.orig - Y.hat - (W.orig - W.hat) * tau.hat)

  gamma <- tau.hat + dr.correction

  cbind("control" = -gamma, "treated" = gamma)
}
