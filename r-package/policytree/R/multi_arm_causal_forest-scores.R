#' @describeIn conditional_means Mean rewards \eqn{\mu} for each treatment \eqn{a}
#' @param outcome Only used with multi arm causal forets. In the event the forest is trained
#'                with multiple outcomes Y, a column number/name specifying the outcome of interest.
#'                Default is 1.
#' @method conditional_means multi_arm_causal_forest
#' @export
conditional_means.multi_arm_causal_forest <- function(object, outcome = 1, ...) {
  outcome <- outcome[1]
  if (!outcome %in% 1:NCOL(object$Y.orig)) {
    outcome <- match(outcome, colnames(object$Y.orig, do.NULL = FALSE))
    if (is.na(outcome)) {
      stop("`outcome` should be a column number/name specifying a column in Y.")
    }
  } else {
    outcome <- as.integer(outcome)
  }
  Y.hat <- object$Y.hat[, outcome]
  W.hat <- object$W.hat[, , drop = FALSE]
  tau.hat.pointwise <- predict(object)$predictions[, , outcome]
  W.hat <- W.hat[, -1, drop = FALSE]
  Y.hat.baseline <- Y.hat - rowSums(W.hat * tau.hat.pointwise)
  mu.matrix <- cbind(Y.hat.baseline, Y.hat.baseline + tau.hat.pointwise)
  colnames(mu.matrix) <- levels(object$W.orig)

  mu.matrix
}


#' @describeIn double_robust_scores Matrix \eqn{\Gamma} of scores for each treatment \eqn{a}
#' @param outcome Only used with multi arm causal forets. In the event the forest is trained
#'                with multiple outcomes Y, a column number/name specifying the outcome of interest.
#'                Default is 1.
#' @method double_robust_scores multi_arm_causal_forest
#' @export
double_robust_scores.multi_arm_causal_forest <- function(object, outcome = 1, ...) {
  outcome <- outcome[1]
  if (!outcome %in% 1:NCOL(object$Y.orig)) {
    outcome <- match(outcome, colnames(object$Y.orig, do.NULL = FALSE))
    if (is.na(outcome)) {
      stop("`outcome` should be a column number/name specifying a column in Y.")
    }
  } else {
    outcome <- as.integer(outcome)
  }
  treatment.names <- levels(object$W.orig)
  num.treatments <- length(treatment.names)
  num.samples <- nrow(object$W.hat)
  observed.treatment <- match(object$W.orig, treatment.names)
  observed.treatment.idx <- cbind(seq_along(object$W.orig), observed.treatment)

  mu.matrix <- conditional_means(object, outcome)
  YY <- matrix(0, num.samples, num.treatments)
  IPW <- matrix(0, num.samples, num.treatments)
  YY[observed.treatment.idx] <- object$Y.orig[, outcome]
  IPW[observed.treatment.idx] <- 1 / object$W.hat[observed.treatment.idx]
  Gamma.matrix <- (YY - mu.matrix) * IPW + mu.matrix

  Gamma.matrix
}
