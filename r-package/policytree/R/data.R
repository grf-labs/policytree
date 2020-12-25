#' Example data generating process from Offline Multi-Action Policy Learning: Generalization and Optimization
#'
#' The DGP from section 6.4.1 in Zhou, Athey, and Wager (2018):
#' There are \eqn{d=3} actions \eqn{(a_0,a_1,a_2)} which depend
#' on 3 regions the covariates \eqn{X \sim U[0,1]^p} reside in. Observed outcomes:
#' \eqn{Y \sim N(\mu_{a_i}(X_i), 4)}
#'
#' @param n Number of observations \eqn{X}.
#' @param p Number of features (minimum 7). Default is 10.
#' @param sigma2 Noise variance. Default is 4.
#'
#' @return A list with realized action \eqn{a_i}, region \eqn{r_i},
#'  conditional mean \eqn{\mu}, outcome \eqn{Y} and covariates \eqn{X}
#' @references Zhou, Zhengyuan, Susan Athey, and Stefan Wager. "Offline multi-action policy learning:
#'  Generalization and optimization." arXiv preprint arXiv:1810.04778 (2018).
#' @export
#' @importFrom stats rnorm runif
gen_data_mapl <- function(n, p = 10, sigma2 = 4) {
  if (p < 7) {
    stop("This DGP requires more than 7 features. Supplied: ", p)
  }
  A <- c(0, 1, 2)
  probs <- cbind(
    c(0.2, 0.2, 0.4),
    c(0.6, 0.6, 0.2),
    c(0.2, 0.2, 0.4)
  )

  X <- matrix(runif(n * p), n, p)
  R0 <- (X[, 5] < 0.6) & (0.35 < X[, 7])
  R2 <- (X[, 5]^2 / 0.6^2 + X[, 7]^2 / 0.35^2 < 1) |
    ((X[, 5] - 1)^2 / 0.4^2 + (X[, 7] - 1)^2 / 0.35^2 < 1)
  R1 <- !(R0 | R2)
  R <- cbind(R0, R1, R2)
  region <- which(R, arr.ind = TRUE)
  region <- region[order(region[, "row"]), "col"] # `which` does not preserve order
  region <- region - 1

  rewards <- sapply(region, function(r) {
    actions <- 0:2
    if (r == 0) {
      return(3 - actions)
    } else if (r == 1) {
      return(2 - abs(actions - 1) / 2)
    } else {
      return(1.5 * (actions - 1))
    }
  })
  rewards <- t(rewards)
  colnames(rewards) <- 0:2
  realized.a <- sapply(region, function(x) {
    sample(A, 1, prob = probs[x + 1, ])
  })
  reward <- rewards[cbind(1:n, realized.a + 1)]
  Y <- rnorm(n, mean = reward, sd = sqrt(sigma2))

  list(action = realized.a, Y = Y, X = X, region = region, mu = reward, mu.all = rewards)
}


#' Example data generating process from Efficient Policy Learning
#'
#' The DGP from section 5.2 in Athey and Wager (2017)
#'
#' @param n Number of observations
#' @param type tau is "continuous" (default - equation 54) or exhibits "jumps" (equation 55)
#'
#' @return A list
#' @references Athey, Susan, and Stefan Wager. "Efficient policy learning." arXiv preprint arXiv:1702.02896 (2017).
#' @export
#' @importFrom stats rbinom
gen_data_epl <- function(n, type = c("continuous", "jump")) {
  p <- 10
  type <- match.arg(type)
  X <- matrix(rnorm(n * p), n, p)
  Z <- rbinom(n = n, size = 1, prob = 1 / (1 + exp(-X[, 3])))
  eps <- rnorm(n)
  Q <- rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eps - X[, 4])))
  W <- as.integer(Q & Z)
  if (type == "continuous") {
    tau <- (pmax(0, X[, 1]) + pmax(0, X[, 2]) - 1) / 2 # (54)
  } else {
    tau <- sign(X[, 1] * X[, 2]) / 2 # (55)
  }
  Y <- pmax(0, X[, 3] + X[, 4]) + W * tau + eps

  list(W = W, Z = Z, tau = tau, Y = Y, X = X)
}
