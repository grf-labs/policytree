#' Fit a policy with exact tree search
#'
#' Finds the optimal (maximizing the sum of rewards) depth L tree by exhaustive search. If the optimal
#' action is the same in both the left and right leaf of a node, the node is pruned.
#'
#' The amortized runtime of the exact tree search is \eqn{O(p^k n^k (log n + d) + pnlog n)} where p is the number of features, d the number of treatments, n the number of observations, and \eqn{k \geq 1} the tree depth.
#'
#' For a depth two tree this is \eqn{O(p^2 n^2 (log n + d))} (ignoring the last term which is a global sort done at the beginning) meaning that it scales quadratically with the number of observations, i.e. if you double the number of observations, the search will take at least four times as long.
#'
#' For a depth three tree it is \eqn{O(p^3 n^3 (log n + d))}. If a depth two tree with 1000 observations, 4 features and 3 actions took around t seconds, you can expect the level three tree to take approximately \eqn{1000\cdot 4} times as long (\eqn{\approx\frac{p^3n^2}{p^2n^2}=pn})
#'
#' The runtime above is with continuous features. There are considerable time savings when the features are
#' discrete. In the extreme case with all binary observations, the runtime will be practically linear in n.
#'
#' The optional approximation parameter `split.step` emulates rounding the data and is recommended to experiment with in order to reduce the runtime.
#'
#' @param X The covariates used. Dimension \eqn{Np} where \eqn{p} is the number of features.
#' @param Gamma The rewards for each action. Dimension \eqn{Nd} where \eqn{d} is the number of actions.
#' @param depth The depth of the fitted tree. Default is 2.
#' @param split.step An optional approximation parameter (integer above zero), the number of possible splits
#'  to consider when performing tree search. split.step = 1 (default) considers every possible split, split.step = 10
#'  considers splitting at every 10'th distinct value and will yield a substantial speedup for densely packed
#'  continuous data.
#'
#' @return A policy_tree object.
#'
#' @references Sverdrup, Erik, Ayush Kanodia, Zhengyuan Zhou, Susan Athey, and Stefan Wager.
#'  "policytree: Policy learning via doubly robust empirical welfare maximization over trees."
#'   Journal of Open Source Software 5, no. 50 (2020): 2232.
#' @references Zhou, Zhengyuan, Susan Athey, and Stefan Wager. "Offline multi-action policy learning:
#'  Generalization and optimization." arXiv preprint arXiv:1810.04778 (2018).
#'
#' @examples
#' \donttest{
#' # Fit a depth two tree on doubly robust treatment effect estimates
#' # from a causal forest.
#' n <- 10000
#' p <- 5
#' X <- round(matrix(rnorm(n * p), n, p), 2)
#' W <- rbinom(n, 1, 1 / (1 + exp(X[, 3])))
#' tau <- 1 / (1 + exp((X[, 1] + X[, 2]) / 2)) - 0.5
#' Y <- X[, 3] + W * tau + rnorm(n)
#' c.forest <- grf::causal_forest(X, Y, W)
#' dr.scores <- double_robust_scores(c.forest)
#'
#' tree <- policy_tree(X, dr.scores, 2)
#' tree
#'
#' # Predict treatment assignment.
#' predicted <- predict(tree, X)
#'
#' plot(X[, 1], X[, 2], col = predicted)
#' legend("topright", c("control", "treat"), col = c(1, 2), pch = 19)
#' abline(0, -1, lty = 2)
#' }
#' @export
#' @importFrom utils type.convert
policy_tree <- function(X, Gamma, depth = 2, split.step = 1) {
  n.features <- ncol(X)
  n.actions <- ncol(Gamma)
  n.obs <- nrow(X)
  valid.classes <- c("matrix", "data.frame")

  if (!inherits(X, valid.classes) || !inherits(Gamma, valid.classes)) {
    stop(paste("Currently the only supported data input types are:",
               "`matrix`, `data.frame`"))
  }
  if (!is.numeric(as.matrix(X))) {
    stop("The feature matrix X must be numeric")
  }
  if (!is.numeric(as.matrix(Gamma))) {
    stop("The reward matrix Gamma must be numeric")
  }
  if (any(is.na(X))) {
    stop("Covariate matrix X contains missing values.")
  }
  if (any(is.na(Gamma))) {
    stop("Gamma matrix contains missing values.")
  }
  if (depth < 0 ) {
    stop("`depth` cannot be negative.")
  }
  if (n.obs != nrow(Gamma)) {
    stop("X and Gamma does not have the same number of rows")
  }

  action.names <- colnames(Gamma)
  if (is.null(action.names)) {
    action.names <- as.character(1:ncol(Gamma))
  }
  action.names <- type.convert(action.names, as.is = TRUE) # TRUE to not convert character to factor
  columns <- colnames(X)
  if (is.null(columns)) {
    columns <- make.names(1:ncol(X))
  }

  result <- tree_search_rcpp(as.matrix(X), as.matrix(Gamma), depth, split.step)
  tree <- list(nodes = result[[1]])

  tree[["_tree_array"]] <- result[[2]]
  tree[["depth"]] <- depth
  tree[["n.actions"]] <- n.actions
  tree[["n.features"]] <- n.features
  tree[["action.names"]] <- action.names
  tree[["columns"]] <- columns
  class(tree) <- "policy_tree"

  tree
}

#' Predict method for policy_tree
#'
#' Predict values based on fitted policy_tree object.
#' @param object policy_tree object
#' @param newdata A data frame with features
#' @param ... Additional arguments (currently ignored).
#'
#' @return A vector of predictions. Each element is an integer from 1 to d where d is
#'  the number of columns in the reward matrix.
#' @export
#'
#' @method predict policy_tree
#' @examples
#' \donttest{
#' # Fit a depth two tree on doubly robust treatment effect estimates
#' # from a causal forest.
#' n <- 10000
#' p <- 5
#' X <- round(matrix(rnorm(n * p), n, p), 2)
#' W <- rbinom(n, 1, 1 / (1 + exp(X[, 3])))
#' tau <- 1 / (1 + exp((X[, 1] + X[, 2]) / 2)) - 0.5
#' Y <- X[, 3] + W * tau + rnorm(n)
#' c.forest <- grf::causal_forest(X, Y, W)
#' dr.scores <- double_robust_scores(c.forest)
#'
#' tree <- policy_tree(X, dr.scores, 2)
#' tree
#'
#' # Predict treatment assignment.
#' predicted <- predict(tree, X)
#'
#' plot(X[, 1], X[, 2], col = predicted)
#' legend("topright", c("control", "treat"), col = c(1, 2), pch = 19)
#' abline(0, -1, lty = 2)
#' }
predict.policy_tree <- function(object, newdata, ...) {
  valid.classes <- c("matrix", "data.frame")
  if (!inherits(newdata, valid.classes)) {
    stop(paste("Currently the only supported data input types are:",
               "`matrix`, `data.frame`"))
  }
  if (!is.numeric(as.matrix(newdata))) {
    stop("The feature matrix X must be numeric")
  }
  if (any(is.na(newdata))) {
    stop("Covariate matrix X contains missing values.")
  }

  tree <- object
  if (tree$n.features != ncol(newdata)) {
    stop("This tree was trained with ", tree$n.features, " variables. Provided: ", ncol(newdata))
  }

  tree_search_rcpp_predict(tree[["_tree_array"]], as.matrix(newdata))
}
