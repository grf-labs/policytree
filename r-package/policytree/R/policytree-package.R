#' @description
#' A package for learning simple rule-based policies, where the rule takes the form of a shallow decision tree. Applications include settings which require interpretable predictions, such as for example a medical treatment prescription. This package uses doubly robust reward estimates from `grf` to find a shallow, but globally optimal decision tree.
#'
#' Some helpful links for getting started:
#'
#' * The R package documentation contains usage examples and method references (\url{https://grf-labs.github.io/policytree/}).
#' * For community questions and answers around usage, see the GitHub issues page (\url{https://github.com/grf-labs/policytree/issues}).
#'
#' @examples
#' \donttest{
#' # Multi-action policy learning.
#' n <- 250
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' W <- as.factor(sample(c("A", "B", "C"), n, replace = TRUE))
#' Y <- X[, 1] + X[, 2] * (W == "B") + X[, 3] * (W == "C") + runif(n)
#' multi.forest <- grf::multi_arm_causal_forest(X, Y, W)
#'
#' # Compute doubly robust reward estimates.
#' Gamma.matrix <- double_robust_scores(multi.forest)
#'
#' # Fit a depth 2 tree on a random training subset.
#' train <- sample(1:n, 200)
#' opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
#' opt.tree
#'
#' # Predict treatment on held out data.
#' predict(opt.tree, X[-train, ])
#' }
#'
#' @useDynLib policytree
#' @importFrom Rcpp evalCpp
#' @importFrom stats predict
#' @importFrom stats rbinom rnorm runif
#' @keywords internal
"_PACKAGE"
