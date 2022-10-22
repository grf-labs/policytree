#' Fit a policy with exact tree search
#'
#' Finds the optimal (maximizing the sum of rewards) depth k tree by exhaustive search. If the optimal
#' action is the same in both the left and right leaf of a node, the node is pruned.
#'
#' Exact tree search is intended as a way to find shallow (i.e. depth 2 or 3) globally optimal
#' tree-based polices on datasets of "moderate" size.
#'
#' @param X The covariates used. Dimension \eqn{N*p} where \eqn{p} is the number of features.
#' @param Gamma1 The rewards for each action. Dimension \eqn{N*d} where \eqn{d} is the number of actions.
#' @param Gamma2 The rewards for each action. Dimension \eqn{N*d} where \eqn{d} is the number of actions.
#' @param depth The depth of the fitted tree. Default is 2.
#' @param split.step An optional approximation parameter, the number of possible splits
#'  to consider when performing tree search. split.step = 1 (default) considers every possible split, split.step = 10
#'  considers splitting at every 10'th sample and may yield a substantial speedup for dense features.
#'  Manually rounding or re-encoding continuous covariates with very high cardinality in a
#'  problem specific manner allows for finer-grained control of the accuracy/runtime tradeoff and may in some cases
#'  be the preferred approach.
#' @param min.node.size An integer indicating the smallest terminal node size permitted. Default is 1.
#' @param penalty.type todo
#' @param verbose Give verbose output. Default is TRUE.
#'
#' @return A policy_tree object.
#'
#' @examples
#' \donttest{
#' # Fit a depth two tree on doubly robust treatment effect estimates from a causal forest.
#' n <- 10000
#' p <- 10
#' # Discretizing continuous covariates decreases runtime.
#' X <- round(matrix(rnorm(n * p), n, p), 2)
#' colnames(X) <- make.names(1:p)
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
#' }
#' @export
penalized_policy_tree <- function(X,
                                  Gamma1,
                                  Gamma2,
                                  depth = 2,
                                  split.step = 1,
                                  min.node.size = 1,
                                  penalty.type = c("ratio", "difference"),
                                  verbose = TRUE) {
  penalty.type <- match.arg(penalty.type)
  n.features <- ncol(X)
  n.actions <- ncol(Gamma1)
  n.obs <- nrow(X)
  valid.classes <- c("matrix", "data.frame")

  if (!inherits(X, valid.classes) || !inherits(Gamma1, valid.classes) || !inherits(Gamma2, valid.classes)) {
    stop(paste("Currently the only supported data input types are:",
               "`matrix`, `data.frame`"))
  }
  if (!is.numeric(as.matrix(X)) || any(dim(X) == 0)) {
    stop("The feature matrix X must be numeric")
  }
  if (!is.numeric(as.matrix(Gamma1)) || any(dim(Gamma1) == 0) || !is.numeric(as.matrix(Gamma2)) || any(dim(Gamma2) == 0)) {
    stop("The reward matrix Gamma must be numeric")
  }
  if (anyNA(X)) {
    stop("Covariate matrix X contains missing values.")
  }
  if (anyNA(Gamma1) || anyNA(Gamma2)) {
    stop("Gamma matrix contains missing values.")
  }
  if (depth < 0 ) {
    stop("`depth` cannot be negative.")
  }
  if (n.obs != nrow(Gamma1) || n.obs != nrow(Gamma2)) {
    stop("X and Gamma does not have the same number of rows")
  }
  if (as.integer(split.step) != split.step || split.step < 1) {
    stop("`split.step` should be an integer greater than or equal to 1.")
  }
  if (as.integer(min.node.size) != min.node.size || min.node.size < 1) {
    stop("min.node.size should be an integer greater than or equal to 1.")
  }

  if (verbose) {
    cardinality <- apply(X, 2, function(x) length(unique(x)))
    if (split.step == 1 && any(cardinality > 20000)) {
      warning(paste0(
        "The cardinality of some covariates exceeds 20000 distinct values. ",
        "Consider using the optional parameter `split.step` to speed up computations, or ",
        "discretize/relabel continuous features for finer grained control ",
        "(the runtime of exact tree search scales with the number of distinct features, ",
        "see the documentation for details.)"
      ), immediate. = TRUE)
    }
    if (ncol(X) > 50) {
      warning(paste0(
        "The number of covariates exceeds 50. Consider reducing the dimensionality before ",
        "running policy_tree, by for example using only the Xj's with the ",
        "highest variable importance (`grf::variable_importance` - the runtime of exact tree ",
        "search scales with ncol(X)^depth, see the documentation for details)."
      ), immediate. = TRUE)
    }
  }

  action.names <- colnames(Gamma1)
  if (is.null(action.names)) {
    action.names <- as.character(1:ncol(Gamma1))
  }
  action.names <- utils::type.convert(action.names, as.is = TRUE) # TRUE to not convert character to factor
  columns <- colnames(X)
  if (is.null(columns)) {
    columns <- make.names(1:ncol(X))
  }

  if (penalty.type == "ratio") {
    reward.type = 2
  } else if (penalty.type == "difference") {
    reward.type = 3
  }

  Gamma <- cbind(Gamma1, Gamma2)
  result <- tree_search_rcpp(as.matrix(X), as.matrix(Gamma), depth, split.step, min.node.size, reward.type, 2)
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
