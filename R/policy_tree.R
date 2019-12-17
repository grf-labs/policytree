#' Fit a policy with exact tree search
#'
#' Finds the optimal (maximizing the sum of rewards) depth L tree by exhaustive search. If the optimal
#'  action is the same in both the left and right leaf of a node, the node is pruned.
#'
#'
#' @param X The covariates used. Dimension \eqn{Np} where \eqn{p} is the number of features.
#' @param Gamma The rewards for each action. Dimension \eqn{Nd} where \eqn{d} is the number of actions.
#' @param depth The depth of the fitted tree. Default is 2.
#'
#' @return A policy_tree object.
#'
#' @references Zhou, Zhengyuan, Susan Athey, and Stefan Wager. "Offline multi-action policy learning:
#'  Generalization and optimization." arXiv preprint arXiv:1810.04778 (2018).
#'
#' @examples
#' n <- 50
#' p <- 10
#' d <- 3
#' features <- matrix(rnorm(n * p), n, p)
#' rewards <- matrix(rnorm(n * d), n, d)
#' tree <- policy_tree(features, rewards, depth = 2)
#' tree
#'
#' @export
#' @useDynLib policyTree
#' @importFrom Rcpp evalCpp
#' @importFrom utils type.convert
policy_tree <- function(X, Gamma, depth = 2) {
  X <- as.matrix(X)
  Gamma <- as.matrix(Gamma)
  n.features <- ncol(X)
  n.outcomes <- ncol(Gamma)
  n.obs <- nrow(X)

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

  outcome.names <- colnames(Gamma)
  if (is.null(outcome.names)) {
    outcome.names <- as.character(1:ncol(Gamma))
  }
  outcome.names <- type.convert(outcome.names, as.is = TRUE) # TRUE to not convert character to factor
  columns <- colnames(X)
  if (is.null(columns)) {
    columns <- make.names(1:ncol(X))
  }

  nodes <- tree_search_rcpp(X, Gamma, depth)
  tree = list(nodes = nodes)

  tree[["depth"]] <- depth
  tree[["n.outcomes"]] <- n.outcomes
  tree[["n.features"]] <- n.features
  tree[["outcome.names"]] <- outcome.names
  tree[["columns"]] <- columns
  class(tree) <- "policy_tree"

  tree
}

#' Predict method for policy_tree
#'
#' Predict values based on fitted policyTree object.
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
#' n <- 50
#' p <- 10
#' d <- 3
#' features <- matrix(rnorm(n * p), n, p)
#' rewards <- matrix(rnorm(n * d), n, d)
#' tree <- policy_tree(features, rewards, depth = 2)
#' print(tree)
#' predict(tree, features)
predict.policy_tree <- function(object, newdata, ...) {
  # if nrow = 1:
  if (is.null(dim(newdata))) {
    newdata <- matrix(newdata, nrow = 1)
  }
  tree <- object
  if (tree$n.features != ncol(newdata)) {
    stop("This tree was trained with ", tree$n.features, " variables. Provided: ", ncol(newdata))
  }

  leaf.nodes <- apply(newdata, 1, function(sample) find_leaf_node(tree, sample))
  out <- sapply(leaf.nodes, function(node) tree$nodes[[node]]$outcome)

  out
}

#' Query a tree with a sample
#'
#' @param tree policy_tree tree object
#' @param sample A vector of observations \eqn{X_i}
#'
#' @return The leaf node the sample falls in to.
#'
#' @keywords internal
find_leaf_node <- function(tree, sample) {
  node <- 1
  while (TRUE) {
    if (tree$nodes[[node]]$is_leaf) {
      return(node)
    }
    split_var <- tree$nodes[[node]]$split_variable
    split_value <- tree$nodes[[node]]$split_value
    if (sample[split_var] <= split_value) {
      node <- tree$nodes[[node]]$left_child
    } else {
      node <- tree$nodes[[node]]$right_child
    }
  }
}
