#' A utility function for generating random trees for test purposes.
#'
#' Build a depth `depth` tree by drawing random split variables
#' and split values from the Nxp matrix `X`. In leaf nodes a random
#' action is drawn from 1:`d`. (Minimum leaf size will be 1)
#' @param X data matrix.
#' @param depth tree depth.
#' @param d number of actions.
#' @return A policy_tree tree object
#' @keywords internal
#' @examples
#' \dontrun{
#' depth <- 2
#' n <- 100
#' p <- 10
#' d <- 3
#' X <- matrix(rnorm(n * p), n, p)
#' Y <- matrix(rnorm(n * d), n, d)
#' tree <- make_tree(X, depth = depth, d = d)
#' pp <- predict_test_tree(tree, X)
#' }
make_tree <- function(X, depth, d) {
  node.index <- 0
  nodes <- list()
  make_split <- function(X, level) {
    node.index <<- node.index + 1
    if (level == 0 | nrow(X) <= 1) {
      node <- list(is_leaf = TRUE,
                   action = sample(1:d, 1))
      nodes <<- c(nodes, list(node))
    } else if (nrow(X) > 1) {
      split_var <- sample(1:ncol(X), 1)
      split_val <- sample(X[, split_var], 1)
      node <- list(is_leaf = FALSE,
                   split_variable = split_var,
                   split_value = split_val,
                   left_child = node.index + 1)
      nodes <<- c(nodes, list(node))
      node.index.this <- node.index
      left <- X[X[, split_var] <= split_val, , drop = F]
      right <- X[X[, split_var] > split_val, , drop = F]
      make_split(left, level - 1)
      nodes[[node.index.this]]$right_child <<- node.index + 1
      make_split(right, level - 1)
    }
  }
  make_split(X, depth)
  tree <- list(nodes = nodes)

  tree[["depth"]] <- depth
  tree[["n.actions"]] <- d
  tree[["n.features"]] <- ncol(X)
  tree[["action.names"]] <- 1:d
  tree[["columns"]] <- paste0("X", 1:ncol(X))
  class(tree) <- "policy_tree"

  tree
}

#' Predict with the above test tree.
#' @param tree tree from make_tree
#' @param newdata data matrix
#'
#' @return Vector of predictions
#' @keywords internal
predict_test_tree <- function(tree, newdata) {
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
  leaf.nodes <- apply(newdata, 1, function(sample) find_leaf_node(tree, sample))
  out <- sapply(leaf.nodes, function(node) tree$nodes[[node]]$action)

  out
}
