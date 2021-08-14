#' Hybrid tree search (experimental)
#'
#' Finds a depth k tree by looking ahead l steps.
#'
#'
#' Builds deeper trees by iteratively using exact tree search to look ahead l splits. For example,
#' with `depth = 3` and `search.depth = 2`, the first split is determined by a depth 2 exact tree,
#' and two new depth 2 trees are fit in the immediate children, leading to a total depth of 3.
#' Note that the resulting tree may be shallower than the specified `depth` depending on
#' whether leaf nodes were pruned or not.
#' This algorithm scales with some coefficient multiple of the runtime of a `search.depth` `policy_tree`.
#'
#' The algorithm: desired depth is given by `depth`. Each node is split using exact tree search
#' with depth  = `search.depth`. When we reach a node where the current level + `search.depth` is equal to `depth`,
#' we stop and attach the `search.depth` subtree to this node.
#' We also stop if the best `search.depth` split yielded a leaf node.
#'
#' @param X The covariates used. Dimension \eqn{N*p} where \eqn{p} is the number of features.
#' @param Gamma The rewards for each action. Dimension \eqn{N*d} where \eqn{d} is the number of actions.
#' @param depth The depth of the fitted tree. Default is 3.
#' @param search.depth Depth to look ahead when splitting. Default is 2.
#' @param split.step An optional approximation parameter, the number of possible splits
#'  to consider when performing tree search. split.step = 1 (default) considers every possible split, split.step = 10
#'  considers splitting at every 10'th sample and may yield a substantial speedup for dense features.
#'  Manually rounding or re-encoding continuous covariates with very high cardinality in a
#'  problem specific manner allows for finer-grained control of the accuracy/runtime tradeoff and may in some cases
#'  be the preferred approach.
#' @param min.node.size An integer indicating the smallest terminal node size permitted. Default is 1.
#' @param verbose Give verbose output. Default is TRUE.
#'
#' @return A policy_tree object.
#'
#' @examples
#' \donttest{
#' # Fit a depth three tree on doubly robust treatment effect estimates from a causal forest.
#' n <- 1500
#' p <- 5
#' X <- round(matrix(rnorm(n * p), n, p), 2)
#' W <- rbinom(n, 1, 1 / (1 + exp(X[, 3])))
#' tau <- 1 / (1 + exp((X[, 1] + X[, 2]) / 2)) - 0.5
#' Y <- X[, 3] + W * tau + rnorm(n)
#' c.forest <- grf::causal_forest(X, Y, W)
#' dr.scores <- double_robust_scores(c.forest)
#'
#' tree <- hybrid_policy_tree(X, dr.scores, depth = 3)
#'
#' # Predict treatment assignment.
#' predicted <- predict(tree, X)
#' }
#' @export
hybrid_policy_tree <- function(X, Gamma,
                               depth = 3,
                               search.depth = 2,
                               split.step = 1,
                               min.node.size = 1,
                               verbose = TRUE) {
  if (search.depth >= depth) {
    stop("`search.depth` should be less than `depth`.")
  }
  if (depth > 20) {
    stop("Specified `depth` is too close to R's memory limit of 2^32 - 1.")
  }
  if (verbose && (!search.depth %in% c(2, 3))) {
    warning("Suggested values for `search.depth` is 2 or 3. ", immediate. = TRUE)
  }
  # Dummy tree object.
  tree <- policy_tree(X[1, , drop = FALSE], Gamma[1, , drop = FALSE], depth = 0, verbose = FALSE)
  if (nrow(X) != nrow(Gamma)) {
    stop("X and Gamma does not have the same number of rows")
  }

  samples <- list(seq_len(nrow(X)))
  levels <- list(0)
  open.nodes <- 1
  node <- 1
  stop <- FALSE
  tree.nodes <- list()
  while (open.nodes > 0) {
    subset <- samples[[node]]
    level <- levels[[node]]
    subtree <- policy_tree(X[subset, , drop = FALSE], Gamma[subset, , drop = FALSE],
                           depth = search.depth, split.step = split.step,
                           min.node.size = min.node.size, verbose = verbose)[["nodes"]]
    if (subtree[[1]]$is_leaf) {
      tree.nodes[[node]] <- list(is_leaf = TRUE, has_subtree = FALSE, action = subtree[[1]]$action)
      stop <- TRUE
    } else if (level + search.depth == depth) {
      tree.nodes[[node]] <- list(is_leaf = FALSE, has_subtree = TRUE, subtree = subtree)
      stop <- TRUE
    } else {
      split.var <- subtree[[1]]$split_variable
      split.val <- subtree[[1]]$split_value
      left.child <- length(samples) + 1
      right.child <- length(samples) + 2
      ix <- which((X[, split.var] <= split.val)[subset])
      samples[[left.child]] <- subset[ix]
      samples[[right.child]] <- subset[-ix]
      levels[[left.child]] <- level + 1
      levels[[right.child]] <- level + 1
      tree.nodes[[node]] <- list(is_leaf = FALSE,
                                 has_subtree = FALSE,
                                 split_variable = split.var,
                                 split_value = split.val,
                                 left_child = left.child,
                                 right_child = right.child)
     stop <- FALSE
    }
    if (stop) {
      open.nodes <- open.nodes - 1
    } else {
      open.nodes <- open.nodes + 1
    }
    node <- node + 1
  }

  tree[["nodes"]] <- unpack_tree(tree.nodes)
  tree[["_tree_array"]] <- tree_mat(tree[["nodes"]], depth)
  tree[["depth"]] <- depth

  tree
}

# "Unpack" potentially nested subtrees in leaf nodes of a tree.
unpack_tree <- function(tree) {
  nodes <- list()
  node.index <- 1
  frontier <- 1
  inds <- 1
  while (length(frontier) > 0) {
    node <- frontier[1]
    frontier <- frontier[-1]
    ind <- inds[1]
    inds <- inds[-1]
    if (tree[[node]]$is_leaf) {
      nodes[[ind]] <- list(is_leaf = TRUE, action = tree[[node]]$action)
    } else if (!tree[[node]]$has_subtree) {
      nodes[[ind]] <- list(is_leaf = FALSE,
                           split_variable = tree[[node]]$split_variable,
                           split_value = tree[[node]]$split_value,
                           left_child = node.index + 1,
                           right_child = node.index + 2)
      inds <- c(inds, node.index + 1, node.index + 2)
      frontier <- c(frontier, tree[[node]]$left_child, tree[[node]]$right_child)
      node.index <- node.index + 2
    } else {
      subtree <- tree[[node]]$subtree
      subtree.frontier <- 1
      subinds <- ind
      while (length(subtree.frontier) > 0) {
        subnode <- subtree.frontier[1]
        subtree.frontier <- subtree.frontier[-1]
        subind <- subinds[1]
        subinds <- subinds[-1]
        if (subtree[[subnode]]$is_leaf) {
          nodes[[subind]] <- list(is_leaf = TRUE, action = subtree[[subnode]]$action)
        } else {
          nodes[[subind]] <- list(is_leaf = FALSE,
                                  split_variable = subtree[[subnode]]$split_variable,
                                  split_value = subtree[[subnode]]$split_value,
                                  left_child = node.index + 1,
                                  right_child = node.index + 2)
          subinds <- c(subinds, node.index + 1, node.index + 2)
          subtree.frontier <- c(subtree.frontier, subtree[[subnode]]$left_child, subtree[[subnode]]$right_child)
          node.index <- node.index + 2
        }
      }
    }
  }

  nodes
}

# Convert an adjacency list to array for predictions.
# see Rcppbindigs.cpp for details.
# Future TODO: consider performing this here instead of in Rcppbindigs.
# For "largish" depth it could also be sparse.
tree_mat <- function(nodes, depth) {
  num.nodes <- 2^(depth + 1) - 1
  tree.array <- matrix(0, num.nodes, 4)
  frontier <- 1
  i <- 1
  j <- 1
  while (length(frontier) > 0) {
    node <- frontier[1]
    frontier <- frontier[-1]
    if (nodes[[node]]$is_leaf) {
      tree.array[j, 1] <- -1
      tree.array[j, 2] <- nodes[[node]]$action
    } else {
      tree.array[j, 1] <- nodes[[node]]$split_variable
      tree.array[j, 2] <- nodes[[node]]$split_value
      tree.array[j, 3] <- i + 1
      tree.array[j, 4] <- i + 2
      frontier <- c(frontier, nodes[[node]]$left_child, nodes[[node]]$right_child)
      i <- i + 2
    }
    j <- j + 1
  }

  tree.array
}
