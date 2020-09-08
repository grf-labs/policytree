#' A utility function for generating random trees
#' Build a depth `depth` tree by drawing random split variables
#' and split values from the Nxp matrix `X`. In leaf nodes a random
#' action is drawn from 1:`d`. (Minimum leaf size will be 1)
#' @param X data matrix.
#' @param depth tree depth.
#' @param d number of actions.
#'
#' @return A policy_tree tree object
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

# Predict with the above test tree. 
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

test_that("predictions have not changed from first vetted version", {
  X <- read.csv("data_clf_X.csv")
  Y <- read.csv("data_clf_Y.csv")
  clf.exact <- read.csv("data_clf_exact.csv")

  pt <- policy_tree(X, Y, depth = 2)

  expect_equal(predict(pt, X), clf.exact$clf.exact)
})


test_that("solver bindings run", {
  n <- 50
  p <- 10
  d <- 3
  X <- matrix(runif(n * p), n, p)
  Y <- matrix(runif(n * d), n, d)
  pt <- policy_tree(X, Y, depth = 2)
  p <- capture.output(print(pt))

  p1 <- predict(pt, X)
  p2 <- predict(pt, X[1, , drop = FALSE])

  p <- capture.output(print(pt))
  plot(pt)

  expect_equal(1, 1)
})


test_that("exact tree search finds the correct depth 0 tree", {
  depth <- 0
  n <- 100
  p <- sample(1:10, 1)
  d <- sample(1:5, 1)
  # Continuous X
  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(rnorm(n * d), n, d)

  best.action <- which.max(colMeans(Y))
  best.reward <- max(colMeans(Y))
  tree <- policy_tree(X, Y, depth = depth)
  action.tree <- predict(tree, X)
  reward.tree <- mean(Y[cbind(1:n, action.tree)])

  expect_equal(reward.tree, best.reward)
  expect_true(all(best.action == action.tree))

  # Discrete X
  X <- matrix(sample(10:20, n * p, replace = TRUE), n, p)

  tree <- policy_tree(X, Y, depth = depth)
  action.tree <- predict(tree, X)
  reward.tree <- mean(Y[cbind(1:n, action.tree)])

  expect_equal(reward.tree, best.reward)
  expect_true(all(best.action == action.tree))
})


test_that("exact tree search finds the correct depth 1 tree", {
  depth <- 1
  n <- 250
  p <- sample(1:10, 1)
  d <- sample(1:5, 1)
  # Continuous X
  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(0, n, d)

  best.tree <- make_tree(X, depth = depth, d = d)
  best.action <- predict_test_tree(best.tree, X)
  Y[cbind(1:n, best.action)] <- 100 * runif(n)
  best.reward <- mean(Y[cbind(1:n, best.action)])
  tree <- policy_tree(X, Y, depth = depth)
  reward.tree <- mean(Y[cbind(1:n, predict(tree, X))])

  expect_equal(reward.tree, best.reward)
  expect_true(all(best.action == predict(tree, X)))

  # Discrete X
  X <- matrix(sample(10:20, n * p, replace = TRUE), n, p)
  Y <- matrix(0, n, d)

  best.tree.discrete <- make_tree(X, depth = depth, d = d)
  best.action <- predict_test_tree(best.tree.discrete, X)
  Y[cbind(1:n, best.action)] <- 100 * runif(n)
  best.reward <- mean(Y[cbind(1:n, best.action)])
  tree.discrete <- policy_tree(X, Y, depth = depth)
  reward.tree <- mean(Y[cbind(1:n, predict(tree.discrete, X))])

  expect_equal(reward.tree, best.reward)
  expect_true(all(best.action == predict(tree.discrete, X)))
})


test_that("exact tree search finds the correct depth 2 tree", {
  depth <- 2
  n <- 250
  p <- sample(1:10, 1)
  d <- sample(1:5, 1)
  # Continuous X
  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(0, n, d)

  best.tree <- make_tree(X, depth = depth, d = d)
  best.action <- predict_test_tree(best.tree, X)
  Y[cbind(1:n, best.action)] <- 100 * runif(n)
  best.reward <- mean(Y[cbind(1:n, best.action)])
  tree <- policy_tree(X, Y, depth = depth)
  reward.tree <- mean(Y[cbind(1:n, predict(tree, X))])

  expect_equal(reward.tree, best.reward)
  expect_true(all(best.action == predict(tree, X)))

  # Discrete X
  X <- matrix(sample(10:20, n * p, replace = TRUE), n, p)
  Y <- matrix(0, n, d)

  best.tree.discrete <- make_tree(X, depth = depth, d = d)
  best.action <- predict_test_tree(best.tree.discrete, X)
  Y[cbind(1:n, best.action)] <- 100 * runif(n)
  best.reward <- mean(Y[cbind(1:n, best.action)])
  tree.discrete <- policy_tree(X, Y, depth = depth)
  reward.tree <- mean(Y[cbind(1:n, predict(tree.discrete, X))])

  expect_equal(reward.tree, best.reward)
  expect_true(all(best.action == predict(tree.discrete, X)))
})


test_that("exact tree search finds the correct depth 3 tree", {
  depth <- 3
  n <- 250
  p <- sample(1:10, 1)
  d <- sample(1:5, 1)
  # Continuous X
  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(0, n, d)

  best.tree <- make_tree(X, depth = depth, d = d)
  best.action <- predict_test_tree(best.tree, X)
  Y[cbind(1:n, best.action)] <- 100 * runif(n)
  best.reward <- mean(Y[cbind(1:n, best.action)])
  tree <- policy_tree(X, Y, depth = depth)
  reward.tree <- mean(Y[cbind(1:n, predict(tree, X))])

  expect_equal(reward.tree, best.reward)
  expect_true(all(best.action == predict(tree, X)))

  # Discrete X
  X <- matrix(sample(10:20, n * p, replace = TRUE), n, p)
  Y <- matrix(0, n, d)

  best.tree.discrete <- make_tree(X, depth = depth, d = d)
  best.action <- predict_test_tree(best.tree.discrete, X)
  Y[cbind(1:n, best.action)] <- 100 * runif(n)
  best.reward <- mean(Y[cbind(1:n, best.action)])
  tree.discrete <- policy_tree(X, Y, depth = depth)
  reward.tree <- mean(Y[cbind(1:n, predict(tree.discrete, X))])

  expect_equal(reward.tree, best.reward)
  expect_true(all(best.action == predict(tree.discrete, X)))
})


test_that("solver works with duplicate categorical X's", {
  n <- 100
  p <- 2
  d <- 3
  X <- matrix(sample(1:4, n * p, TRUE), n, p)
  Y <- matrix(sample(1:10, n * d, TRUE), n, d)
  ptn <- policy_tree(X, Y, depth = 2)
  reward.n <- sum(Y[cbind(1:nrow(X), predict(ptn, X))])

  expect_equal(1, 1)
})


test_that("solver does not break with all identical X's or Y's", {
  n <- 200
  p <- 5
  d <- 3
  X <- matrix(0, n, p)
  Y <- matrix(rnorm(n*d), n, d)
  ptn.equalX <- policy_tree(X, Y, depth = 2)
  expect_true(all(predict(ptn.equalX, X) == which.max(colSums(Y))))

  X <- matrix(rnorm(n*p), n, p)
  Y <- matrix(0, n, d)
  ptn.equalY <- policy_tree(X, Y, depth = 2)
  expect_true(all(predict(ptn.equalY, X) == 1))
})


test_that("tiny n compared to depth does not break", {
  n <- 2
  p <- 5
  d <- 3
  X <- matrix(rnorm(n*p), n, p)
  Y <- matrix(rnorm(n*d), n, d)
  ptn <- policy_tree(X, Y, depth = 2)
  p <- capture.output(print(ptn))
  plot(ptn)

  expect_equal(1, 1)
})


test_that("all equal rewards are pruned", {
  n <- 100
  p <- 5
  d <- 3
  X <- matrix(rnorm(n*p), n, p)
  Y <- matrix(0, n, d)
  Y[, 2] <- 100
  ptn <- policy_tree(X, Y, depth = 2)

  expect_equal(ptn$nodes[[1]]$action, 2)
})


test_that("tree search with approximate splitting works as expected", {
  depth <- 2
  n <- 10000
  p <- 5
  d <- 2

  X <- matrix(rnorm(n * p), n, p)
  X.halved <- matrix(sample(X, n / 2, replace = TRUE), n, p)
  Y <- matrix(rnorm(n * d), n, d)

  tree <- policy_tree(X, Y, depth = depth, split.step = 1)
  tree.skip2 <- policy_tree(X, Y, depth = depth, split.step = 2)
  tree.halved <- policy_tree(X.halved, Y, depth = depth, split.step = 1)

  reward.full <- mean(Y[cbind(1:n, predict(tree, X))])
  reward.skip2 <- mean(Y[cbind(1:n, predict(tree.skip2, X))])
  reward.halved <- mean(Y[cbind(1:n, predict(tree.halved, X.halved))])
  colmax <- which.max(colMeans(Y))
  reward.colmax <- max(colMeans(Y))

  expect_true(reward.skip2 > 0.95 * reward.full)
  expect_true(reward.skip2 > reward.colmax)
  expect_true(reward.skip2 > reward.halved)

  # split.step <= 0 or greater than the number of distinct values is meaningless
  # but passes through and just implies no splits:
  # Setting split.step to a number greater than the number of distinct features
  tree.all <- policy_tree(X, Y, depth = depth, split.step = n + 100)
  expect_true(all(predict(tree.all, X) == colmax))

  # Which is the same as setting split.step to 0 or smaller
  tree.all <- policy_tree(X, Y, depth = depth, split.step = 0)
  expect_true(all(predict(tree.all, X) == colmax))
  tree.all <- policy_tree(X, Y, depth = depth, split.step = -n)
  expect_true(all(predict(tree.all, X) == colmax))
})
