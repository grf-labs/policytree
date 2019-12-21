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
  p2 <- predict(pt, X[1, ])

  p <- capture.output(print(pt))
  plot(pt)

  expect_equal(1, 1)
})


test_that("exact tree search finds the correct depth 1 tree", {
  n <- 101
  p <- 10
  d <- 4
  xi <- seq(0, 1, length.out = n)
  X <- matrix(sample(xi, n * p, TRUE), n, p)
  Y <- matrix(rnorm(n * d), n, d)
  split1 <- sample(X[X != 0 & X != 1], 1)
  splitvar1 <- sample(1:p, 1)
  best.action1 <- sample(1:d, 1)
  best.action2 <- sample((1:d)[-best.action1], 1)
  Y[X[, splitvar1] < split1, best.action1] <- Y[X[, splitvar1] < split1, best.action1] + 500
  Y[X[, splitvar1] >= split1, best.action2] <- Y[X[, splitvar1] >= split1, best.action2] + 500
  tree <- policy_tree(X, Y, depth = 1)

  expect_equal(tree$nodes[[1]]$split_variable, splitvar1)
  expect_lte(tree$nodes[[1]]$split_value, split1)
  expect_equal(tree$nodes[[2]]$action, best.action1)
  expect_equal(tree$nodes[[3]]$action, best.action2)
})


test_that("exact tree search finds the optimal depth 2 reward", {
  n <- 200
  p <- 10
  d <- 5
  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(0, n, d)
  split1 <- sample(X, 1)
  splitvar1 <- sample(1:p, 1)
  # Could in principle check if this is feasible if splitvar2=splitvar1, very unlikely however.
  split2 <- sample(X, 1)
  splitvar2 <- sample(1:p, 1)
  split3 <- sample(X, 1)
  splitvar3 <- sample(1:p, 1)
  best.action1 <- 1
  best.action2 <- 2
  best.action3 <- 3
  best.action4 <- 4

  best.tree <- list()
  best.tree[[1]] <- list(is_leaf = FALSE, split_variable = splitvar1, split_value = split1, left_child = 2, right_child = 3)
  best.tree[[2]] <- list(is_leaf = FALSE, split_variable = splitvar2, split_value = split2, left_child = 4, right_child = 5)
  best.tree[[3]] <- list(is_leaf = FALSE, split_variable = splitvar3, split_value = split3, left_child = 6, right_child = 7)
  best.tree[[4]] <- list(is_leaf = TRUE, action = best.action1)
  best.tree[[5]] <- list(is_leaf = TRUE, action = best.action2)
  best.tree[[6]] <- list(is_leaf = TRUE, action = best.action3)
  best.tree[[7]] <- list(is_leaf = TRUE, action = best.action4)
  best.tree <- list(nodes = best.tree)
  leaf.nodes <- apply(X, 1, function(sample) find_leaf_node(best.tree, sample))
  best.action <- sapply(leaf.nodes, function(node) best.tree$nodes[[node]]$action)
  Y[cbind(1:n, best.action)] <- runif(n) * 10
  best.reward <- mean(Y[cbind(1:n, best.action)])

  tree <- policy_tree(X, Y, depth = 2)
  reward.tree <- mean(Y[cbind(1:n, predict(tree, X))])

  expect_equal(reward.tree, best.reward)
})


test_that("exact tree search finds the optimal depth 2 reward (discrete X)", {
  n <- 200
  p <- 10
  d <- 5
  X <- matrix(sample(50:60, n*p, TRUE), n, p)
  Y <- matrix(0, n, d)
  split1 <- sample(X, 1)
  splitvar1 <- sample(1:p, 1)
  # Could in principle check if this is feasible if splitvar2=splitvar1, very unlikely however.
  split2 <- sample(X, 1)
  splitvar2 <- sample(1:p, 1)
  split3 <- sample(X, 1)
  splitvar3 <- sample(1:p, 1)
  best.action1 <- 1
  best.action2 <- 2
  best.action3 <- 3
  best.action4 <- 4

  best.tree <- list()
  best.tree[[1]] <- list(is_leaf = FALSE, split_variable = splitvar1, split_value = split1, left_child = 2, right_child = 3)
  best.tree[[2]] <- list(is_leaf = FALSE, split_variable = splitvar2, split_value = split2, left_child = 4, right_child = 5)
  best.tree[[3]] <- list(is_leaf = FALSE, split_variable = splitvar3, split_value = split3, left_child = 6, right_child = 7)
  best.tree[[4]] <- list(is_leaf = TRUE, action = best.action1)
  best.tree[[5]] <- list(is_leaf = TRUE, action = best.action2)
  best.tree[[6]] <- list(is_leaf = TRUE, action = best.action3)
  best.tree[[7]] <- list(is_leaf = TRUE, action = best.action4)
  best.tree <- list(nodes = best.tree)
  leaf.nodes <- apply(X, 1, function(sample) find_leaf_node(best.tree, sample))
  best.action <- sapply(leaf.nodes, function(node) best.tree$nodes[[node]]$action)
  Y[cbind(1:n, best.action)] <- runif(n) * 10
  best.reward <- mean(Y[cbind(1:n, best.action)])

  tree <- policy_tree(X, Y, depth = 2)
  reward.tree <- mean(Y[cbind(1:n, predict(tree, X))])

  expect_equal(reward.tree, best.reward)
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

  X <- matrix(rnorm(n*p), n, p)
  X <- matrix(0, n, d)
  ptn.equalY <- policy_tree(X, Y, depth = 2)

  expect_equal(1, 1)
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
