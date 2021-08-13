test_that("predictions have not changed from first vetted version", {
  X <- read.csv("data/data_clf_X.csv")
  Y <- read.csv("data/data_clf_Y.csv")
  clf.exact <- read.csv("data/data_clf_exact.csv")
  printed.tree <- readLines("data/data_clf_tree.txt")

  pt <- policy_tree(X, Y, depth = 2)

  expect_equal(predict(pt, X), clf.exact$clf.exact)
  expect_equal(capture.output(print(pt)), printed.tree)
})

test_that("policy_tree has not changed", {
  X <- read.csv("data/data_clf_X.csv")
  Y <- read.csv("data/data_clf_Y.csv")
  X <- X[, 1:4]
  X[, 1:2] <- round(X[, 1:2], 1)

  pt1 <- policy_tree(X, Y, depth = 2)
  # writeLines(capture.output(print(pt1)), "data/data_pt1_tree.txt") # <--- uncomment to update
  expect_equal(capture.output(print(pt1)), readLines("data/data_pt1_tree.txt"))

  pt2 <- policy_tree(X, Y, depth = 2, split.step = 3, min.node.size = 5)
  # writeLines(capture.output(print(pt2)), "data/data_pt2_tree.txt") # <--- uncomment to update
  expect_equal(capture.output(print(pt2)), readLines("data/data_pt2_tree.txt"))

  pt3 <- policy_tree(X[1:75, ], Y[1:75, ], depth = 3, split.step = 2, min.node.size = 2)
  # writeLines(capture.output(print(pt3)), "data/data_pt3_tree.txt") # <--- uncomment to update
  expect_equal(capture.output(print(pt3)), readLines("data/data_pt3_tree.txt"))
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
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    plot(pt)
  }

  expect_equal(1, 1)
})


test_that("exact tree search finds the correct depth 0 tree", {
  depth <- 0
  n <- 100
  p <- 5
  d <- 3
  # Continuous X
  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(rnorm(n * d), n, d)

  best.action <- which.max(colMeans(Y))
  best.reward <- max(colMeans(Y))
  tree <- policy_tree(X, Y, depth = depth)
  action.tree <- predict(tree, X)
  reward.tree <- mean(Y[cbind(1:n, action.tree)])

  expect_equal(reward.tree, best.reward)
  expect_equal(rep(best.action, n), action.tree)

  # Discrete X
  X <- matrix(sample(10:20, n * p, replace = TRUE), n, p)

  tree <- policy_tree(X, Y, depth = depth)
  action.tree <- predict(tree, X)
  reward.tree <- mean(Y[cbind(1:n, action.tree)])

  expect_equal(reward.tree, best.reward)
  expect_equal(rep(best.action, n), action.tree)
})


test_that("exact tree search finds the correct depth 1 tree", {
  depth <- 1
  n <- 250
  p <- 5
  d <- 3
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
  expect_equal(best.action, predict(tree, X))

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
  expect_equal(best.action, predict(tree.discrete, X))
})


test_that("exact tree search finds the correct depth 2 tree", {
  depth <- 2
  n <- 250
  p <- 5
  d <- 3
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
  expect_equal(best.action, predict(tree, X))

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
  expect_equal(best.action, predict(tree.discrete, X))
})


test_that("exact tree search finds the correct depth 3 tree", {
  depth <- 3
  n <- 250
  p <- 4
  d <- 3
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
  expect_equal(best.action, predict(tree, X))

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
  expect_equal(best.action, predict(tree.discrete, X))
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
  expect_equal(predict(ptn.equalX, X), rep(which.max(colSums(Y)), n))

  X <- matrix(rnorm(n*p), n, p)
  Y <- matrix(0, n, d)
  ptn.equalY <- policy_tree(X, Y, depth = 2)
  expect_equal(predict(ptn.equalY, X), rep(1, n))
})


test_that("tiny n compared to depth does not break", {
  n <- 2
  p <- 5
  d <- 3
  X <- matrix(rnorm(n*p), n, p)
  Y <- matrix(rnorm(n*d), n, d)
  ptn <- policy_tree(X, Y, depth = 2)
  p <- capture.output(print(ptn))
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    plot(ptn)
  }

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
  expect_equal(predict(ptn, X), rep(2, n))
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

  expect_gt(reward.skip2, 0.95 * reward.full)
  expect_gt(reward.skip2, reward.colmax)
  expect_gt(reward.skip2, reward.halved)

  # split.step greater than the number of distinct values is meaningless
  # but passes through and just implies no splits:
  # Setting split.step to a number greater than the number of distinct features
  tree.all <- policy_tree(X, Y, depth = depth, split.step = n + 100)
  expect_equal(predict(tree.all, X), rep(colmax, n))
})


test_that("tree search with approximate splitting on data with low cardinality works as expected", {
  depth <- 2
  n <- 500
  p <- 1
  d <- 4
  X <- round(matrix(abs(rnorm(n * p)), n, p), 1)
  Y <- matrix(rnorm(n * d), n, d)
  best.tree <- make_tree(X, depth = depth, d = d)
  best.action <- predict_test_tree(best.tree, X)
  Y[cbind(1:n, best.action)] <- 100 * runif(n)

  tree <- policy_tree(X, Y, depth = depth, split.step = 10)
  best.reward <- mean(Y[cbind(1:n, best.action)])
  reward <- mean(Y[cbind(1:n, predict(tree, X))])

  expect_equal(reward, best.reward, tol = 0.05)
})


test_that("leaf node predictions work as expected", {
  depth <- 2
  n <- 250
  p <- 5
  d <- 3
  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(0, n, d)

  tree <- policy_tree(X, Y, depth = depth)
  leaf.id <- predict(tree, X, type = "node.id")
  expect_equal(leaf.id, rep(1, n))

  # Leaf node and action predictions are internally consistent

  # Continuous X tree
  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(0, n, d)
  best.tree <- make_tree(X, depth = depth, d = d)
  best.action <- predict_test_tree(best.tree, X)
  Y[cbind(1:n, best.action)] <- 100 * runif(n)
  tree <- policy_tree(X, Y, depth = depth)

  pp <- predict(tree, X)
  pp.node <- predict(tree, X, type = "node.id")
  values <- aggregate(Y, by = list(leaf.node = pp.node), FUN = mean)
  best <- apply(values[, -1], 1, FUN = which.max)
  expect_equal(best[match(pp.node, values[, 1])], pp)

  # Discrete X
  X <- matrix(sample(10:20, n * p, replace = TRUE), n, p)
  Y <- matrix(0, n, d)
  best.tree.discrete <- make_tree(X, depth = depth, d = d)
  best.action <- predict_test_tree(best.tree.discrete, X)
  Y[cbind(1:n, best.action)] <- 100 * runif(n)
  tree.discrete <- policy_tree(X, Y, depth = depth)

  pp.discrete <- predict(tree.discrete, X)
  pp.node.discrete <- predict(tree.discrete, X, type = "node.id")
  values.discrete <- aggregate(Y, by = list(leaf.node = pp.node.discrete), FUN = mean)
  best.discrete <- apply(values.discrete[, -1], 1, FUN = which.max)
  expect_equal(best.discrete[match(pp.node.discrete, values.discrete[, 1])], pp.discrete)
})

test_that("min.node.size works as expected", {
  depth <- 2
  n <- 100
  p <- 5
  d <- 3
  # Continuous X
  X <- matrix(runif(n * p), n, p)
  Y <- matrix(rnorm(n * d), n, d)
  tree <- policy_tree(X, Y, depth = depth)
  smallest.leaf <- min(summary(as.factor(predict(tree, X, type = "node.id"))))

  tree.min <- policy_tree(X, Y, depth = depth, min.node.size = smallest.leaf + 5)
  leaf.sizes.min <- summary(as.factor(predict(tree.min, X, type = "node.id")))
  expect_true(all(leaf.sizes.min >= smallest.leaf + 5))

  # Discrete X
  X <- matrix(sample(1:5, n, TRUE), n, p)
  tree <- policy_tree(X, Y, depth = depth)
  smallest.leaf <- min(summary(as.factor(predict(tree, X, type = "node.id"))))

  tree.min <- policy_tree(X, Y, depth = depth, min.node.size = smallest.leaf + 5)
  leaf.sizes.min <- summary(as.factor(predict(tree.min, X, type = "node.id")))
  expect_true(all(leaf.sizes.min >= smallest.leaf + 5))
})
