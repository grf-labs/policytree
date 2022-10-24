test_that("penalized policy tree reward calculation works as expected", {
  n <- 500
  p <- 1
  d <- 10
  X <- matrix(rnorm(n * p), n, p)
  Y1 <- matrix(rnorm(n * d), n, d)
  Y2 <- matrix(10*runif(n * d), n, d)

  # ratio objective
  objective.ratio <- colMeans(Y1) / pmax(1, sqrt(colMeans(Y2)))
  ppt <- penalized_policy_tree(X, Y1, Y2, depth = 0, penalty.type = "ratio")
  expect_equal(ppt$nodes[[1]]$action, which.max(objective.ratio))

  # sum objective
  objective.sum <- colSums(Y1) + sqrt(colSums(Y2))
  ppt.sum <- penalized_policy_tree(X, Y1, Y2, depth = 0, penalty.type = "sum")
  expect_equal(ppt.sum$nodes[[1]]$action, which.max(objective.sum))

  # penalized sum objective
  lambda <- -100
  objective.sump <- colSums(Y1) + lambda * sqrt(colSums(Y2))
  ppt.sump <- penalized_policy_tree(X, Y1, Y2, depth = 0, penalty.type = "sum", lambda = lambda)
  expect_equal(ppt.sump$nodes[[1]]$action, which.max(objective.sump))
})


test_that("depth 0 penalized policy tree works as expected", {
  depth <- 0
  n <- 500
  p <- 5
  d <- 3
  X <- matrix(rnorm(n * p), n, p)
  Y1 <- matrix(rnorm(n * d), n, d)
  Y2 <- matrix(100 * runif(n * d), n, d)
  Y.ones <- matrix(1, n, d)
  pt <- policy_tree(X, Y1, depth = depth)

  # zero penalties: same
  ppt.ratio <- penalized_policy_tree(X, Y1, Y2 * 0, depth = depth, penalty.type = "ratio")
  ppt.sum <- penalized_policy_tree(X, Y1, Y2 * 0, depth = depth, penalty.type = "sum")
  ppt.sum.lam <- penalized_policy_tree(X, Y1, Y2, depth = depth, penalty.type = "sum", lambda = 0)
  expect_equal(predict(pt, X), predict(ppt.ratio, X))
  expect_equal(predict(pt, X), predict(ppt.sum, X))
  expect_equal(predict(pt, X), predict(ppt.sum.lam, X))

  # invariances
  ppt.ratio <- penalized_policy_tree(X, Y1, Y.ones, depth = depth, penalty.type = "ratio")
  ppt.ratio.scaled <- penalized_policy_tree(X, Y1, Y.ones * 100, depth = depth, penalty.type = "ratio")
  expect_equal(predict(ppt.ratio, X), predict(ppt.ratio.scaled, X))
})


test_that("depth 2 penalized policy tree works as expected", {
  depth <- 2
  n <- 500
  p <- 5
  d <- 3
  X <- matrix(rnorm(n * p), n, p)
  Y1 <- matrix(rnorm(n * d), n, d)
  Y2 <- matrix(100 * runif(n * d), n, d)
  Y.ones <- matrix(1, n, d)
  pt <- policy_tree(X, Y1, depth = depth)

  # zero penalties: same
  ppt.ratio <- penalized_policy_tree(X, Y1, Y2 * 0, depth = depth, penalty.type = "ratio")
  ppt.sum <- penalized_policy_tree(X, Y1, Y2 * 0, depth = depth, penalty.type = "sum")
  ppt.sum.lam <- penalized_policy_tree(X, Y1, Y2, depth = depth, penalty.type = "sum", lambda = 0)
  expect_equal(predict(pt, X), predict(ppt.ratio, X))
  expect_equal(predict(pt, X), predict(ppt.sum, X))
  expect_equal(predict(pt, X), predict(ppt.sum.lam, X))

  # invariances
  ppt.ratio <- penalized_policy_tree(X, Y1, Y.ones, depth = depth, penalty.type = "ratio")
  ppt.ratio.scaled <- penalized_policy_tree(X, Y1, Y.ones * 100, depth = depth, penalty.type = "ratio")
  expect_equal(predict(ppt.ratio, X), predict(ppt.ratio.scaled, X))

  ppt.ratio <- penalized_policy_tree(X, Y1, Y2, depth = depth, penalty.type = "ratio")
  expect_lt(mean(Y1[cbind(1:n, predict(ppt.ratio, X))]), mean(Y1[cbind(1:n, predict(pt, X))]))
})
