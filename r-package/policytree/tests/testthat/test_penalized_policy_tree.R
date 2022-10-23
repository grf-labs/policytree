test_that("penalized policy tree reward calculation works as expected", {
  n <- 500
  p <- 1
  d <- 10
  X <- matrix(rnorm(n * p), n, p)
  Y1 <- matrix(rnorm(n * d), n, d)
  Y2 <- matrix(10*runif(n * d), n, d)

  numerator <- colMeans(Y1)
  denominator <- pmax(1, sqrt(colMeans(Y2)))
  denominator.diff <- sqrt(colMeans(Y2))

  ppt <- penalized_policy_tree(X, Y1, Y2, depth = 0, penalty.type = "ratio")
  expect_equal(ppt$nodes[[1]]$action, which.max(numerator / denominator))

  # no... TODO
  # ppt.diff <- penalized_policy_tree(X, Y1, Y2, depth = 0, penalty.type = "difference")
  # expect_equal(ppt.diff$nodes[[1]]$action, which.max(numerator + denominator.diff))
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
  ppt.diff <- penalized_policy_tree(X, Y1, Y2 * 0, depth = depth, penalty.type = "difference")
  ppt.diff.lam <- penalized_policy_tree(X, Y1, Y2, depth = depth, penalty.type = "difference")
  expect_equal(predict(pt, X), predict(ppt.ratio, X))
  expect_equal(predict(pt, X), predict(ppt.diff, X))
  expect_equal(predict(pt, X), predict(ppt.diff.lam, X))

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
  ppt.diff <- penalized_policy_tree(X, Y1, Y2 * 0, depth = depth, penalty.type = "difference")
  ppt.diff.lam <- penalized_policy_tree(X, Y1, Y2, depth = depth, penalty.type = "difference", lambda = 0)
  expect_equal(predict(pt, X), predict(ppt.ratio, X))
  expect_equal(predict(pt, X), predict(ppt.diff, X))
  expect_equal(predict(pt, X), predict(ppt.diff.lam, X))

  # invariances
  ppt.ratio <- penalized_policy_tree(X, Y1, Y.ones, depth = depth, penalty.type = "ratio")
  ppt.ratio.scaled <- penalized_policy_tree(X, Y1, Y.ones * 100, depth = depth, penalty.type = "ratio")
  expect_equal(predict(ppt.ratio, X), predict(ppt.ratio.scaled, X))

  ppt.ratio <- penalized_policy_tree(X, Y1, Y2, depth = depth, penalty.type = "ratio")
  expect_lt(mean(Y1[cbind(1:n, predict(ppt.ratio, X))]), mean(Y1[cbind(1:n, predict(pt, X))]))
})
