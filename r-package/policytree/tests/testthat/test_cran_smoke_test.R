test_that("a simple workflow works on CRAN", {
  n <- 500
  p <- 10
  d <- 3
  X <- matrix(runif(n * p), n, p)
  Y <- runif(n)
  W <- sample(1:d, n, replace = TRUE)

  # Some simple smoke tests
  mcf <- multi_causal_forest(X, Y, W)
  tau.hat <- predict(mcf)
  mu.hat <- conditional_means(mcf)
  gamma.hat <- double_robust_scores(mcf)
  p <- capture.output(print(mcf))
  pt <- policy_tree(X, gamma.hat, depth = 2)
  p2 <- capture.output(print(pt))
  pt.actions <- predict(pt, X)

  W <- rbinom(n, 1, 0.5)
  Z <- rbinom(n, 1, 0.5)
  cf <- grf::causal_forest(X, Y, W)
  mu.cf <- conditional_means(cf)
  dr.cf <- double_robust_scores(cf)
  iv <- grf::instrumental_forest(X, Y, W, Z)
  mu.iv <- conditional_means(iv)
  dr.iv <- double_robust_scores(iv)

  expect_equal(ncol(tau.hat$predictions), d)
  expect_equal(ncol(mu.hat), d)
  expect_equal(ncol(gamma.hat), d)
  expect_equal(length(pt.actions), n)

  expect_equal(ncol(mu.cf), 2)
  expect_equal(ncol(dr.cf), 2)
  expect_equal(ncol(mu.iv), 2)
  expect_equal(ncol(dr.iv), 2)
})
