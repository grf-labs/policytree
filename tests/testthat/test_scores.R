test_that("everything runs", {
  n <- 250
  p <- 10
  X <- matrix(runif(n * p), n, p)
  Y <- runif(n)
  W <- rbinom(n, 1, 0.5)
  Z <- rbinom(n, 1, 0.5)

  cf <- grf::causal_forest(X, Y, W)
  conditional_means(cf)
  double_robust_scores(cf)

  iv <- grf::instrumental_forest(X, Y, W, Z)
  conditional_means(iv)
  double_robust_scores(iv)

  expect_equal(1, 1)
})
