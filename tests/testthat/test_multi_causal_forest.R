test_that("everything runs", {
  n <- 250
  p <- 10
  d <- 3
  X <- matrix(runif(n * p), n, p)
  Y <- runif(n)
  W <- sample(1:d, n, replace = TRUE)

  mcf <- multi_causal_forest(X, Y, W)
  predict(mcf)
  get_conditional_means(mcf)
  get_double_robust_scores(mcf)
  p <- capture.output(print(mcf))

  multi_causal_forest(X, Y, W, orthog.boosting = TRUE)

  multi_causal_forest(X, Y, W, W.hat = c(1 / 3, 1 / 3, 1 / 3))

  multi_causal_forest(X, Y, W, W.hat = matrix(1 / 3, n, d))

  expect_equal(1, 1)
})

test_that("predictions have not changed", {
  set.seed(123)
  n <- 250
  p <- 10
  d <- 3
  X <- matrix(runif(n * p), n, p)
  Y <- runif(n)
  W <- sample(1:d, n, replace = TRUE)
  mcf <- multi_causal_forest(X = X, Y = Y, W = W, seed = 123)
  tau <- predict(mcf)$predictions

  expect_equal(mean(colMeans(tau)), -0.001, tolerance = 1e-3)
})
