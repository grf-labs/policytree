test_that("everything runs", {
  n <- 250
  p <- 10
  d <- 3
  X <- matrix(runif(n * p), n, p)
  Y <- runif(n)
  W <- sample(1:d, n, replace = TRUE)

  # Some simple smoke tests
  expect_warning(mcf <- multi_causal_forest(X, Y, W))
  predict(mcf)
  conditional_means(mcf)
  double_robust_scores(mcf)
  p <- capture.output(print(mcf))

  multi_causal_forest(X, Y, W, orthog.boosting = TRUE)

  multi_causal_forest(X, Y, W, W.hat = c(1 / 3, 1 / 3, 1 / 3))

  multi_causal_forest(X, Y, W, W.hat = matrix(1 / 3, n, d))

  #
  predictions.oob <- predict(mcf)
  predictions <- predict(mcf, X)
  predictions.var <- predict(mcf, X, estimate.variance = TRUE)

  expect_equal(length(predictions.oob), 3) # grf returns 3 estimates
  expect_equal(length(predictions), 1) # grf returns 1 estimate
  expect_equal(length(predictions.var), 2) # grf returns 2 estimates

  expect_equal(ncol(predictions.oob$predictions), d)
  expect_equal(ncol(predictions$predictions), d)
  expect_equal(ncol(predictions.var$predictions), d)
})

test_that("predictions have not changed", {
  set.seed(123)
  n <- 250
  p <- 10
  d <- 3
  X <- matrix(runif(n * p), n, p)
  Y <- runif(n)
  W <- sample(1:d, n, replace = TRUE)
  expect_warning(mcf <- multi_causal_forest(X = X, Y = Y, W = W, seed = 123))
  tau <- predict(mcf)$predictions

  expect_equal(mean(colMeans(tau)), -0.001, tolerance = 1e-3)
})
