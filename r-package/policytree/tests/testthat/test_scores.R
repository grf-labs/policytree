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

test_that("multi_arm_causal_forest scores works as expected", {
  n <- 1500
  p <- 10
  data <- gen_data_mapl(n, p)
  mcf <- grf::multi_arm_causal_forest(data$X, data$Y, as.factor(data$action))

  Gamma.matrix <- double_robust_scores(mcf)
  tree <- policy_tree(data$X, Gamma.matrix, 2)
  pp <- predict(tree, data$X)

  reward <- mean(data$mu.all[cbind(1:n, pp)])
  expect_true(all(reward > colMeans(data$mu.all)))
  expect_equal(unname(Gamma.matrix[, -1] - Gamma.matrix[, 1]),
               unname(grf::get_scores(mcf)[,,]),
               tolerance = 1e-15)
})

test_that("multi_arm_causal_forest scores with multiple outcomes works as expected", {
  n <- 250
  p <- 10
  data <- gen_data_mapl(n, p)
  X <- data$X
  W <- as.factor(data$action)
  Y <- data.frame(Y1 = data$Y, Y2 = runif(n), orange = rep(42, n))
  mcf <- grf::multi_arm_causal_forest(X, Y, W)

  expect_equal(double_robust_scores(mcf), double_robust_scores(mcf, outcome = "Y1"))
  expect_equal(double_robust_scores(mcf, outcome = 2), double_robust_scores(mcf, outcome = "Y2"))
  expect_equal(double_robust_scores(mcf, outcome = 3), double_robust_scores(mcf, outcome = "orange"))
})

test_that("causal survival forest on complete data is ~same as causal forest", {
  n <- 2000
  p <- 5
  X <- round(matrix(runif(n * p), n, p), 2)
  W <- rbinom(n, 1, 1 / (1 + exp(X[, 3])))
  tau <- 1 / (1 + exp((X[, 1] + X[, 2]) / 2)) + 0.5
  Y <- X[, 3] + W * tau + runif(n)
  cf <- grf::causal_forest(X, Y, W)
  dr.cf <- double_robust_scores(cf)
  tree.cf <- policy_tree(X, dr.cf, 2)
  pp.cf <- predict(tree.cf, X)

  csf <- grf::causal_survival_forest(X, Y, W, rep(1, n), horizon = max(Y))
  dr.csf <- double_robust_scores(csf)
  tree.csf <- policy_tree(X, dr.csf)
  pp.csf <- predict(tree.csf, X)

  pp.eq <- as.integer(pp.cf == pp.csf)
  expect_gte(mean(pp.eq), 0.95)
  expect_equal(conditional_means(cf), conditional_means(csf), tolerance = 0.05)
})
