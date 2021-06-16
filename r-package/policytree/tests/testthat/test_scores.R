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
})
