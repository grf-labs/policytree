test_that("hybrid solver bindings run", {
  n <- 50
  p <- 10
  d <- 3
  X <- matrix(runif(n * p), n, p)
  Y <- matrix(runif(n * d), n, d)
  pt <- policy_tree(X, Y, depth = 2, solver = "hybrid", 1, 1, 1)
  p <- capture.output(print(pt))

  p1 <- predict(pt, X)
  p2 <- predict(pt, X[1, , drop = FALSE])

  p <- capture.output(print(pt))
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    plot(pt)
  }

  expect_equal(1, 1)
})
