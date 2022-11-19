test_that("hybrid_policy_tree works as expected", {
  n <- 500
  p <- 2
  d <- 42
  X <- round(matrix(rnorm(n * p), n, p), 1)
  Y <- matrix(runif(n * d), n, d)

  # a manually constructed (3, 2) hybrid tree gives identical predictions.
  htree <- hybrid_policy_tree(X, Y, depth = 3, search.depth = 2)
  hpp <- predict(htree, X)

  base.tree <- policy_tree(X, Y, depth = 2)
  svar <- base.tree$nodes[[1]]$split_variable
  sval <- base.tree$nodes[[1]]$split_value
  left <- X[, svar] <= sval
  right <- !left
  left.tree <- policy_tree(X[left, ], Y[left, ], depth = 2)
  right.tree <- policy_tree(X[right, ], Y[right, ], depth = 2)
  pp <- rep(NA, n)
  for (sample in seq_len(n)) {
    if (X[sample, svar] <= sval) {
      pp[sample] <- predict(left.tree, X[sample, , drop = FALSE])
    } else {
      pp[sample] <- predict(right.tree, X[sample, , drop = FALSE])
    }
  }
  expect_equal(hpp, pp)
  # is internally consistent with predicted node ids.
  hpp.node <- predict(htree, X, type = "node.id")
  values <- aggregate(Y, by = list(leaf.node = hpp.node), FUN = mean)
  best <- apply(values[, -1], 1, FUN = which.max)
  expect_equal(best[match(hpp.node, values[, 1])], hpp)

  # uses node labels that are the same as the printed labels.
  printed.node.id <- lapply(seq_along(htree$nodes), function(i) {
    if (htree$nodes[[i]]$is_leaf) i
  })
  expect_true(all(unique(hpp.node) %in% printed.node.id))

  # search.depth = 1 when a single split is optimal is identical to a depth 1 policy_tree
  n <- 250
  p <- 2
  d <- 2
  X <- matrix(runif(n * p), n, p)
  Y <- matrix(-100, n, d)
  Y[X[, 1] <= 0.5, 1] <- 42
  Y[X[, 1] > 0.5, 2] <- 42

  tree <- policy_tree(X, Y, depth = 1)
  htree <- hybrid_policy_tree(X, Y, depth = 3, search.depth = 1, verbose = FALSE)
  expect_equal(unlist(htree$nodes), unlist(tree$nodes))
  htree <- hybrid_policy_tree(X, Y, depth = 4, search.depth = 1, verbose = FALSE)
  expect_equal(unlist(htree$nodes), unlist(tree$nodes))
})


test_that("hybrid_policy_tree reward estimates are reasonable", {
  n <- 1000
  p <- 2
  d <- 3
  X <- round(matrix(runif(n * p), n, p), 1)
  Y <- matrix(rnorm(n * d), n, d)

  tree <- policy_tree(X, Y, depth = 2)
  reward.tree <- mean(Y[cbind(1:n, predict(tree, X))])

  htree3 <- hybrid_policy_tree(X, Y, depth = 3, search.depth = 2)
  reward.htree3 <- mean(Y[cbind(1:n, predict(htree3, X))])

  htree4 <- hybrid_policy_tree(X, Y, depth = 4, search.depth = 2)
  reward.htree4 <- mean(Y[cbind(1:n, predict(htree4, X))])

  htree5 <- hybrid_policy_tree(X, Y, depth = 5, search.depth = 2)
  reward.htree5 <- mean(Y[cbind(1:n, predict(htree5, X))])

  expect_gte(reward.htree3, 1.1 * reward.tree)
  expect_gte(reward.htree4, 1.2 * reward.tree)
  expect_gte(reward.htree5, 1.3 * reward.tree)
})


test_that("hybrid_policy_tree bindings works", {
  n <- 250
  p <- 2
  d <- 3
  X <- matrix(runif(n * p), n, p)
  Y <- matrix(runif(n * d), n, d)

  hpt <- hybrid_policy_tree(X, Y, depth = 3, search.depth = 2)
  pp <- predict(hpt, X)
  p <- capture.output(print(hpt))
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    plot(hpt)
  }

  hpt <- hybrid_policy_tree(X, Y, depth = 4, search.depth = 2)
  pp <- predict(hpt, X)
  p <- capture.output(print(hpt))
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    plot(hpt)
  }

  expect_equal(1, 1)
})


test_that("hybrid_policy_tree utils are internally consistent", {
  n <- 100
  p <- 2
  d <- 3
  X <- matrix(runif(n * p), n, p)
  Y <- matrix(sample(c(-1, 0, 1), n * d, TRUE), n, d)

  depth <- 2
  tree <- policy_tree(X, Y, depth = depth)
  expect_equal(convert_nodes(tree[["nodes"]], depth)[[2]], tree[["_tree_array"]], tolerance = 1e-16)
  tree.nodes <- lapply(seq_along(tree$nodes), function(i) {
    node <- tree$nodes[[i]]
    node$has_subtree <- FALSE
    node
  })
  expect_equal(unlist(unpack_tree(tree.nodes)), unlist(tree$nodes))

  depth <- 3
  tree <- policy_tree(X, Y, depth = depth)
  expect_equal(convert_nodes(tree[["nodes"]], depth)[[2]], tree[["_tree_array"]], tolerance = 1e-16)
  tree.nodes <- lapply(seq_along(tree$nodes), function(i) {
    node <- tree$nodes[[i]]
    node$has_subtree <- FALSE
    node
  })
  expect_equal(unlist(unpack_tree(tree.nodes)), unlist(tree$nodes))
})
