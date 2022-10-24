# Check that the following tree search code path passes under valgrind.
# Usage:
# R -d "valgrind --tool=memcheck --leak-check=full" --vanilla  < test_policy_tree_valgrind.R
library(policytree)
depth <- 3
n <- 65
p <- 5
d <- 4
# 1/2 continuous/discrete X
X.c <- matrix(rnorm(n * p), n, p)
X.d <- matrix(sample(10:15, n * p, replace = TRUE), n, p)
X <- cbind(X.c, X.d, rbinom(n, 1, 0.5))
Y <- matrix(rnorm(n * d), n, d)

tree <- policy_tree(X, Y, depth = depth)
pp <- predict(tree, X)
tree.step <- policy_tree(X, Y, depth = depth, split.step = 2)
pp.step <- predict(tree.step, X)

tree.penalized <- penalized_policy_tree(X, Y, abs(Y / 10), depth = 2)
pp.penalized <- predict(tree.penalized, X)
