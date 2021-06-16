# Test that table 2 from MAPL matches https://arxiv.org/abs/1810.04778
# Usage: `Rscript test_replication_mapl.R`
# policytree version: 1.1
# grf version: 2.0
library(policytree)
library(grf)

nsim <- 400
n <- c(1000, 1500, 2000, 2500)
p <- 10
tune.parameters <- c("none")
params <- expand.grid(n = n, tune.parameters = tune.parameters, stringsAsFactors = FALSE)

# Best tree
n.best <- 10000
data.best <- gen_data_mapl(n.best, p)
tree.best <- policy_tree(X = data.best$X, Gamma = data.best$mu.all)
# test data
n.test <- 15000
data.test <- gen_data_mapl(n.test, p)

index.policy.best <- cbind(1:n.test, predict(tree.best, data.test$X))
reward.policy.best <- mean(data.test$mu.all[index.policy.best])

res <- parallel::mclapply(1:nrow(params), function(i) {
  n <- params$n[i]
  tune.parameters <- params$tune.parameters[i]
  out <- parallel::mclapply(1:nsim, function(ii) {
    data <- gen_data_mapl(n, p)
    mcf <- multi_arm_causal_forest(X = data$X, Y = data$Y, W = as.factor(data$action))
    Gamma.hat <- double_robust_scores(mcf)
    tree <- policy_tree(X = data$X, Gamma = Gamma.hat)

    index.policy <- cbind(1:n.test, predict(tree, data.test$X))
    reward.policy <- mean(data.test$mu.all[index.policy])

    reward.policy.best - reward.policy
  }, mc.cores = 2)
  out <- unlist(out)

  data.frame(regret = out, n = n, tune.parameters = tune.parameters)
}, mc.cores = nrow(params))

res.df <- do.call(rbind, res)
print(
  aggregate(res.df$regret, by = list(n = res.df$n, tune = res.df$tune.parameters), FUN = summary)
)
