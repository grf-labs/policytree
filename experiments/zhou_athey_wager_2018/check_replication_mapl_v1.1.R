# Test that table 2 from MAPL matches https://arxiv.org/abs/1810.04778
# Usage: `Rscript test_replication_mapl.R`
# policytree version: 1.1.0
# grf version: 2.0.0
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
# Run one
#      n tune       x.Min.    x.1st Qu.     x.Median       x.Mean    x.3rd Qu.       x.Max.
# 1 1000 none 0.0035000000 0.0763833333 0.1539333333 0.1531095833 0.1936083333 0.6988000000
# 2 1500 none 0.0008000000 0.0289166667 0.0710833333 0.0939141667 0.1542083333 0.3604000000
# 3 2000 none 0.0019333333 0.0181250000 0.0340166667 0.0580348333 0.0812666667 0.2674000000
# 4 2500 none 0.0005666667 0.0159166667 0.0309666667 0.0445355833 0.0541250000 0.2104666667
# Run two
#      n tune        x.Min.     x.1st Qu.      x.Median        x.Mean     x.3rd Qu.        x.Max.
# 1 1000 none -0.0002000000  0.0727583333  0.1528333333  0.1543220833  0.1996583333  0.8274333333
# 2 1500 none  0.0005666667  0.0279500000  0.0578166667  0.0862634167  0.1455666667  0.4349000000
# 3 2000 none  0.0001333333  0.0158500000  0.0345000000  0.0579962500  0.0860333333  0.3072333333
# 4 2500 none -0.0007000000  0.0120083333  0.0231166667  0.0383813333  0.0473166667  0.2062333333
