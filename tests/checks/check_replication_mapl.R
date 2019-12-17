# Test that table 2 from MAPL matches https://arxiv.org/abs/1810.04778
# Usage: `Rscript test_replication_mapl.R` (time with 16 cores: ~ 7 hours)
# grf version: 1.0.1
library(policyTree)

nsim <- 400
n <- c(1000, 1500, 2000, 2500)
p <- 10
tune.parameters <- c("all", "none")
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
    mcf <- multi_causal_forest(X = data$X, Y = data$Y, W = data$action,
                               tune.parameters = tune.parameters)
    Gamma.hat <- get_double_robust_scores(mcf)
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
# n tune        x.Min.     x.1st Qu.      x.Median        x.Mean     x.3rd Qu.        x.Max.
# 1 1000  all  0.0062666667  0.0982416667  0.1302166667  0.1394390000  0.1680416667  0.6151333333
# 2 1500  all  0.0013000000  0.0313583333  0.0956833333  0.0884824167  0.1285583333  0.5220333333
# 3 2000  all  0.0018666667  0.0195000000  0.0476666667  0.0683548333  0.1162250000  0.2634000000
# 4 2500  all  0.0008333333  0.0152416667  0.0282000000  0.0520855833  0.0960916667  0.2444000000
# 5 1000 none  0.0066666667  0.0975833333  0.1288833333  0.1489028333  0.1830833333  0.7165333333
# 6 1500 none  0.0010666667  0.0370000000  0.1018333333  0.0952007500  0.1352833333  0.3595000000
# 7 2000 none  0.0007666667  0.0202583333  0.0487333333  0.0703962500  0.1187416667  0.3541000000
# 8 2500 none -0.0001333333  0.0140583333  0.0314333333  0.0499800833  0.0881250000  0.2044666667
write.csv(res.df, "check_replication_mapl_out.csv")
