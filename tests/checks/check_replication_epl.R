# Test that figure 3 from EPL matches https://arxiv.org/abs/1702.02896
# Usage: `Rscript check_replication_epl.R` (time with 20 cores: ~ 6 hours)
# grf version: 1.0.1
library(policyTree)

nsim <- 200
n <- c(500, 1000, 2000, 4000, 8000, 16000)
dgp <- c("continuous", "jump")
params <- expand.grid(n = n, dgp = dgp, stringsAsFactors = FALSE)
params$mc.cores <- 1
params[params$n == max(n), "mc.cores"] <- 4

res <- parallel::mclapply(1:nrow(params), function(i) {
  n <- params$n[i]
  dgp <- params$dgp[i]
  mc.cores <- params$mc.cores[i]
  out <- parallel::mclapply(1:nsim, function(ii) {
    data <- gen_data_epl(n, type = dgp)
    data.test <- gen_data_epl(n, type = dgp)
    cf <- grf::instrumental_forest(data$X, data$Y, data$W, data$Z)
    gamma <- get_double_robust_scores(cf)

    tree <- policy_tree(data$X, gamma)
    pi.x <- predict(tree, data.test$X) - 1 # EPL: 0=control, 1=treated
    reward.policy <- (2 * pi.x - 1) * data.test$tau

    Ai <- mean(reward.policy)

    Ai
  }, mc.cores = mc.cores)
  out <- unlist(out)

  data.frame(Ai = out, n = n, dgp = dgp)
}, mc.cores = nrow(params))

res.df <- do.call(rbind, res)
print(
  aggregate(res.df$Ai, by = list(n = res.df$n, dgp = res.df$dgp), FUN = summary)
)
#        n        dgp       x.Min.    x.1st Qu.     x.Median       x.Mean    x.3rd Qu.       x.Max.
# 1    500 continuous -0.098345547  0.007174275  0.072657814  0.077427644  0.142327776  0.273062293
# 2   1000 continuous -0.106212105  0.053657195  0.135869311  0.128455108  0.197580857  0.328833840
# 3   2000 continuous -0.058494253  0.115066592  0.170697575  0.169711138  0.227199592  0.338179634
# 4   4000 continuous -0.013110722  0.182613505  0.227495603  0.230105290  0.291316779  0.342105596
# 5   8000 continuous  0.097593448  0.236835372  0.294337843  0.278607855  0.320402241  0.347309067
# 6  16000 continuous  0.190112919  0.293919664  0.318892805  0.305177060  0.331953919  0.344124360
# 7    500       jump -0.060000000 -0.008000000  0.012000000  0.046900000  0.036000000  0.444000000
# 8   1000       jump -0.050000000 -0.007000000  0.015000000  0.113070000  0.210000000  0.484000000
# 9   2000       jump -0.025000000  0.010000000  0.225500000  0.221515000  0.393125000  0.493500000
# 10  4000       jump -0.007250000  0.339812500  0.426625000  0.374656250  0.455437500  0.491750000
# 11  8000       jump  0.321375000  0.446218750  0.464937500  0.458548750  0.479593750  0.498375000
# 12 16000       jump  0.430687500  0.471671875  0.484187500  0.479122917  0.490421875  0.498812500
write.csv(res.df, "check_replication_epl_out.csv")
