# Test that figure 2 from EPL matches https://arxiv.org/abs/1702.02896
# Usage: `Rscript check_replication_epl.R` (time with 20 cores: ~ 6 hours)
# policytree varsion: 1.0.0   grf version: 1.2.0
library(policytree)

nsim <- 200
n <- c(500, 1000, 2000, 4000, 8000, 16000)
dgp <- c("continuous", "jump")
params <- expand.grid(n = n, dgp = dgp, stringsAsFactors = FALSE)
params$mc.cores <- 1
params[params$n == max(n), "mc.cores"] <- 6

res <- parallel::mclapply(1:nrow(params), function(i) {
  n <- params$n[i]
  dgp <- params$dgp[i]
  mc.cores <- params$mc.cores[i]
  out <- parallel::mclapply(1:nsim, function(ii) {
    data <- gen_data_epl(n, type = dgp)
    data.test <- gen_data_epl(n, type = dgp)
    cf <- grf::instrumental_forest(data$X, data$Y, data$W, data$Z)
    gamma <- double_robust_scores(cf)

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

write.csv(res.df, "figure_2_raw_output.csv")
