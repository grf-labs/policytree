# > sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# 
# Matrix products: default
# BLAS:   /software/free/R/R-3.6.1/lib/R/lib/libRblas.so
# LAPACK: /software/free/R/R-3.6.1/lib/R/lib/libRlapack.so
# 
# locale:
# [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
# [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
# [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
# [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
# 
# attached base packages:
# [1] parallel  stats     graphics  grDevices utils     datasets  methods  
# [8] base     
# 
# other attached packages:
# [1] grf_1.2.0        EQL_1.0-1        ttutils_1.0-1    doMC_1.3.6      
# [5] iterators_1.0.12 foreach_1.5.0    policytree_1.0   glmnet_4.0-2    
# [9] Matrix_1.2-17   
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.3        lattice_0.20-38   codetools_0.2-16  grid_3.6.1       
# [5] splines_3.6.1     survival_2.44-1.1 compiler_3.6.1    shape_1.4.4   
#
# Replicate figure 2 from AW17 https://arxiv.org/abs/1702.02896
# Time with 20 cores: ~ 6 hours

library(policytree)

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
