# > sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Sierra 10.12.6
#
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
#
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
# [1] xtable_1.8-4   ggplot2_3.2.1  sandwich_2.5-1 policytree_1.0 grf_1.2.0
#
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.4         rstudioapi_0.10    magrittr_1.5       tidyselect_0.2.5
# [5] munsell_0.5.0      colorspace_1.4-1   lattice_0.20-38    R6_2.4.1
# [9] rlang_0.4.1        dplyr_0.8.3        tools_3.6.1        grid_3.6.1
# [13] gtable_0.3.0       withr_2.1.2        digest_0.6.23      lazyeval_0.2.2
# [17] assertthat_0.2.1   tibble_2.1.3       lifecycle_0.1.0    crayon_1.3.4
# [21] Matrix_1.2-17      farver_2.0.1       RColorBrewer_1.1-2 purrr_0.3.3
# [25] glue_1.3.1         labeling_0.3       compiler_3.6.1     pillar_1.4.2
# [29] scales_1.1.0       DiceKriging_1.5.6  pkgconfig_2.0.3    zoo_1.8-7

set.seed(123456)

rm(list = ls())

library(grf)
library(policytree)
library(sandwich)
library(ggplot2)
library(xtable)

#
# This script replicates the analysis of the California GAIN program reported in
# Section 5.1 of Athey & Wager (2021). The data itself, however, cannot be made
# publicly available at this time due to potential privacy conerns.
#

DATAPATH = "~/git/efficient-policy-paper/gain_experiment/"

W = as.numeric(read.csv(paste0(DATAPATH, "labor_treat.csv"))$trainee)
site.raw = read.csv(paste0(DATAPATH, "labor_sample.csv"))
X.raw = read.csv(paste0(DATAPATH, "labor_covariates.csv"), header=FALSE)
Y.all = read.csv(paste0(DATAPATH, "labor_outcomes.csv"), header=FALSE)

oracle_ate = function(pts) {
  site.pts = site.raw[pts,1:3]
  site.scl = scale(site.pts)
  df.pts = data.frame(Y=Y[pts], W=W[pts], site.scl)
  oracle.fit = lm(Y ~ W * ., data = df.pts)
  t.hat = coef(oracle.fit)[2]
  t.se = sqrt(vcovHC(oracle.fit)[2,2])
  c(EST=t.hat, SE=t.se)
}

# numbers indicate #quarters before treatment
x.nms.raw = c(
  sapply(1:10, function(iii) paste0("earnings_per_quarter_", iii)),
  sapply(1:10, function(iii) paste0("tcpp_", iii)),
  sapply(1:10, function(iii) paste0("earnings_above_min_wage_", iii)),
  sapply(4:1, function(iii) paste0("paid_", iii)),
  sapply(4:1, function(iii) paste0("adcpc_", iii)),
  "female",
  "has_high_school_diploma",
  "x1chld",
  "single",
  "has_children",
  "xchld05", "grd1720", "grade16", "grd1315", "grade12",
  "grde911", "white", "hisp", "black", "age", "agesq")

site = factor(as.matrix(site.raw) %*% 1:4)


# omit either unknown or redundant features
unk.cols = c(21:30, 35:38, 41)
redundant.cols = c(11:20, 54)

x.nms = x.nms.raw[-c(unk.cols, redundant.cols)]
X = X.raw[,-c(unk.cols, redundant.cols)]
colnames(X) = sapply(1:ncol(X), function(ii) paste0("V", ii))

# The first 36 columns represent quarterly earnings in $1000, over 9 years.
Y = rowMeans(Y.all[,1:36])

# The next 36 columns represent employment status, over 9 years.
Yemp = rowMeans(Y.all[,37:72])

#
# TABLE 1 ANALYSIS
#
# Race looks like a confounder. Table 1 reports output from the two
# comparisons below.
#

t.test(Y[W == 0 & X[, which(x.nms == "white")] == 0],
       Y[W == 0 & X[, which(x.nms == "white")] == 1])

# Welch Two Sample t-test
#
# data:  Y[W == 0 & X[, which(x.nms == "white")] == 0] and Y[W == 0 & X[, which(x.nms == "white")] == 1]
# t = -2.2959, df = 2603.5, p-value = 0.02176
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.21762299 -0.01712792
# sample estimates:
#   mean of x mean of y
# 0.7855096 0.9028851

t.test(W[X[, which(x.nms == "white")] == 0],
       W[X[, which(x.nms == "white")] == 1])

# Welch Two Sample t-test
#
# data:  W[X[, which(x.nms == "white")] == 0] and W[X[, which(x.nms == "white")] == 1]
# t = -7.3947, df = 15560, p-value = 1.49e-13
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.05673615 -0.03296015
# sample estimates:
#   mean of x mean of y
# 0.7647639 0.8096121


n = length(W)
nfold = 10
foldid = sample(rep(1:nfold, floor(n/nfold)))

# Add a cost OFFSET to treatment that roughly matches the ATE

OFFSET = 0.14
print(oracle_ate(1:n))
#      EST.W         SE
# 0.14492326 0.03046564

#
# first train a causal forest, and the form doubly robust scores
#

tune.parameters = c("mtry", "min.node.size", "alpha", "imbalance.penalty")
propf = regression_forest(X, W, tune.parameters = tune.parameters)
ehat = predict(propf)$predictions
margf = regression_forest(X, Y, tune.parameters = tune.parameters)
mhat = predict(margf)$predictions
cf = causal_forest(X, Y, W, Y.hat = mhat, W.hat = ehat, tune.parameters = tune.parameters)
tauhat = predict(cf)$predictions

Gamma.dr = tauhat - OFFSET +
  W / ehat * (Y - mhat - (1 - ehat) * tauhat) -
  (1 - W) / (1 - ehat) * (Y - mhat + ehat * tauhat)

Gamma.ipw = W / ehat * Y - (1 - W) / (1 - ehat) * Y - OFFSET



site.freq.treat = t(site.raw) %*% W / length(W)
site.freq = colMeans(site.raw)
site.prop = site.freq.treat / site.freq
ehat.oracle = as.matrix(site.raw) %*% as.numeric(site.prop)


#
# don't split on sensitive features
#

sensitive.feat = c("female", "white", "hisp", "black", "age")
safe.feat = which(!x.nms %in% sensitive.feat)
safe.nms = x.nms[safe.feat]
X.safe  = X[,safe.feat]

#
# train trees via DR, to depth 1 & 2
#

tree.dr.1 = policy_tree(X.safe, cbind(-Gamma.dr, Gamma.dr), depth = 1)
policy.dr.1 = predict(tree.dr.1, X.safe) - 1

tree.dr.2 = policy_tree(X.safe, cbind(-Gamma.dr, Gamma.dr), depth = 2)
policy.dr.2 = predict(tree.dr.2, X.safe) - 1

tree.ipw.1 = policy_tree(X.safe, cbind(-Gamma.ipw, Gamma.ipw), depth = 1)
policy.ipw.1 = predict(tree.ipw.1, X.safe) - 1

tree.ipw.2 = policy_tree(X.safe, cbind(-Gamma.ipw, Gamma.ipw), depth = 2)
policy.ipw.2 = predict(tree.ipw.2, X.safe) - 1

tauhat.out.of.fold = rep(NA, n)
mhat.out.of.fold = rep(NA, n)
ehat.out.of.fold = rep(NA, n)

# for depth 1 & 2 DR trees
policy.oob.dr.1 = rep(NA, n)
trees.dr.1 = as.list(rep(NA, nfold))
policy.oob.dr.2 = rep(NA, n)
trees.dr.2 = as.list(rep(NA, nfold))

# for depth 1 & 2 IPW trees
policy.oob.ipw.1 = rep(NA, n)
trees.ipw.1 = as.list(rep(NA, nfold))
policy.oob.ipw.2 = rep(NA, n)
trees.ipw.2 = as.list(rep(NA, nfold))

for(fold in 1:nfold) {

  idx = which(foldid != fold & foldid > 0)
  test = which(foldid == fold)

  tr.dr.1 = policy_tree(X.safe[idx,], cbind(-Gamma.dr[idx], Gamma.dr[idx]), depth = 1)
  po.dr.1 = predict(tr.dr.1, X.safe[test,]) - 1
  trees.dr.1[[fold]] = tr.dr.1
  policy.oob.dr.1[test] = po.dr.1

  print("depth 1")

  tr.dr.2 = policy_tree(X.safe[idx,], cbind(-Gamma.dr[idx], Gamma.dr[idx]), depth = 2)
  po.dr.2 = predict(tr.dr.2, X.safe[test,]) - 1
  trees.dr.2[[fold]] = tr.dr.2
  policy.oob.dr.2[test] = po.dr.2

  print("depth 2")

  tr.ipw.1 = policy_tree(X.safe[idx,], cbind(-Gamma.ipw[idx], Gamma.ipw[idx]), depth = 1)
  po.ipw.1 = predict(tr.ipw.1, X.safe[test,]) - 1
  trees.ipw.1[[fold]] = tr.ipw.1
  policy.oob.ipw.1[test] = po.ipw.1

  print("depth 1 IPW")

  tr.ipw.2 = policy_tree(X.safe[idx,], cbind(-Gamma.ipw[idx], Gamma.ipw[idx]), depth = 2)
  po.ipw.2 = predict(tr.ipw.2, X.safe[test,]) - 1
  trees.ipw.2[[fold]] = tr.ipw.2
  policy.oob.ipw.2[test] = po.ipw.2

  print("depth 2 IPW")
}


#
# Look at consistency of predictions
#

all.tree.preds.dr.1 = sapply(1:nfold, function(fold) {
  predict(trees.dr.1[[fold]], X.safe) - 1
})

all.tree.preds.dr.2 = sapply(1:nfold, function(fold) {
  predict(trees.dr.2[[fold]], X.safe) - 1
})

all.tree.preds.ipw.1 = sapply(1:nfold, function(fold) {
  predict(trees.ipw.1[[fold]], X.safe) - 1
})

all.tree.preds.ipw.2 = sapply(1:nfold, function(fold) {
  predict(trees.ipw.2[[fold]], X.safe) - 1
})

avg.pred.dr.1 = rowMeans(all.tree.preds.dr.1)
avg.pred.dr.2 = rowMeans(all.tree.preds.dr.2)
avg.pred.ipw.1 = rowMeans(all.tree.preds.ipw.1)
avg.pred.ipw.2 = rowMeans(all.tree.preds.ipw.2)

pdf("agreement_dr_1.pdf")
ggplot(data.frame("average_prediction"=as.numeric(avg.pred.dr.1),
                  "full_data_policy"=factor(policy.dr.1)),
       aes(x=average_prediction, fill=full_data_policy)) +
  geom_histogram(alpha = 0.9, position="stack", binwidth = 0.1,
                 colour='black',size=0.5) +
  scale_fill_brewer(palette="PuRd") + theme(text = element_text(size=16)) +
  labs(x="mean holdout policy", fill="policy") +
  ylim(0, 9000)
dev.off()

pdf("agreement_dr_2.pdf")
ggplot(data.frame("average_prediction"=as.numeric(avg.pred.dr.2),
                  "full_data_policy"=factor(policy.dr.2)),
       aes(x=average_prediction, fill=full_data_policy)) +
  geom_histogram(alpha = 0.9, position="stack", binwidth = 0.1,
                 colour='black',size=0.5) +
  scale_fill_brewer(palette="PuRd") + theme(text = element_text(size=16)) +
  labs(x="mean holdout policy", fill="policy") +
  ylim(0, 9000)
dev.off()

pdf("agreement_ipw_1.pdf")
ggplot(data.frame("average_prediction"=as.numeric(avg.pred.ipw.1),
                  "full_data_policy"=factor(policy.ipw.1)),
       aes(x=average_prediction, fill=full_data_policy)) +
  geom_histogram(alpha = 0.9, position="stack", binwidth = 0.1,
                 colour='black',size=0.5) +
  scale_fill_brewer(palette="PuRd") + theme(text = element_text(size=16)) +
  labs(x="mean holdout policy", fill="policy") +
  ylim(0, 9000)
dev.off()

pdf("agreement_ipw_2.pdf")
ggplot(data.frame("average_prediction"=as.numeric(avg.pred.ipw.2),
                  "full_data_policy"=factor(policy.ipw.2)),
       aes(x=average_prediction, fill=full_data_policy)) +
  geom_histogram(alpha = 0.9, position="stack", binwidth = 0.1,
                 colour='black',size=0.5) +
  scale_fill_brewer(palette="PuRd") + theme(text = element_text(size=16)) +
  labs(x="mean holdout policy", fill="policy") +
  ylim(0, 9000)
dev.off()



table("is high school graduate"=X[,16], "has children"=X[,18])
#                      has children
# is high school graduate    0    1
#                       0 3706 5771
#                       1 4392 5301


#
# evaluate policies
#

Gamma.oracle = tauhat - OFFSET +
  W / ehat.oracle * (Y - mhat - (1 - ehat.oracle) * tauhat) -
  (1 - W) / (1 - ehat.oracle) * (Y - mhat + ehat.oracle * tauhat)

get_advantage_oracle = function(policy) {
  mu = mean((2 * policy - 1) * Gamma.oracle)
  se = sqrt(var((2 * policy - 1) * Gamma.oracle) / length(Gamma.oracle))
  c(point.estimate=mu, std.err=se)
}

get_advantage = function(policy) {
  mu = mean((2 * policy - 1) * Gamma.dr)
  se = sqrt(var((2 * policy - 1) * Gamma.dr) / length(Gamma.dr))
  c(point.estimate=mu, std.err=se)
}

cf.treat = as.numeric(tauhat - OFFSET > 0)

advs = rbind(c(get_advantage(cf.treat), get_advantage_oracle(cf.treat), mean(cf.treat)),
             c(get_advantage(policy.oob.ipw.1), get_advantage_oracle(policy.oob.ipw.1), mean(policy.oob.ipw.1)),
             c(get_advantage(policy.oob.ipw.2), get_advantage_oracle(policy.oob.ipw.2), mean(policy.oob.ipw.2)),
             c(get_advantage(policy.oob.dr.1), get_advantage_oracle(policy.oob.dr.1), mean(policy.oob.dr.1)),
             c(get_advantage(policy.oob.dr.2), get_advantage_oracle(policy.oob.dr.2), mean(policy.oob.dr.2)))

advs.print = data.frame(
  method=c("causal forest", "IPW depth 1", "IPW depth 2", "AIPW depth 1", "AIPW depth 2"),
  advantage=sapply(1:5, function(ii)
    paste("$", round(advs[ii,1], 3), "\\pm", round(advs[ii,2], 3), "$")),
  oracle_advantage=sapply(1:5, function(ii)
    paste("$", round(advs[ii,3], 3), "\\pm", round(advs[ii,4], 3), "$")))


#
# TABLE 2
#

print(xtable(advs.print), include.rownames = F, sanitize.text.function = identity, file = "table_2.tex")


save.image("gain_output.RData")
