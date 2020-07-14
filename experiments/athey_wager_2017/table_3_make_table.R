library(xtable)

rm(list = ls())

setups = 1:2
nvals = c(600, 1800, 5400, 16200)

all.res = Reduce(rbind, lapply (setups, function(setup) {
    Reduce(rbind, lapply (nvals, function(n) {
        res = read.csv(paste0("table_3_raw_output/out_n", n, "_setup", setup, ".csv"))
        theta = mean(res[,"theta.oracle"])
        c(setup = setup, n = n,
          bias = colMeans(res[, c("theta.plugin", "theta.weight", "theta.DR")] - theta),
          rmse = sqrt(colMeans((res[, c("theta.plugin", "theta.weight", "theta.DR")] - theta)^2)),
          std.err = sqrt(mean((res[, "theta.DR"] - theta)^2 / res[, "se.DR"]^2)),
          policy.value = mean(res[, "policy.value"]))
    }))
}))

all.res = all.res[,c(1, 2, 3, 6, 4, 7, 5, 8, 9, 10)]

xtab = xtable(all.res, digits = c(0, 0, 0, rep(3, 6), 2, 3))
print(xtab, include.rownames = FALSE, hline.after = c(4, 8), file = "table_3.tex")
