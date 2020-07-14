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

set.seed(1234)

rm(list = ls())

library(glmnet)
library(policytree)
library(foreach)
library(doMC)
library(EQL)

registerDoMC(cores=10)

# These basis functions are used for series regression
generate.basis = function(X, order=3) {
  H = lapply(1:ncol(X), function(j) {
    sapply(1:order, function(k) EQL::hermite(X[,j], k, prob = TRUE) / sqrt(factorial(k)))
  })
  polys = lapply(1:order, function(r) {
    partitions = combn(r + ncol(X) -1, ncol(X) - 1,
                       function(vec) c(vec, r + ncol(X)) - c(0, vec) - 1)
    elems = sapply(1:ncol(partitions), function(iter) {
      part = partitions[,iter]
      idx = which(part > 0)
      elem = H[[idx[1]]][,part[idx[1]]]
      if (length(idx) > 1) {
        for (id in idx[-1]) {
          elem = elem * H[[id]][,part[id]]
        }
      }
      elem
    })
    scale(elems) / sqrt(ncol(elems)) / r
  })
  Reduce(cbind, polys)
}

wfn_list = list(
  function(x) {
    pmin(6.99, pmax(-4.99, 3 / (1 + 3/exp((x[1] + x[3] + rnorm(1)))) + rnorm(1)))
  },
  function(x) {
    pmin(6.99, pmax(-4.99, 3 / (1 + 3/exp((x[1] + x[3]))) + rnorm(1)))
  }
)

setups = 1:2
nvals = c(600, 1800, 5400, 16200)
NREP = 40

n.eval = 11111
p = 6
eps = 0.01
K = 2
COST = 0.2

W.breaks = seq(-5, 7, by = 0.5)
W.mids = (W.breaks[-1] + W.breaks[-length(W.breaks)])/2

for (setup in setups) {
  for (n.samples in nvals) {
    
    wfn = wfn_list[[setup]]
    
    results = foreach(reps = 1:NREP, .combine = rbind) %dopar% {
      X.all = matrix(rnorm(n.samples * p), n.samples, p)
      W.all = apply(X.all, 1, wfn)
      target.all = 5 / (1 + 3/exp(X.all[,1] + X.all[,2])) - 1/2
      prob.all = pnorm(W.all - target.all)
      deriv.all = dnorm(W.all - target.all)
      Y.all = rbinom(n.samples, 1, prob.all)
      foldid = rep(1:K, n.samples/K)
      
      X.eval = matrix(rnorm(n.eval * p), n.eval, p)
      W.eval = apply(X.eval, 1, wfn)
      target.eval = 5 / (1 + 3/exp(X.eval[,1] + X.eval[,2])) - 1/2
      prob.eval = pnorm(W.eval - target.eval)
      deriv.eval = dnorm(W.eval - target.eval)
      
      # hist(prob.all)
      
      Gamma.hat.all = rep(NA, n.samples)
      deriv.hat.all = rep(NA, n.samples)
      mu.hat.all = rep(NA, n.samples)
      g.hat.all = rep(NA, n.samples)
      
      for (k in 1:K) {
        n.train = sum(foldid != k)
        n.test = sum(foldid == k)
        X = X.all[foldid != k,]
        Y = Y.all[foldid != k]
        W = W.all[foldid != k]
        target = target.all[foldid != k]
        prob = prob.all[foldid != k]
        deriv = deriv.all[foldid != k]
        
        X.test = X.all[foldid == k,]
        Y.test = Y.all[foldid == k]
        W.test = W.all[foldid == k]
        target.test = target.all[foldid == k]
        prob.test = prob.all[foldid == k]
        deriv.test = deriv.all[foldid == k]
        
        B.all = generate.basis(rbind(cbind(X, W),
                                     cbind(X.test, W.test),
                                     cbind(X.test, W.test + eps)), 3)
        B = B.all[1:n.train,]
        B.test = B.all[n.train + 1:n.test,]
        B.test.eps = B.all[n.train + n.test + 1:n.test,]
        
        Y.fit = cv.glmnet(B, Y, family = "binomial")
        Y.hat = predict(Y.fit, B.test, type = "response")
        Y.hat.eps = predict(Y.fit, B.test.eps, type = "response")
        deriv.hat = (Y.hat.eps - Y.hat) / eps
        
        #plot(deriv.hat, deriv.test)
        #abline(0, 1, col = 2, lwd = 4)
        
        W.cut = cut(W.all, W.breaks)
        W.bucket.all = model.matrix( ~ W.cut + 0)
        W.bucket = W.bucket.all[1:n.train,]
        
        XWgrid = expand.grid(1:n.train, 1:length(W.mids))
        
        BX.0 = cbind(10, generate.basis(rbind(X, X.test), 3))
        
        Wvec = c(W.mids, W.test, W.test + eps)
        if (setup == 1) {
          BW.0 = cbind(10, splines::ns(Wvec, df = 5))
        } else if (setup == 2) {
          BW.0 = cbind(10, Wvec, Wvec^2)
        }
        
        BX.all = BX.0[c(XWgrid[,1], n.train + 1:n.test, n.train + 1:n.test),]
        BW.all = BW.0[c(XWgrid[,2], length(W.mids) + 1:(2 * n.test)),]
        
        outergrid = expand.grid(1:ncol(BX.all), 1:ncol(BW.all))
        BF.all = sapply(1:nrow(outergrid),
                        function(ii) BX.all[,outergrid[ii,1]] * BW.all[,outergrid[ii,2]])
        
        BF = BF.all[1:nrow(XWgrid),]
        BF.test = BF.all[nrow(XWgrid) + 1:n.test,]
        BF.test.eps = BF.all[nrow(XWgrid) + n.test + 1:n.test,]
        
        #proc.time()
        W.fit = cv.glmnet(BF, c(W.bucket), family = "binomial",
                          alpha = 0,
                          standardize = FALSE)
        #proc.time()
        
        fW.hat = predict(W.fit, BF.test, type = "response")
        fW.hat.eps = predict(W.fit, BF.test.eps, type = "response")
        g.hat = - (log(fW.hat.eps) - log(fW.hat)) / eps
        
        Gamma = deriv.hat + g.hat * (Y.test - Y.hat)
        
        Gamma.hat.all[foldid == k] = Gamma
        deriv.hat.all[foldid == k] = deriv.hat
        mu.hat.all[foldid == k] = Y.hat
        g.hat.all[foldid == k] = g.hat
        deriv.all[foldid == k] = deriv.test
      }
      
      Delta.hat = Gamma.hat.all - COST
      if (all(Delta.hat <= 0)) {
        pi.hat = rep(0, length(deriv.eval))
      } else if (all(Delta.hat >= 0)) {
        pi.hat = rep(1, length(deriv.eval))
      } else {
        tree = policy_tree(X.all, cbind(-Delta.hat, Delta.hat))
        pi.hat = predict(tree, X.eval) - 1
      }
      util = mean(pi.hat * (deriv.eval - COST))
      
      return(c(theta.oracle = mean(deriv.all),
               theta.plugin = mean(deriv.hat),
               theta.weight = mean(Y.all * g.hat.all),
               theta.DR = mean(Gamma.hat.all),
               se.DR = sqrt(var(Gamma.hat.all) / n.samples),
               policy.value = util))
    }
    
    write.csv(results, paste0("table_3_raw_output/out_n", n.samples, "_setup", setup, ".csv"))
  }
}

