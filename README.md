[![CRANstatus](https://www.r-pkg.org/badges/version/policytree)](https://cran.r-project.org/package=policytree)
[![Build Status](https://travis-ci.com/grf-labs/policytree.svg?branch=master)](https://travis-ci.com/grf-labs/policytree)

# policytree

A package for learning optimal policies via doubly robust empirical welfare maximization over trees. This package implements the multi-action doubly robust approach of Zhou et al. (2018) in the case where we want to learn policies that belong to the class of depth _k_ decision trees. Many practical policy applications require interpretable predictions. For example, a drug prescription guide that follows a simple 2-question Yes/No checklist can be encoded as a depth 2 decision tree (does the patient have a heart condition - etc.). `policytree` currently has support for estimating multi-action treatment effects with one vs. all [grf](https://github.com/grf-labs/grf), calculating statistics such as double robust scores (support for a subset of _grf_ forest types) and fitting optimal policies with exact tree search.

Some helpful links for getting started:

* The [R package documentation](https://grf-labs.github.io/policytree/) contains usage examples and method reference.
* For community questions and answers around usage, see the GitHub [issues page](https://github.com/grf-labs/policytree/issues).


### Installation

The latest release of the package can be installed through CRAN:

```R
install.packages("policytree")
```

To install the latest development version from source:

```
git clone https://github.com/grf-labs/policytree.git
Rscript -e 'install.packages("policytree", repos = NULL, type = "source")'

# or
devtools::install_github("grf-labs/policytree")
```

Installing from source requires a C++ 11 compiler (on Windows Rtools is required as well) together with the R packages
`Rcpp` and `BH`.

### Multi-action treatment effect estimation
```r
library(policytree)
n <- 250
p <- 10
X <- matrix(rnorm(n * p), n, p)
W <- sample(c("A", "B", "C"), n, replace = TRUE)
Y <- X[, 1] + X[, 2] * (W == "B") + X[, 3] * (W == "C") + runif(n)
multi.forest <- multi_causal_forest(X = X, Y = Y, W = W)

# tau.hats
head(predict(multi.forest)$predictions)
#              A            B          C
# 1  0.110469853 -0.005280646  0.1664277
# 2  0.258415454 -0.747010156  0.1734191
# 3  0.449918392 -0.284277647 -0.6307613
# 4 -0.005547692  0.871686529 -0.6564725
# 5  0.343872139 -0.090049312 -0.3968521
# 6  0.376482355  0.233689768 -0.8111073
```

### Policy learning
```r
Gamma.matrix <- double_robust_scores(multi.forest)
head(Gamma.matrix)
#              A          B           C
# 1 -0.002612209 -0.1438422 -0.04243015
# 2  0.417066177  0.4212708  1.04000173
# 3  2.020414370  0.3963890  1.33038496
# 4  1.193587749  1.7862142 -0.05668051
# 5  0.808323778  0.5017521  1.52094053
# 6 -0.045844471 -0.1460745 -1.56055025

train <- sample(1:n, 200)
opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
opt.tree
policy_tree object
# Tree depth:  2
# Actions:  1: A 2: B 3: C
# Variable splits:
# (1) split_variable: X3  split_value: 0.368037
#   (2) split_variable: X2  split_value: -0.098143
#     (4) * action: 1
#     (5) * action: 2
#   (3) split_variable: X2  split_value: 1.25697
#     (6) * action: 3
#     (7) * action: 2

## Predict treatment on held out data
head(predict(opt.tree, X[-train, ]))
#> [1] 2 3 1 2 3 3
```

### Details
Tree search
* `policy_tree()`: fits a depth L tree by exhaustive search (_Nxp_ features on _Nxd_ actions). The optimal tree maximizes the sum of rewards.

Treatment effects
* `multi_causal_forest()`: fits one causal forest for each treatment. Operates similarly to _grf_: `predict()` returns treatment estimates.
* `double_robust_scores()`: generic function dispatching on appropriate forest type. By the OOB nature of forest estimates these have cross-fitting "baked in".

### Contributing

Contributions are welcome, please consult the [development guide](https://github.com/grf-labs/policytree/blob/master/DEVELOPING.md) for details.

### References

Athey, Susan, and Stefan Wager. "Efficient policy learning." [arXiv](https://arxiv.org/abs/1702.02896) preprint arXiv:1702.02896 (2017).

Kitagawa, Toru, and Aleksey Tetenov. "Who should be treated? empirical welfare maximization methods for treatment choice." Econometrica 86.2 (2018): 591-616.

Sverdrup, Erik, Ayush Kanodia, Zhengyuan Zhou, Susan Athey, and Stefan Wager. "policytree: Policy learning via doubly robust empirical welfare maximization over trees". Journal of Open Source Software, 5(50), 2232. [[paper](https://joss.theoj.org/papers/10.21105/joss.02232)]

Zhou, Zhengyuan, Susan Athey, and Stefan Wager. "Offline multi-action policy learning: Generalization and optimization." [arXiv](https://arxiv.org/abs/1810.04778) preprint arXiv:1810.04778 (2018).
