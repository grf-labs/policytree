[![CRANstatus](https://www.r-pkg.org/badges/version/policytree)](https://cran.r-project.org/package=policytree)
[![Build Status](https://travis-ci.com/grf-labs/policytree.svg?branch=master)](https://travis-ci.com/grf-labs/policytree)

# policytree

Learn optimal policies via doubly robust empirical welfare maximization over trees

* Estimate multi-action treatment effects with one vs. all [grf](https://github.com/grf-labs/grf)
* Calculate statistics such as double robust scores (support for a subset of _grf_ forest types)
* Fit optimal policies with exact tree search

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

Contributions are welcome. This repository follows the standard open source protocol and setup with git where there is an abundance of existing resources to get up to speed. Condensed greatly, the workflow is to fork this repository, check out a branch, commit your changes (forming an ideally legible commit history), then submitting a pull request explaining your contribution, ideally referring to the issue you created, or the issue you chose to work on.

Building and compiling the package can be done conveniently with RStudio. A C++ compiler, together with the R packages _Rcpp_ and _BH_ is required. For documentation _roxygen2_ and _pkgdown_ is needed as well. To compile and load the package after making changes run _Build -> Install and Restart_. To test the package run _Build -> Test Package_. R Markdown tutorials are stored in `vignettes\` and these can be built by running _pkgdown::build_articles()_.

### References

Athey, Susan, and Stefan Wager. "Efficient policy learning." [arXiv](https://arxiv.org/abs/1702.02896) preprint arXiv:1702.02896 (2017).

Kitagawa, Toru, and Aleksey Tetenov. "Who should be treated? empirical welfare maximization methods for treatment choice." Econometrica 86.2 (2018): 591-616.

Zhou, Zhengyuan, Susan Athey, and Stefan Wager. "Offline multi-action policy learning: Generalization and optimization." [arXiv](https://arxiv.org/abs/1810.04778) preprint arXiv:1810.04778 (2018).
