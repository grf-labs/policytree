[![CRANstatus](https://www.r-pkg.org/badges/version/policytree)](https://cran.r-project.org/package=policytree)
[![Build Status](https://travis-ci.com/grf-labs/policytree.svg?branch=master)](https://travis-ci.com/grf-labs/policytree)

# policytree

Learn optimal policies via doubly robust empirical welfare maximization over trees

* Estimate multi-action treatment effects with one vs. all [grf](https://github.com/grf-labs/grf)
* Calculate statistics such as double robust scores (support for a subset of _grf_ forest types)
* Fit optimal policies with exact tree search

### Documentation
https://grf-labs.github.io/policytree/
* [Get started](https://grf-labs.github.io/policytree/articles/policytree.html)
* [Reference](https://grf-labs.github.io/policytree/reference/index.html)

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
d <- 3
X <- matrix(runif(n * p), n, p)
Y <- runif(n)
W <- sample(c("A", "B", "C"), n, replace = TRUE)
multi.forest <- multi_causal_forest(X = X, Y = Y, W = W)

# tau.hats
head(predict(multi.forest)$predictions)
#>               A          B            C
#> 1 -0.0154032776 0.06835031 -0.045857681
#> 2 -0.0266322101 0.04587167 -0.000490636
#> 3 -0.0375290300 0.07230710 -0.048778941
#> 4  0.0002828242 0.06351683 -0.032401124
#> 5 -0.0090172925 0.06350601 -0.092397923
#> 6  0.0144621507 0.07101125 -0.075447751
```

### Policy learning
```r
Gamma.matrix <- double_robust_scores(multi.forest)
head(Gamma.matrix)
#>              A         B         C
#> [1,] 0.4953139 0.8489728 0.4768008
#> [2,] 0.4826144 0.9327100 0.4983701
#> [3,] 0.5275024 0.6020840 1.0111579
#> [4,] 1.4449256 0.5391810 0.4715283
#> [5,] 0.5369825 1.8125964 0.4870979
#> [6,] 0.5360087 0.5766404 0.2826482

train <- sample(1:n, 200)
opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
opt.tree
#> policy_tree object
#> Tree depth:  2
#> Actions:  1: A 2: B 3: C
#> Variable splits:
#> (1) split_variable: X6  split_value: 0.421259
#>   (2) split_variable: X3  split_value: 0.683169
#>     (4) * action: 1
#>     (5) * action: 3
#>   (3) split_variable: X6  split_value: 0.686948
#>     (6) * action: 2
#>     (7) * action: 3

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

Mimic build/test/checks through RStudio's build menu. To build the online documentation locally run _pkgdown::build_site()_.

### References

Athey, Susan, and Stefan Wager. "Efficient policy learning." [arXiv](https://arxiv.org/abs/1702.02896) preprint arXiv:1702.02896 (2017).

Kitagawa, Toru, and Aleksey Tetenov. "Who should be treated? empirical welfare maximization methods for treatment choice." Econometrica 86.2 (2018): 591-616.

Zhou, Zhengyuan, Susan Athey, and Stefan Wager. "Offline multi-action policy learning: Generalization and optimization." [arXiv](https://arxiv.org/abs/1810.04778) preprint arXiv:1810.04778 (2018).
