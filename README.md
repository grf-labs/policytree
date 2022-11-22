# policytree

[![CRANstatus](https://www.r-pkg.org/badges/version/policytree)](https://cran.r-project.org/package=policytree)
[![Build Status](https://dev.azure.com/grf-labs/grf/_apis/build/status/grf-labs.policytree?branchName=master)](https://dev.azure.com/grf-labs/grf/_build/latest?definitionId=1&branchName=master)

A package for learning simple rule-based policies, where the rule takes the form of a shallow decision tree. Applications include settings which require interpretable predictions, such as for example a medical treatment prescription. This package uses doubly robust reward estimates from [grf](https://github.com/grf-labs/grf) to find a shallow, but globally optimal decision tree.

Some helpful links for getting started:

* The [R package documentation](https://grf-labs.github.io/policytree/) contains usage examples and method references.
* For community questions and answers around usage, see the GitHub [issues page](https://github.com/grf-labs/policytree/issues).
* The package [sparse policytree](https://github.com/Yale-Medicaid/sparsepolicytree) uses an alternate solver that may be faster on large datasets with sparse categorical features.

### Installation

The latest release of the package can be installed through CRAN:

```R
install.packages("policytree")
```

To install the latest development version from source:

```R
devtools::install_github("grf-labs/policytree", subdir = "r-package/policytree")
```

Installing from source requires a C++ 11 compiler (on Windows Rtools is required as well) together with the R packages
`Rcpp` and `BH`.

### Multi-action policy learning example
```r
library(policytree)
n <- 250
p <- 10
X <- matrix(rnorm(n * p), n, p)
W <- as.factor(sample(c("A", "B", "C"), n, replace = TRUE))
Y <- X[, 1] + X[, 2] * (W == "B") + X[, 3] * (W == "C") + runif(n)
multi.forest <- grf::multi_arm_causal_forest(X, Y, W)

# Compute doubly robust reward estimates.
Gamma.matrix <- double_robust_scores(multi.forest)
head(Gamma.matrix)
#              A          B           C
# 1 -0.002612209 -0.1438422 -0.04243015
# 2  0.417066177  0.4212708  1.04000173
# 3  2.020414370  0.3963890  1.33038496
# 4  1.193587749  1.7862142 -0.05668051
# 5  0.808323778  0.5017521  1.52094053
# 6 -0.045844471 -0.1460745 -1.56055025

# Fit a depth 2 tree on a random training subset.
train <- sample(1:n, 200)
opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
opt.tree
# policy_tree object
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
* `policy_tree()`: fits a depth _k_ tree by exhaustive search (_Nxp_ features on _Nxd_ actions). The optimal tree maximizes the sum of rewards: let $\Gamma_i \in \mathbb R^d$ be a vector of unit-specific rewards for each action 1 to $d$ and $\pi(X_i) \in \\{1, ..., d\\}$ a mapping from covariates $X_i$ to action. `policy_tree` solves the following: 
  
  $$
  \pi^* = argmax_{\pi \in \Pi} \left[ \sum_{i=1}^{n} \Gamma_i(\pi(X_i)) \right],
  $$
  
  where $\Pi$ is the class of depth-_k_ decision trees. (`hybrid_policy_tree()` employs a mix between a optimal/greedy approach and can be used to fit deeper trees).
* `double_robust_scores()`: computes doubly robust reward estimates for a subset of _grf_ forest types.

### Contributing

Contributions are welcome, please consult the [development guide](https://github.com/grf-labs/policytree/blob/master/DEVELOPING.md) for details.

### Funding

Development of policytree is supported by the National Science Foundation, the Sloan Foundation, the Office of Naval Research (Grant N00014-17-1-2131) and Schmidt Futures.

### References

Susan Athey and Stefan Wager.
<b>Policy Learning With Observational Data.</b> <i>Econometrica 89.1 (2021): 133-161.</i>
[<a href="https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA15732">paper</a>,
<a href="https://arxiv.org/abs/1702.02896">arxiv</a>]

Toru Kitagawa and Aleksey Tetenov.
<b>Who Should be Treated? Empirical Welfare Maximization Methods for Treatment Choice.</b> <i>Econometrica 86.2 (2018): 591-616.</i>
[<a href="https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA13288">paper</a>]

Erik Sverdrup, Ayush Kanodia, Zhengyuan Zhou, Susan Athey, and Stefan Wager.
<b>policytree: Policy learning via doubly robust empirical welfare maximization over trees.</b> <i>Journal of Open Source Software, 5(50), 2020.</i>
[<a href="https://joss.theoj.org/papers/10.21105/joss.02232">paper</a>]

Zhengyuan Zhou, Susan Athey, and Stefan Wager.
<b>Offline Multi-Action Policy Learning: Generalization and Optimization.</b> <i> Operations Research</i>, forthcoming.
[<a href="https://arxiv.org/abs/1810.04778">arxiv</a>]
