#' One vs. all causal forest for multiple treatment effect estimation
#'
#' For K treatments this "naive" multivariate-grf proceeeds by fitting K separate causal forests
#' where in forest k the treatment assignment vector is one-hot encoded for treament k
#' (i.e. treatment vector  w_k entry i is one where individual i receives treatment k, else zero). The steps are:
#' 1) Estimate propensities \eqn{e_k(x)} for each action 1..K: This is done with k separate regression forests
#' with propensities normalized to sum to 1 at the final step.
#' 2) Estimate the expected response m(x) = E(Y | X) marginalizing over treatment. This is done with one
#' regression forest.
#' 3) Estimate the treatment effect \eqn{\tau_k(x) = \frac{\mu_k(x) - m(x)}{1 - e_k(x)}} with a causal forest (where \eqn{\mu_k(x) = E[Y | X, W=W_k]})
#'
#' @param X The covariates used in the causal regression.
#' @param Y The outcome (must be a numeric vector with no NAs).
#' @param W The treatment assignment (must be a categorical vector with no NAs).
#' @param Y.hat Estimates of the expected responses E\[Y | Xi\], marginalizing
#'              over treatment. If Y.hat = NULL, these are estimated using
#'              a separate regression forest. See section 6.1.1 of the GRF paper for
#'              further discussion of this quantity. Default is NULL.
#' @param W.hat Matrix with estimates of the treatment propensities E\[Wk | Xi\]. If W.hat = NULL,
#'              these are estimated using a k separate regression forests. Default is NULL.
#' @param num.trees Number of trees grown in the forest. Note: Getting accurate
#'                  confidence intervals generally requires more trees than
#'                  getting accurate predictions. Default is 2000.
#' @param sample.weights (experimental) Weights given to each sample in estimation.
#'                       If NULL, each observation receives the same weight.
#'                       Note: To avoid introducing confounding, weights should be
#'                       independent of the potential outcomes given X. Default is NULL.
#' @param clusters Vector of integers or factors specifying which cluster each observation corresponds to.
#'  Default is NULL (ignored).
#' @param equalize.cluster.weights If FALSE, each unit is given the same weight (so that bigger
#'  clusters get more weight). If TRUE, each cluster is given equal weight in the forest. In this case,
#'  during training, each tree uses the same number of observations from each drawn cluster: If the
#'  smallest cluster has K units, then when we sample a cluster during training, we only give a random
#'  K elements of the cluster to the tree-growing procedure. When estimating average treatment effects,
#'  each observation is given weight 1/cluster size, so that the total weight of each cluster is the
#'  same. Note that, if this argument is FALSE, sample weights may also be directly adjusted via the
#'  sample.weights argument. If this argument is TRUE, sample.weights must be set to NULL. Default is
#'  FALSE.
#' @param sample.fraction Fraction of the data used to build each tree.
#'                        Note: If honesty = TRUE, these subsamples will
#'                        further be cut by a factor of honesty.fraction. Default is 0.5.
#' @param mtry Number of variables tried for each split. Default is
#'             \eqn{\sqrt p + 20} where p is the number of variables.
#' @param min.node.size A target for the minimum number of observations in each tree leaf. Note that nodes
#'                      with size smaller than min.node.size can occur, as in the original randomForest package.
#'                      Default is 5.
#' @param honesty Whether to use honest splitting (i.e., sub-sample splitting). Default is TRUE.
#'  For a detailed description of honesty, honesty.fraction, honesty.prune.leaves, and recommendations for
#'  parameter tuning, see the grf
#'  \href{https://grf-labs.github.io/grf/REFERENCE.html#honesty-honesty-fraction-prune-empty-leaves}{algorithm reference}.
#' @param honesty.fraction The fraction of data that will be used for determining splits if honesty = TRUE. Corresponds
#'                         to set J1 in the notation of the paper. Default is 0.5 (i.e. half of the data is used for
#'                         determining splits).
#' @param honesty.prune.leaves If true, prunes the estimation sample tree such that no leaves
#'  are empty. If false, keep the same tree as determined in the splits sample (if an empty leave is encountered, that
#'  tree is skipped and does not contribute to the estimate). Setting this to false may improve performance on
#'  small/marginally powered data, but requires more trees (note: tuning does not adjust the number of trees).
#'  Only applies if honesty is enabled. Default is TRUE.
#' @param alpha A tuning parameter that controls the maximum imbalance of a split. Default is 0.05.
#' @param imbalance.penalty A tuning parameter that controls how harshly imbalanced splits are penalized. Default is 0.
#' @param stabilize.splits Whether or not the treatment should be taken into account when
#'                         determining the imbalance of a split. Default is TRUE.
#' @param ci.group.size The forest will grow ci.group.size trees on each subsample.
#'                      In order to provide confidence intervals, ci.group.size must
#'                      be at least 2. Default is 2.
#' @param tune.parameters A vector of parameter names to tune.
#'  If "all": all tunable parameters are tuned by cross-validation. The following parameters are
#'  tunable: ("sample.fraction", "mtry", "min.node.size", "honesty.fraction",
#'   "honesty.prune.leaves", "alpha", "imbalance.penalty"). If honesty is false these parameters are not tuned.
#'  Default is "none" (no parameters are tuned).
#' @param tune.num.trees The number of trees in each 'mini forest' used to fit the tuning model. Default is 200.
#' @param tune.num.reps The number of forests used to fit the tuning model. Default is 50.
#' @param tune.num.draws The number of random parameter values considered when using the model
#'                          to select the optimal parameters. Default is 1000.
#' @param compute.oob.predictions Whether OOB predictions on training set should be precomputed. Default is TRUE.
#' @param orthog.boosting (experimental) If TRUE, then when Y.hat = NULL or W.hat is NULL,
#'                 the missing quantities are estimated using boosted regression forests.
#'                 The number of boosting steps is selected automatically. Default is FALSE.
#' @param num.threads Number of threads used in training. By default, the number of threads is set
#'                    to the maximum hardware concurrency.
#' @param seed The seed of the C++ random number generator.
#'
#' @return A trained multi causal forest object (collection of causal forests). If tune.parameters is enabled,
#'  then tuning information will be included through the `tuning.output` attribute of each forest.
#'
#' @examples
#' \donttest{
#' # Train a multi causal forest.
#' n <- 250
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' W <- sample(c("A", "B", "C"), n, replace = TRUE)
#' Y <- X[, 1] + X[, 2] * (W == "B") + X[, 3] * (W == "C") + runif(n)
#' multi.forest <- multi_causal_forest(X = X, Y = Y, W = W)
#'
#' # Predict using the forest.
#' multi.forest.pred <- predict(multi.forest)
#' head(multi.forest.pred$predictions)
#' }
#' @export
multi_causal_forest <- function(X, Y, W,
                                Y.hat = NULL,
                                W.hat = NULL,
                                num.trees = 2000,
                                sample.weights = NULL,
                                clusters = NULL,
                                equalize.cluster.weights = FALSE,
                                sample.fraction = 0.5,
                                mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
                                min.node.size = 5,
                                honesty = TRUE,
                                honesty.fraction = 0.5,
                                honesty.prune.leaves = TRUE,
                                alpha = 0.05,
                                imbalance.penalty = 0,
                                stabilize.splits = TRUE,
                                ci.group.size = 2,
                                tune.parameters = "none",
                                tune.num.trees = 200,
                                tune.num.reps = 50,
                                tune.num.draws = 1000,
                                compute.oob.predictions = TRUE,
                                orthog.boosting = FALSE,
                                num.threads = NULL,
                                seed = runif(1, 0, .Machine$integer.max)) {

  # All parameters except W.hat (and W) is the same for each grf::causal_forest
  treatments <- sort(unique(W))
  treatment.names <- as.character(treatments)
  n.treatments <- length(treatments)

  # Unconditional mean
  args.orthog <- list(X = X,
                     num.trees = max(50, num.trees / 4),
                     sample.weights = sample.weights,
                     clusters = clusters,
                     equalize.cluster.weights = equalize.cluster.weights,
                     sample.fraction = sample.fraction,
                     mtry = mtry,
                     min.node.size = 5,
                     honesty = TRUE,
                     honesty.fraction = 0.5,
                     honesty.prune.leaves = honesty.prune.leaves,
                     alpha = alpha,
                     imbalance.penalty = imbalance.penalty,
                     ci.group.size = 1,
                     tune.parameters = tune.parameters,
                     num.threads = num.threads,
                     seed = seed)

  if (is.null(Y.hat) && !orthog.boosting) {
    forest.Y <- do.call(grf::regression_forest, c(Y = list(Y), args.orthog))
    Y.hat <- predict(forest.Y)$predictions
  } else if (is.null(Y.hat) && orthog.boosting) {
    forest.Y <- do.call(grf::boosted_regression_forest, c(Y = list(Y), args.orthog))
    Y.hat <- predict(forest.Y)$predictions
  } else if (length(Y.hat) == 1) {
    Y.hat <- rep(Y.hat, nrow(X))
  } else if (length(Y.hat) != nrow(X)) {
    stop("Y.hat has incorrect length.")
  }

  # Propensities
  W.onevsall <- sapply(treatments, function(treatment) as.integer(W == treatment))
  if (is.null(W.hat)) {
    if (!orthog.boosting) {
      W.hat <- apply(W.onevsall, 2, function(Wi) {
        forest.W <- do.call(grf::regression_forest, c(Y = list(Wi), args.orthog))
        predict(forest.W)$predictions
      })
    } else if (orthog.boosting) {
      W.hat <- apply(W.onevsall, 2, function(Wi) {
        forest.W <- do.call(grf::boosted_regression_forest, c(Y = list(Wi), args.orthog))
        predict(forest.W)$predictions
      })
    }
    # Normalize to sum to 1
    W.hat <- sweep(W.hat, 1, rowSums(W.hat), `/`)
  } else if (length(W.hat) == n.treatments) {
    W.hat <- matrix(W.hat, nrow = length(Y), ncol = n.treatments, byrow = TRUE)
  } else if ((NROW(W.hat) != nrow(X)) || (ncol(W.hat) != n.treatments)) {
    stop("W.hat has incorrect dimensions: should be a matrix of propensities for each (observation, action).")
  }
  colnames(W.hat) <- treatment.names

  # k causal forests
  forests <- lapply(1:n.treatments, function(i) {
    grf::causal_forest(X, Y, W.onevsall[, i], Y.hat, W.hat[, i],
                       num.trees = num.trees,
                       sample.weights = sample.weights,
                       clusters = clusters,
                       equalize.cluster.weights = equalize.cluster.weights,
                       sample.fraction = sample.fraction,
                       mtry = mtry,
                       min.node.size = min.node.size,
                       honesty = honesty,
                       honesty.fraction = honesty.fraction,
                       honesty.prune.leaves = honesty.prune.leaves,
                       alpha = alpha,
                       imbalance.penalty = imbalance.penalty,
                       stabilize.splits = stabilize.splits,
                       ci.group.size = ci.group.size,
                       tune.parameters = tune.parameters,
                       tune.num.trees = tune.num.trees,
                       tune.num.reps = tune.num.reps,
                       compute.oob.predictions = compute.oob.predictions,
                       orthog.boosting = FALSE,
                       num.threads = num.threads,
                       seed = seed)
  })
  names(forests) <- treatment.names

  out <- list()
  class(out) <- "multi_causal_forest"
  out$forests <- forests
  out$Y.orig <- Y
  out$W.orig <- W
  out$W.onevsall <- W.onevsall
  out$W.hat <- W.hat
  out$Y.hat <- Y.hat

  out
}

#' Predict with multi_causal_forest
#'
#' Computes estimates of \eqn{\tau_a(x)}
#'
#' @param object The trained forest.
#' @param newdata Points at which predictions should be made. If NULL, makes out-of-bag
#'                predictions on the training set instead (i.e., provides predictions at
#'                Xi using only trees that did not use the i-th training example). Note
#'                that this matrix should have the number of columns as the training
#'                matrix, and that the columns must appear in the same order.
#' @param ... Additional arguments passed to
#'  \href{https://grf-labs.github.io/grf/reference/predict.causal_forest.html}{grf::predict.causal_forest}.
#'
#' @return List containing matrix of predictions and other estimates (debiased error, etc.) for each treatment.
#'
#' @examples
#' \donttest{
#' # Train a multi causal forest.
#' n <- 250
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' W <- sample(c("A", "B", "C"), n, replace = TRUE)
#' Y <- X[, 1] + X[, 2] * (W == "B") + X[, 3] * (W == "C") + runif(n)
#' multi.forest <- multi_causal_forest(X = X, Y = Y, W = W)
#'
#' # Predict using the forest.
#' multi.forest.pred <- predict(multi.forest)
#' head(multi.forest.pred$predictions)
#' }
#' @method predict multi_causal_forest
#' @export
predict.multi_causal_forest <- function(object, newdata = NULL, ...) {
  forests <- object$forests

  # A `grf` prediction object is a data frame with columns `predictions`
  # and other estimates. This transforms the list of data frames for each
  # forest (action k) to a list of each estimate (with each data frame containing
  # n.actions columns).
  predictions <- sapply(forests, function(forest) {
    predict(forest, newdata = newdata, ...)
  })
  outputs <- if(is.null(rownames(predictions))) {
    "predictions"
  } else {
    rownames(predictions)
  }
  predictions <- matrix(predictions, ncol = length(object$forests))

  out <- lapply(1:nrow(predictions), function(row) {
    values <- as.data.frame(predictions[row, ])
    colnames(values) <- names(forests)
    values
  })
  names(out) <- outputs

  out
}
