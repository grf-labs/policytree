#' Fit a policy with exact tree search
#'
#' Finds the optimal (maximizing the sum of rewards) depth k tree by exhaustive search. If the optimal
#' action is the same in both the left and right leaf of a node, the node is pruned.
#'
#' Exact tree search is intended as a way to find shallow (i.e. depth 2 or 3) globally optimal
#' tree-based polices on datasets of "moderate" size.
#' The amortized runtime of exact tree search is \eqn{O(p^k n^k (log n + d) + pnlog n)} where p is
#' the number of features, n the number of distinct observations, d the number of treatments, and k >= 1
#' the tree depth. Due to the exponents in this expression, exact tree search will not scale to datasets
#' of arbitrary size.
#'
#' As an example, the runtime of a depth two tree scales quadratically with the number of observations, implying
#' that doubling the number of samples will quadruple the runtime.
#' n refers to the number of distinct observations, substantial speedups can be gained
#' when the features are discrete (with all binary features, the runtime will be ~ linear in n),
#' and it is therefore beneficial to round down/re-encode very dense data to a lower cardinality
#' (the optional parameter `split.step` emulates this, though rounding/re-encoding allow for finer-grained control).
#'
#' @param X The covariates used. Dimension \eqn{N*p} where \eqn{p} is the number of features.
#' @param Gamma The rewards for each action. Dimension \eqn{N*d} where \eqn{d} is the number of actions.
#' @param depth The depth of the fitted tree. Default is 2.
#' @param split.step An optional approximation parameter, the number of possible splits
#'  to consider when performing tree search. split.step = 1 (default) considers every possible split, split.step = 10
#'  considers splitting at every 10'th sample and may yield a substantial speedup for dense features.
#'  Manually rounding or re-encoding continuous covariates with very high cardinality in a
#'  problem specific manner allows for finer-grained control of the accuracy/runtime tradeoff and may in some cases
#'  be the preferred approach.
#' @param min.node.size An integer indicating the smallest terminal node size permitted. Default is 1.
#' @param verbose Give verbose output. Default is TRUE.
#'
#' @return A policy_tree object.
#'
#' @references Athey, Susan, and Stefan Wager. "Policy Learning With Observational Data."
#'  Econometrica 89.1 (2021): 133-161.
#' @references Sverdrup, Erik, Ayush Kanodia, Zhengyuan Zhou, Susan Athey, and Stefan Wager.
#'  "policytree: Policy learning via doubly robust empirical welfare maximization over trees."
#'   Journal of Open Source Software 5, no. 50 (2020): 2232.
#' @references Zhou, Zhengyuan, Susan Athey, and Stefan Wager. "Offline multi-action policy learning:
#'  Generalization and optimization." Operations Research, forthcoming.
#'
#' @examples
#' \donttest{
#' # Fit a depth two tree on doubly robust treatment effect estimates from a causal forest.
#' n <- 10000
#' p <- 10
#' # Discretizing continuous covariates decreases runtime.
#' X <- round(matrix(rnorm(n * p), n, p), 2)
#' colnames(X) <- make.names(1:p)
#' W <- rbinom(n, 1, 1 / (1 + exp(X[, 3])))
#' tau <- 1 / (1 + exp((X[, 1] + X[, 2]) / 2)) - 0.5
#' Y <- X[, 3] + W * tau + rnorm(n)
#' c.forest <- grf::causal_forest(X, Y, W)
#' dr.scores <- double_robust_scores(c.forest)
#'
#' tree <- policy_tree(X, dr.scores, 2)
#' tree
#'
#' # Predict treatment assignment.
#' predicted <- predict(tree, X)
#'
#' plot(X[, 1], X[, 2], col = predicted)
#' legend("topright", c("control", "treat"), col = c(1, 2), pch = 19)
#' abline(0, -1, lty = 2)
#'
#' # Predict the leaf assigned to each sample.
#' node.id <- predict(tree, X, type = "node.id")
#' # Can be reshaped to a list of samples per leaf node with `split`.
#' samples.per.leaf <- split(1:n, node.id)
#'
#' # The value of all arms (along with SEs) by each leaf node.
#' values <- aggregate(dr.scores, by = list(leaf.node = node.id),
#'                     FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x))))
#' print(values, digits = 2)
#'
#' # Take cost of treatment into account by offsetting the objective
#' # with an estimate of the average treatment effect.
#' # See section 5.1 in Athey and Wager (2021) for more details, including
#' # suggestions on using cross-validation to assess the accuracy of the learned policy.
#' ate <- grf::average_treatment_effect(c.forest)
#' cost.offset <- ate[["estimate"]]
#' dr.scores[, "treated"] <- dr.scores[, "treated"] - cost.offset
#' tree.cost <- policy_tree(X, dr.scores, 2)
#'
#' # If there are too many covariates to make tree search computationally feasible,
#' # one can consider for example only the top 5 features according to GRF's variable importance.
#' var.imp <- grf::variable_importance(c.forest)
#' top.5 <- order(var.imp, decreasing = TRUE)[1:5]
#' tree.top5 <- policy_tree(X[, top.5], dr.scores, 2, split.step = 50)
#' }
#' @seealso \code{\link{hybrid_policy_tree}} for building deeper trees.
#' @export
policy_tree <- function(X, Gamma, depth = 2, split.step = 1, min.node.size = 1, verbose = TRUE) {
  n.features <- ncol(X)
  n.actions <- ncol(Gamma)
  n.obs <- nrow(X)
  valid.classes <- c("matrix", "data.frame")

  if (!inherits(X, valid.classes) || !inherits(Gamma, valid.classes)) {
    stop(paste("Currently the only supported data input types are:",
               "`matrix`, `data.frame`"))
  }
  if (!is.numeric(as.matrix(X)) || any(dim(X) == 0)) {
    stop("The feature matrix X must be numeric")
  }
  if (!is.numeric(as.matrix(Gamma)) || any(dim(Gamma) == 0)) {
    stop("The reward matrix Gamma must be numeric")
  }
  if (anyNA(X)) {
    stop("Covariate matrix X contains missing values.")
  }
  if (anyNA(Gamma)) {
    stop("Gamma matrix contains missing values.")
  }
  if (depth < 0 ) {
    stop("`depth` cannot be negative.")
  }
  if (n.obs != nrow(Gamma)) {
    stop("X and Gamma does not have the same number of rows")
  }
  if (as.integer(split.step) != split.step || split.step < 1) {
    stop("`split.step` should be an integer greater than or equal to 1.")
  }
  if (as.integer(min.node.size) != min.node.size || min.node.size < 1) {
    stop("min.node.size should be an integer greater than or equal to 1.")
  }

  if (verbose) {
    cardinality <- apply(X, 2, function(x) length(unique(x)))
    if (split.step == 1 && any(cardinality > 20000)) {
      warning(paste0(
        "The cardinality of some covariates exceeds 20000 distinct values. ",
        "Consider using the optional parameter `split.step` to speed up computations, or ",
        "discretize/relabel continuous features for finer grained control ",
        "(the runtime of exact tree search scales with the number of distinct features, ",
        "see the documentation for details.)"
      ), immediate. = TRUE)
    }
    if (ncol(X) > 50) {
      warning(paste0(
        "The number of covariates exceeds 50. Consider reducing the dimensionality before ",
        "running policy_tree, by for example using only the Xj's with the ",
        "highest variable importance (`grf::variable_importance` - the runtime of exact tree ",
        "search scales with ncol(X)^depth, see the documentation for details)."
      ), immediate. = TRUE)
    }
    if (depth > 2 && nrow(X) > 5000) {
      warning(paste0(
        "A depth 3 or deeper policy_tree is only feasible for 'small' n and p. ",
        "To fit deeper trees, consider using the hybrid greedy approach available in the function ",
        "`hybrid_policy_tree`. Note that this still requires an (n, p) configuration ",
        "which is feasible for a depth k=2 policy_tree, see the documentation for details."
      ), immediate. = TRUE)
    }
  }

  action.names <- colnames(Gamma)
  if (is.null(action.names)) {
    action.names <- as.character(1:ncol(Gamma))
  }
  action.names <- utils::type.convert(action.names, as.is = TRUE) # TRUE to not convert character to factor
  columns <- colnames(X)
  if (is.null(columns)) {
    columns <- make.names(1:ncol(X))
  }

  result <- tree_search_rcpp(as.matrix(X), as.matrix(Gamma), depth, split.step, min.node.size)
  tree <- list(nodes = result[[1]])

  tree[["_tree_array"]] <- result[[2]]
  tree[["depth"]] <- depth
  tree[["n.actions"]] <- n.actions
  tree[["n.features"]] <- n.features
  tree[["action.names"]] <- action.names
  tree[["columns"]] <- columns
  class(tree) <- "policy_tree"

  tree
}

#' Predict method for policy_tree
#'
#' Predict values based on fitted policy_tree object.
#' @param object policy_tree object
#' @param newdata Points at which predictions should be made. Note that this matrix should have the
#'  same number of columns as the training matrix, and that the columns must appear in the same order.
#' @param type The type of prediction required, "action.id" is the action id and
#'  "node.id" is the integer id of the leaf node the sample falls into. Default is "action.id".
#' @param ... Additional arguments (currently ignored).
#'
#' @return A vector of predictions. For type = "action.id" each element is an integer from 1 to d where d is
#'  the number of columns in the reward matrix. For type = "node.id" each element is an integer corresponding
#'  to the node the sample falls into (level-ordered).
#' @export
#'
#' @method predict policy_tree
#' @examples
#' \donttest{
#' # Fit a depth two tree on doubly robust treatment effect estimates from a causal forest.
#' n <- 10000
#' p <- 10
#' # Discretizing continuous covariates decreases runtime.
#' X <- round(matrix(rnorm(n * p), n, p), 2)
#' colnames(X) <- make.names(1:p)
#' W <- rbinom(n, 1, 1 / (1 + exp(X[, 3])))
#' tau <- 1 / (1 + exp((X[, 1] + X[, 2]) / 2)) - 0.5
#' Y <- X[, 3] + W * tau + rnorm(n)
#' c.forest <- grf::causal_forest(X, Y, W)
#' dr.scores <- double_robust_scores(c.forest)
#'
#' tree <- policy_tree(X, dr.scores, 2)
#' tree
#'
#' # Predict treatment assignment.
#' predicted <- predict(tree, X)
#'
#' plot(X[, 1], X[, 2], col = predicted)
#' legend("topright", c("control", "treat"), col = c(1, 2), pch = 19)
#' abline(0, -1, lty = 2)
#'
#' # Predict the leaf assigned to each sample.
#' node.id <- predict(tree, X, type = "node.id")
#' # Can be reshaped to a list of samples per leaf node with `split`.
#' samples.per.leaf <- split(1:n, node.id)
#'
#' # The value of all arms (along with SEs) by each leaf node.
#' values <- aggregate(dr.scores, by = list(leaf.node = node.id),
#'                     FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x))))
#' print(values, digits = 2)
#'
#' # Take cost of treatment into account by offsetting the objective
#' # with an estimate of the average treatment effect.
#' # See section 5.1 in Athey and Wager (2021) for more details, including
#' # suggestions on using cross-validation to assess the accuracy of the learned policy.
#' ate <- grf::average_treatment_effect(c.forest)
#' cost.offset <- ate[["estimate"]]
#' dr.scores[, "treated"] <- dr.scores[, "treated"] - cost.offset
#' tree.cost <- policy_tree(X, dr.scores, 2)
#'
#' # If there are too many covariates to make tree search computationally feasible,
#' # one can consider for example only the top 5 features according to GRF's variable importance.
#' var.imp <- grf::variable_importance(c.forest)
#' top.5 <- order(var.imp, decreasing = TRUE)[1:5]
#' tree.top5 <- policy_tree(X[, top.5], dr.scores, 2, split.step = 50)
#' }
predict.policy_tree <- function(object, newdata, type = c("action.id", "node.id"), ...) {
  type <- match.arg(type)
  valid.classes <- c("matrix", "data.frame")
  if (!inherits(newdata, valid.classes)) {
    stop(paste("Currently the only supported data input types are:",
               "`matrix`, `data.frame`"))
  }
  if (!is.numeric(as.matrix(newdata))) {
    stop("The feature matrix X must be numeric")
  }
  if (anyNA(newdata)) {
    stop("Covariate matrix X contains missing values.")
  }

  tree <- object
  if (tree$n.features != ncol(newdata)) {
    stop("This tree was trained with ", tree$n.features, " variables. Provided: ", ncol(newdata))
  }

  ret <- tree_search_rcpp_predict(tree[["_tree_array"]], as.matrix(newdata))

  if (type == "action.id") {
    return (ret[, 1])
  } else {
    return (ret[, 2] + 1) # + 1 for R-index.
  }

}
