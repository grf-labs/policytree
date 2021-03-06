#' Writes each node information
#' If it is a leaf node: show it in different color, show number of samples, show leaf id
#' If it is a non-leaf node: show its splitting variable and splitting value
#' @param tree the tree to convert
#' @param index the index of the current node
#' @keywords internal
create_dot_body <- function(tree, index = 1) {
  node <- tree$nodes[[index]]

  # Leaf case: print label only
  if (node$is_leaf) {
    action <- node$action
    line_label <- paste(index - 1,
                        ' [shape=box,style=filled,color=".7 .3 1.0" , label="',
                        tree$leaf.labels[action],
                        '"];')
    return(line_label)
  }

  # Non-leaf case: print label, child edges
  if (!is.null(node$left_child)) {
    edge <- paste(index - 1, "->", node$left_child - 1)
    if (index == 1) {
      edge_info_left <- paste(edge, '[labeldistance=2.5, labelangle=45, headlabel="True"];')
    }
    else {
      edge_info_left <- paste(edge, " ;")
    }
  }
  else {
    edge_info_right <- NULL
  }

  if (!is.null(node$right_child)) {
    edge <- paste(index - 1, "->", node$right_child - 1)
    if (index == 1) {
      edge_info_right <- paste(edge, '[labeldistance=2.5, labelangle=-45, headlabel="False"]')
    } else {
      edge_info_right <- paste(edge, " ;")
    }
  } else {
    edge_info_right <- NULL
  }

  variable_name <- tree$columns[node$split_variable]
  node_info <- paste(index - 1, '[label="', variable_name, "<=", round(node$split_value, 2), '"] ;')

  this_lines <- paste(node_info,
    edge_info_left,
    edge_info_right,
    sep = "\n"
  )

  left_child_lines <- ifelse(!is.null(node$left_child),
    create_dot_body(tree, index = node$left_child),
    NULL
  )

  right_child_lines <- ifelse(!is.null(node$right_child),
    create_dot_body(tree, index = node$right_child),
    NULL
  )

  lines <- paste(this_lines, left_child_lines, right_child_lines, sep = "\n")

  return(lines)
}

#' Export a tree in DOT format.
#' This function generates a GraphViz representation of the tree,
#' which is then written into `dot_string`.
#' @param tree the tree to convert
#' @keywords internal
export_graphviz <- function(tree) {
  header <- "digraph nodes { \n node [shape=box] ;"
  footer <- "}"
  body <- create_dot_body(tree)

  dot_string <- paste(header, body, footer, sep = "\n")

  return(dot_string)
}

#' Plot a policy_tree tree object.
#' @param x The tree to plot.
#' @param leaf.labels An optional character vector of leaf labels for each treatment.
#' @param ... Additional arguments (currently ignored).
#'
#' @method plot policy_tree
#' @examples
#' # Plot a policy_tree object
#' \dontrun{
#' n <- 250
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' W <- as.factor(sample(c("A", "B", "C"), n, replace = TRUE))
#' Y <- X[, 1] + X[, 2] * (W == "B") + X[, 3] * (W == "C") + runif(n)
#' multi.forest <- grf::multi_arm_causal_forest(X = X, Y = Y, W = W)
#' Gamma.matrix <- double_robust_scores(multi.forest)
#' tree <- policy_tree(X, Gamma.matrix, depth = 2)
#' plot(tree)
#'
#' # Provide optional names for the treatment names in each leaf node
#' # `action.names` is by default the column names of the reward matrix
#' plot(tree, leaf.labels = tree$action.names)
#' # Providing a custom character vector
#' plot(tree, leaf.labels = c("treatment A", "treatment B", "placebo C"))
#'
#' # Saving a plot in a vectorized SVG format can be done with the `DiagrammeRsvg` package.
#' install.packages("DiagrammeRsvg")
#' tree.plot = plot(tree)
#' cat(DiagrammeRsvg::export_svg(tree.plot), file = 'plot.svg')
#' }
#' @export
plot.policy_tree <- function(x, leaf.labels = NULL, ...) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package \"DiagrammeR\" must be installed to plot trees.")
  }

  n.actions <- x$n.actions
  if (is.null(leaf.labels)) {
    leaf.labels <- paste("leaf node\n action =", 1:n.actions)
  } else if (length(leaf.labels) != n.actions) {
    stop("If provided, `leaf.labels` should be a vector with leaf labels for each treatment 1,..,K")
  }
  x$leaf.labels <- leaf.labels

  dot_file <- export_graphviz(x)
  DiagrammeR::grViz(dot_file)
}
