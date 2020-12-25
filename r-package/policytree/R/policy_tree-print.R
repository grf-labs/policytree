#' Print a policy_tree object.
#' @param x The tree to print.
#' @param ... Additional arguments (currently ignored).
#'
#' @method print policy_tree
#' @export
print.policy_tree <- function(x, ...) {
  action.names <- if (all(x$action.names == 1:x$n.actions)) {
    1:x$n.actions
  } else {
    paste0(1:x$n.actions, ": ", x$action.names)
  }
  cat("policy_tree object", "\n")
  cat("Tree depth: ", x$depth, "\n")
  cat("Actions: ", action.names, "\n")
  cat("Variable splits:", "\n")

  # Add the index of each node as an attribute for easy access.
  nodes <- lapply(1:length(x$nodes), function(i) {
    node <- x$nodes[[i]]
    node$index <- i
    return(node)
  })

  # Perform DFS to print the nodes (mimicking a stack with a list).
  frontier <- nodes[1]
  frontier[[1]]$depth <- 0
  while (length(frontier) > 0) {
    # Pop the first node off the stack.
    node <- frontier[[1]]
    frontier <- frontier[-1]

    output <- paste(rep("  ", node$depth), collapse = "")
    output <- paste(output, "(", node$index, ")", sep = "")

    if (node$is_leaf) {
      output <- paste(output, "* action:", node$action)
    } else {
      split.var <- node$split_variable
      split.var.name <- x$columns[split.var]
      output <- paste(output, "split_variable:", split.var.name, " split_value:", signif(node$split_value))

      left_child <- nodes[node$left_child]
      left_child[[1]]$depth <- node$depth + 1

      right_child <- nodes[node$right_child]
      right_child[[1]]$depth <- node$depth + 1

      frontier <- c(left_child, right_child, frontier)
    }
    cat(output, "\n")
  }
}
