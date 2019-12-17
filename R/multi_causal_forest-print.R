#' Print a multi_causal_forest object.
#' @param x The object to print.
#' @param ... Additional arguments (currently ignored).
#'
#' @method print multi_causal_forest
#' @export
print.multi_causal_forest <- function(x, ...) {
  cat("m-grf object of type", class(x), "\n")
  cat("Fitted one vs all forests: \n")

  print(x$forests)
}
