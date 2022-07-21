#' @useDynLib jsonlite C_simplify
simplify_c <- function(x) {
  .Call(C_simplify, x)
}
