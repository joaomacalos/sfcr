#' Check if difference is below a given tolerance level
#'
.is_under_tol <- function(x, y, tol = 0.1) {
  abs(x - y) < tol
}

#' Check if hidden equation condition is validated
#'
.sfcr_is_hidden_true <- function(m, hidden) {
  h1 <- names(hidden)
  h2 <- hidden

  is_hidden_true <- .is_under_tol(m[, h1], m[, h2])
  is_hidden_true <- sum(is_hidden_true) / length(is_hidden_true)

  return(is_hidden_true)
}
