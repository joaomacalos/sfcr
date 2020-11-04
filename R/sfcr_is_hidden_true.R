.is_equal_tol <- function(x, y, tol = 0.1) {
  abs(x - y) < tol
}

.sfcr_is_hidden_true <- function(m, hidden, .hidden_tolerance = 0.1) {
  h1 <- names(hidden)
  h2 <- hidden

  is_hidden_true <- .is_equal_tol(m[, h1], m[, h2])
  is_hidden_true <- sum(is_hidden_true) / length(is_hidden_true)

  return(is_hidden_true)
}
