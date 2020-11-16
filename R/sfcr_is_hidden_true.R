#' Check if difference is below a given tolerance level
#'
#'@param y,x Numeric values
#'@param tol Numeric value determining the tolerance for the test
#'
.is_under_tol <- function(x, y, tol = 0.1) {
  abs(x - y) < tol
}

#' Check if hidden equation condition is validated
#'
#' @param m A matrix simulated with \code{sfcr_gauss_seidel()}.
#' @param hidden A named list containing the hidden equation.
#' @param .hidden_tol The tolerance to accept equality in the hidden equation
#'
.sfcr_is_hidden_true <- function(m, hidden, .hidden_tol) {
  h1 <- names(hidden)
  h2 <- hidden

  # Rows
  is_hidden_true <- .is_under_tol(m[, h1], m[, h2], .hidden_tol)
  # Columns
  #is_hidden_true <- .is_under_tol(m[h1, ], m[h2, ], .hidden_tol)

  is_hidden_true <- sum(is_hidden_true) / length(is_hidden_true)

  return(is_hidden_true)
}
