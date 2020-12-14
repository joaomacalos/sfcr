#' Gauss Seidel algorithm
#'
#' @param m The initialized matrix obtained with \code{.make_matrix()}.
#' @param equations Prepared equations with \code{.prep_equations()}.
#' @param periods Total number of rows (periods) in the model.
#' @param max_ite Maximum number of iterations allowed per block per period.
#'
#' @inheritParams sfcr_baseline
#'
#' @details This algorithm simulates the model by recursion by using
#' nested for loops. At each round of iteration, the values calculated
#' are compared to the previous values. If the difference is below
#' a tolerance value set by the user, the round of calculations have converged
#' and the algorithm jump to the next block of equations.
#'
#' The algorithm modifies a matrix in place to optimize its performance.
#'
#' @author João Macalós
#'
#' @keyword Internal
#'
.sfcr_gauss_seidel <- function(m, equations, periods, max_ite, tol) {

  exprs <- purrr::map(equations$rhs, function(x) parse(text=x))

  checks <- rep(0, vctrs::vec_size(equations$lhs))
  names(checks) <- equations$lhs

  #holdouts <- rep(3, vctrs::vec_size(equations$lhs))
  holdouts <- c(m[1, 1:vctrs::vec_size(equations$lhs)])
  names(holdouts) <- equations$lhs

  blocks <- unique(sort(equations$block))

  equations_id <- lapply(blocks, function(x) equations[, "id"][equations[, "block"] == x])

  block_names <- lapply(blocks, function(x) paste0("block", x))

  # safe check
  # TODO: tryCatch only once as it damages performance

  for (.i in 2:periods) {
    for (block in seq_along(blocks)) {

      id <- equations_id[[block]]

      # If 1 variable in the block, it is deterministic and no iteration is required.
      if (length(id) == 1) {
        m[.i, id] <- eval(exprs[[id]])
        m[.i, block_names[[block]]] <- 1
      }

      # If cyclical block, use Gauss-Seidel algorithm
      else {

        for (ite in 1:max_ite) {
          for (var in id) {

            m[.i, var] <- suppressMessages(eval(exprs[[var]]))

            checks[[var]] <- suppressMessages(abs(m[.i, var] - holdouts[[var]])/(holdouts[[var]] + 1e-15))

          }

          m[.i, block_names[[block]]] <- ite

          if (any(!is.finite(checks[id]))) rlang::abort(message = "Gauss-Seidel algorithm failed to converge. Check the initial values to exclude any division by zero or other invalid operations. If the problem persists, try a different method.")

          if (all(checks[id] < tol)) {
            break
          } else {
            for (var in id) {
              holdouts[[var]] <- m[.i, var]
            }
          }

        }
      }
    }
  }
  return(m)
}
