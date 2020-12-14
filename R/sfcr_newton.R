#' Newton-Raphson solver implemented with \code{rootSolve::multiroot()}
#'
#' @param m The initialized matrix obtained with \code{.make_matrix()}.
#' @param equations Prepared equations with \code{.prep_equations()}.
#' @param periods Total number of rows (periods) in the model.
#' @param max_ite Maximum number of iterations allowed per block per period.
#' @param tol Tolerance accepted to determine convergence.
#' @param ... Extra parameters to pass to \code{rootSolve::multiroot()}.
#'
#'
#' @details This function implements the Newton-Raphson method to solve the cyclical
#' blocks of equations. It relies on the \code{multiroot()} function from \code{rootSolve}.
#'
#' @author João Macalós
#'
#' @keyword Internal
#'
.sfcr_newton <- function(m, equations, periods, max_ite, tol, ...) {

  blocks <- unique(sort(equations$block))

  equations_id <- purrr::map(blocks, ~equations[, "id"][equations[, "block"] == .x])

  cnd_statements <- equations %>%
    dplyr::filter(stringr::str_detect(.data$rhs, "if"),
                  stringr::str_detect(.data$rhs, "else")) %>%
    dplyr::pull(block)

  eqs2 <- equations %>%
    dplyr::mutate(lhs2 = gsub(.pvar(.data$lhs), "m\\[.i, '\\1'\\]", .data$lhs, perl = T)) %>%
    dplyr::mutate(rhs2 = paste0(.data$rhs, " - ", .data$lhs2)) %>%
    dplyr::mutate(lhs2 = stringr::str_replace_all(.data$lhs2, c("\\[" = "\\\\[", "\\]" = "\\\\]")))

  blk <- purrr::map(blocks, ~eqs2[eqs2$block == .x,])

  blk <- purrr::map(blk, .prep_broyden)

  block_names <- purrr::map(blocks, ~paste0("block", .x))

  ## Parsed non-linear expressions (for nleqslv)
  exs_nl <- purrr::map(blk, function(.X) purrr::map(.X$rhs2, ~rlang::parse_expr(.x)))

  ## Parsed linear expressions (for Gauss Seidel)
  exs_l <- purrr::map(blk, function(.X) purrr::map(.X$rhs, ~rlang::parse_expr(.x)))

  block_foo <- function(.x) {
    .y <- numeric(length(exs))
    for (.id in seq_along(exs)) {
      .y[.id] <- eval(exs[[.id]])
    }
    .y
  }


  for (.i in 2:periods) {
    for (.b in blocks) {

      block <- blk[[.b]]
      idvar_ <- equations_id[[.b]]

      ## CND statement must be dealt separately
      if (.b %in% cnd_statements) {

        m[.i, idvar_] <- eval(exs_l[[.b]][[1]])

      } else {

        # If acyclical block --> deterministic
        if (vctrs::vec_size(block) == 1) {

          m[.i, idvar_] <- eval(exs_l[[.b]][[1]])

        } else {

          xstart <- m[.i-1, idvar_]
          exs <- exs_nl[[.b]]

          x <- rootSolve::multiroot(block_foo, xstart, max_ite, ctol = tol, ...)

          for (.v in seq_along(x$root)) {
            m[.i, idvar_[[.v]]] <- x$root[.v]
            m[.i, block_names[[.b]]] <- x$iter
          }

        }
      }




    }

  }

  return(m)
}
