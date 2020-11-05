#' Re-wrote the equations with the correct matrix syntax that will be used to evaluate
#' the expressions inside the Gauss Seidel algorithm
#'
#' @param ordered_eqs ordered equations after passing through \code{.sfcr_find_order()} function.
#' @param external Tibble of exogenous values and parameters, already separated with
#' \code{.eq_as_tb()} function.
#'
#' @author João Macalós
.prep_equations <- function(ordered_eqs, external) {

  pend <- paste0("(?<![[:alnum:]])(", paste0(ordered_eqs$lhs, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")
  pendlag <- paste0("(?<![[:alnum:]])(", paste0(ordered_eqs$lhs, collapse = "|"), ")(?=___)")
  pexg <- paste0("(?<![[:alnum:]])(", paste0(c(external$lhs), collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")
  pexglag <- paste0("(?<![[:alnum:]])(", paste0(c(external$lhs), collapse = "|"), ")(?=___)")

  x <- ordered_eqs %>%
    dplyr::mutate(rhs = gsub(pend, "m\\[i,'\\1'\\]", rhs, perl = T),
                  rhs = gsub(pendlag, "m\\[i-1,'\\1'\\]", rhs, perl = T),
                  rhs = gsub(pexg, "m\\[i,'\\1'\\]", rhs, perl = T),
                  rhs = gsub(pexglag, "m\\[i-1,'\\1'\\]", rhs, perl = T),
                  rhs = gsub("___", "", rhs),
                  id = dplyr::row_number())

  # Uncomment to loop on a list instead of a matrix
  # x <- ordered_eqs %>%
  #  dplyr::mutate(rhs = gsub(pend, "m\\[\\['\\1'\\]\\]\\[\\[i\\]\\]", rhs, perl = T),
  #                rhs = gsub(pendlag, "m\\[\\['\\1'\\]\\]\\[\\[i-1\\]\\]", rhs, perl = T),
  #                rhs = gsub(pexg, "m\\[\\['\\1'\\]\\]\\[\\[i\\]\\]", rhs, perl = T),
  #                rhs = gsub(pexglag, "m\\[\\['\\1'\\]\\]\\[\\[i-1\\]\\]", rhs, perl = T),
  #                rhs = gsub("___", "", rhs),
  #                id = dplyr::row_number())

  return(x)
}

#' Make the underlying matrix that will be modified in place with the Gauss Seidel
#' algorithm
#'
#' @param equations Prepared equations.
#' @param external Exogenous and parameters as tibble.
#' @param periods Total number of rows.
#' @param initial Initial values, if supplied.
#'
#' @author João Macalós
#'
.make_matrix <- function(equations, external, periods, initial = NULL) {

  # If no initial values supplied, start with a 1
  ends <- rep(1, vctrs::vec_size(equations$lhs))
  names(ends) <- equations$lhs

  # Exogenous variables are supplied
  #exgs <- unlist(c(exogenous, parameters))
  exgs <- rep(1, vctrs::vec_size(external$lhs))
  exgs_names <- external$lhs
  exg_exprs <- purrr::map(external$rhs, function(x) parse(text=x))

  # Blocks of independent equations
  blocks <- unique(equations$block)
  blocks <- paste0("block", blocks)
  lblocks <- rep(0, vctrs::vec_size(blocks))
  names(lblocks) <- blocks

  mcols <- vctrs::vec_size(ends) + vctrs::vec_size(exgs) + vctrs::vec_size(lblocks)
  mnames <- c(names(ends), exgs_names, names(lblocks))

  # Matrix with variables
  m1 <- matrix(c(ends, exgs, lblocks), nrow = periods, ncol = mcols, dimnames = list(1:periods, mnames), byrow = T)

  for (var in seq_along(exgs_names)) {
    m1[, exgs_names[[var]]] <- eval(exg_exprs[[var]])
  }

  # All variables start at 1 (including exogenous)
  # Otherwise problems may arise if some endogenous
  # variable depends on lagged exogenous

  m1[1, ] <- 1

  if (!is.null(initial)) {
    initial <- .eq_as_tb(initial)
    initial_names <- initial$lhs
    initial_exprs <- purrr::map(initial$rhs, function(x) parse(text = x))

    for (var in seq_along(initial_names)) {
      m1[1, initial_names[[var]]] <- eval(initial_exprs[[var]])
    }
  }


  # Loop on a list instead of a matrix
  #m1 <- as.list(data.frame(m1))

  return(m1)
}


#' Gauss Seidel algorithm
#'
#' @importFrom Rdpack reprompt
#'
#' @param m The initialized matrix obtained with \code{.make_matrix()}.
#' @param equations Prepared equations with \code{.prep_equations()}.
#' @param periods Total number of rows (periods) in the model.
#' @param max_ite Maximum number of iterations allowed per block per period.
#'
#' @details This is the main algorithm of the package. It simulates the model
#' by recursion with the help of four nested for loops. At each round of
#' iteration, the values calculated are compared to the previous values. If
#' the difference is below 1e-4, the round of calculations have converged
#' and the algorithm jump to the next block of equations.
#'
#' The algorithm modifies a matrix in place to optimize its performance.
#'
#' @author João Macalós
#'
.sfcr_gauss_seidel <- function(m, equations, periods, max_ite) {

  exprs <- purrr::map(equations$rhs, function(x) parse(text=x))

  checks <- rep(0, vctrs::vec_size(equations$lhs))
  names(checks) <- equations$lhs

  holdouts <- rep(3, vctrs::vec_size(equations$lhs))
  names(holdouts) <- equations$lhs

  blocks <- unique(equations$block)


  for (i in 2:periods) {
    for (block in blocks) {
      block_name <- paste0('block', block)

      # Just keeping this single line inside the loop call was more than doubling execution time
      equations_id <- equations[, 'id'][equations[, 'block'] == block]

      for (ite in 1:max_ite) {
        for (var in equations_id) {

          m[i, equations$lhs[[var]]] <- eval(exprs[[var]])
          #m[[equations$lhs[[var]]]][[i]] <- eval(exprs[[var]])

          checks[[var]] <- abs((m[[i, var]] - holdouts[[var]]) / (holdouts[[var]] + 1e-15))
          #checks[[var]] <- abs( (m[[var]][[i]] - holdouts[[var]]) / (holdouts[[var]] + 1e-15))

          holdouts[[var]] <- m[[i, var]]
          #holdouts[[var]] <- m[[var]][[i]]
        }

        m[i, block_name] <- ite
        #m[[paste0('block', block)]][[i]] <- ite

        if (all(checks < 1e-4)) {break}

      }
    }
  }
  return(m)
}
