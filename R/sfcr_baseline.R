#' new_sfcr_tbl constructor
#'
#' @param tbl A tibble
#' @param matrix a Matrix
#' @param calls Calls tibble
#' @param external Vector with external names
new_sfcr_tbl <- function(tbl, matrix, calls, external) {
  stopifnot(inherits(tbl, "tbl_df"))
  stopifnot(inherits(matrix, "matrix"))
  stopifnot(inherits(calls, "tbl_df"), names(calls) == c("lhs", "rhs", "block", "id"))
  stopifnot(inherits(external, "character"))

  structure(tbl,
            class = c("sfcr_tbl", "tbl_df", "tbl", "data.frame"),
            matrix = matrix,
            calls = calls,
            external = external)
}


#' Simulate the baseline scenario of a stock-flow consistent model
#'
#' The \code{sfcr_baseline()} function is used to simulate a SFC model. With adequate number of
#' periods, the simulated model should converge to a steady state scenario.
#'
#' @param equations A list containing all the equations of the model to be simulated. The equations
#' must be written in R syntax, with the left-hand side separated from the right-hand side
#' by a tilde \code{~}.
#' @param periods A number specifying the total number of periods of the model to be simulated.
#' @param external,initial List of external variables (exogenous and parameters) or of initial
#' values. They should be written as equations using the R syntax.
#' @param hidden Named object that identify the two variables that make the hidden equality
#' in the SFC model, e.g., \code{c("H_h" = "H_s")}. Defaults to NULL.
#' If \code{hidden} is supplied, the model will evaluate if the hidden equation is satisfied.
#' @param max_iter Maximum iterations allowed per period.
#' @param .hidden_tol Error tolerance to accept the equality of the hidden equation. Defaults to 1.
#' @param tol Tolerance accepted to determine convergence in the Gauss Seidel algorithm.
#'
#' @return A \code{sfcr_tbl}.
#'
#' @details The output of a \code{sfcr_baseline()} is a \code{sfcr_tbl}. The only difference between
#' a \code{sfcr_tbl} and a standard \code{tbl_df} is that the former has two extra attributes:
#' \code{matrix} and \code{call}. The \code{matrix} attribute, for example, can be accessed by
#' calling \code{attributes(sfcr_sim_object)$matrix}.
#' It is possible to see, in the matrix, the number of iterations required to calculate each
#' block of equations in the model.
#' The \code{call} attribute shows the blocks of equations and preserve the call that are used
#' internally.
#'
#' The \code{equations}, \code{exogenous}, and \code{parameters} arguments must be written
#' with the R formula syntax, i.e., the left-hand side of each item is separated to the
#' right-hand side by a tilde. Variables that represent lags of endogenous or exogenous
#' variables must be followed by \code{[-1]}. See examples for details on the syntax.
#'
#' This function uses two consecutive depth-first searches to determine the blocks
#' of independent equations. These blocks are used in turn to solve the model
#' sequentially, optimizing the simulation. The \code{igraph} package is used
#' to implement this algorithm.
#'
#' The model is simulated using the Gauss Seidel algorithm, as described
#' by \insertCite{kinsella2011gauss}{sfcr}. It calculates the values of each
#' block of independent equations sequentially. When the absolute difference
#' between the value calculated in the new round and the value calculated
#' in the previous round divided by the value of the previous round for
#' all variables in the block falls below 1e-4, the model jumps to the
#' next block.
#'
#' @seealso \code{\link[igraph]{components}}
#'
#' @references
#'
#' \insertRef{kinsella2011gauss}{sfcr}
#'
#'
#' @example inst/examples/example_sfcr_sim.R
#'
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @author João Macalós, \email{joaomacalos@@gmail.com}
#'
sfcr_baseline <- function(equations, external, periods, initial = NULL, hidden = NULL, max_iter = 350, .hidden_tol = 1, tol = 1e-8) {

  #external <- dplyr::bind_rows(.eq_as_tb(exogenous), .eq_as_tb(parameters))
  external <- .eq_as_tb(external)

  s1 <- .sfcr_find_order(equations)

  # Checks:

  # 1. Random vars not defined together with the endogenous variables
  # If defined there, the random numbers will change at each iteration and not at each period.

  if (mean(grepl('rnorm|rbinom|runif', s1$rhs)) > 0) {stop ("Please define random variations as an external parameter.")}

  # 2. Check that a variable defined as external was not defined as endogenous:

  if (mean(external$lhs %in% s1$lhs) > 0) {
    p1 <- which((external$lhs %in% s1$lhs) > 0)
    rlang::abort(paste0("The variable(s) in `", paste0(s1$lhs[p1], collapse = ", "), "` was/were defined both as endogenous and external variable(s). Please check the equations and try again."))
  }

  s2 <- .prep_equations(s1, external)

  s3 <- .make_matrix(s2, external, periods, initial = initial)

  .sfcr_eqs_check(s3, s2)

  s4 <- .sfcr_gauss_seidel(s3, s2, periods, max_iter, tol)

  if (!is.null(hidden)) {
    is_hidden_true <- .sfcr_is_hidden_true(s4, hidden, .hidden_tol)

  if (isTRUE(is_hidden_true < .hidden_tol)) stop("Hidden equation is not fulfilled. Check the model try again. If the problem persists, try `hidden = NULL` to see if it is related to the tolerance level.")
  }

  # Rows
  s5 <- tibble::tibble(data.frame(s4))
  s5["period"] <- 1:nrow(s5)
  s5 <- dplyr::select(s5, -tidyselect::contains("block"))
  s5 <- dplyr::select(s5, .data$period, tidyselect::everything())
  #s5 <- dplyr::mutate(s5, dplyr::across(-c(.data$period), ~round(.x, digits = 4)))

  x <- new_sfcr_tbl(tbl = s5, matrix = s4, calls = s2, external = external$lhs)


  #attr(s5, "matrix") <- s4

  # Columns
  # s5 <- tibble::tibble(data.frame(t(s4))) %>%
  #   dplyr::mutate(period = dplyr::row_number()) %>%
  #   dplyr::select(-tidyselect::contains('block')) %>%
  #   dplyr::select(period, tidyselect::everything())
  #
  # attr(s5, "matrix") <- t(s4)


  #attr(s5, "calls") <- s2

  #class(s5) <- c("sfcr_tbl", "tbl_df", "tbl", "data.frame")

  return(x)
}


