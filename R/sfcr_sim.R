#' Simulate a stock-flow consistent model
#'
#' The \code{sfcr_sim} function is used to simulate a SFC model. With adequate number of
#' periods, the simulated model should converge to a steady state scenario.
#'
#' @param equations A list containing all the equations of the model to be simulated. The equations
#' must be written in R syntax, with the left-hand side separated from the right-hand side
#' by a tilde \code{~}.
#' @param periods A number specifying the total number of periods of the model to be simulated.
#' @param exogenous,parameters,initial Lists of exogenous variables, parameters, and initial
#' values. They should be written as equations using the R syntax.
#' @param hidden Named list that identify the two variables that make the hidden equality
#' in the SFC model, e.g., \code{list("H_h" = "H_s")}. Defaults to NULL.
#' If \code{hidden} is supplied, the model will evaluate if the hidden equation is satisfied.
#' @param max_iter Maximum iterations allowed per period.
#' @param .hidden_tol Error tolerance to accept the equality of the hidden equation. Defaults to 1.
#'
#' @return A \code{sfcr_tbl}.
#'
#' @details The output of a \code{sfcr_sim()} is a \code{sfcr_tbl}. The only difference between
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
#' The algorithm finds the block of independent equations sequentially.
#' It first looks for all variables that depends only on exogenous variables or
#' on lagged values. It saves these variables as the first block and eliminate
#' them from the adjacency matrix.
#' It repeats this process until all blocks are identified or until it finds
#' a cycle.
#' If a cycle is found, it jumps to the bottom and finds all variables that
#' does not have any children. The algorithm assign these variables to the
#' end of the block list and eliminates them sequentially until it reaches
#' the cycle block. This algorithm is inspired by the algorithm of
#' \insertCite{godin2018pksfc}{Rdpack}
#'
#' All the cyclical variables are treated as a single cycle. Hence, this algorithm
#' is unable to identify independent cycles. However, this is not a common
#' structure of SFC models.
#'
#' The model is simulated using the Gauss Seidel algorithm, as described
#' by \insertCite{kinsella2011gauss}. It calculates the values of each
#' block of independent equations sequentially. When the absolute difference
#' between the value calculated in the new round and the value calculated
#' in the previous round divided by the value of the previous round for
#' all variables in the block falls below 1e-4, the model jumps to the
#' next block.
#'
#'
#' @example inst/examples/example_sfcr_sim.R
#'
#' @export
#'
#' @author João Macalós
#'
sfcr_sim <- function(equations, exogenous, parameters, periods, initial = NULL, hidden = NULL, max_iter = 350, .hidden_tol = 1) {

  external <- dplyr::bind_rows(.eq_as_tb(exogenous), .eq_as_tb(parameters))

  s1 <- .sfcr_find_order(equations)

  if (mean(grepl('rnorm|rbinom|runif', s1$rhs)) > 0) {stop ("Please define random variations as an external parameter.")}

  s2 <- .prep_equations(s1, external)

  s3 <- .make_matrix(s2, external, periods, initial = initial)

  s4 <- .sfcr_gauss_seidel(s3, s2, periods, max_iter)

  if (!is.null(hidden)) {
    is_hidden_true <- .sfcr_is_hidden_true(s4, hidden)

    if (isTRUE(is_hidden_true < .hidden_tol)) stop("Hidden equation is not fulfilled. Check the model try again. If the problem persists, try `hidden = NULL` to see if it is related to the tolerance level.")
  }

  s5 <- tibble::tibble(data.frame(s4)) %>%
    dplyr::mutate(period = dplyr::row_number()) %>%
    dplyr::select(-tidyselect::contains('block')) %>%
    dplyr::select(period, tidyselect::everything())

  attr(s5, "matrix") <- s4
  attr(s5, "calls") <- s2

  class(s5) <- c("sfcr_tbl", "tbl_df", "tbl", "data.frame")

  return(s5)
}
