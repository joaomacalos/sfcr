#' Check for missing endogenous variables
#'
#' @inheritParams .sfcr_gauss_seidel
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.sfcr_eqs_check <- function(m, equations) {

  exprs <- purrr::map(equations$rhs, function(x) parse(text=x))

  for (.i in 2) {
    for (var in seq_along(equations$rhs)) {
      tryCatch(
        m[.i, equations$lhs[[var]]] <- eval(exprs[[var]]),
        error = function(err) {
          msg <- conditionMessage(err)

          if (grepl("non-numeric", msg)) {
            msg <- "An endogenous variable is missing. Check the equations and try again.\n\nTip: look for the variable not surrounded by `m[.i, ]` after `Error in` in this message."
          }

          err$message <- msg
          stop(err)
        }
      )
    }
  }
}


#' new_sfcr_tbl constructor
#'
#' @param tbl A tibble
#' @param matrix a Matrix
#' @param calls Calls tibble
#' @param external Vector with external names
#'
#' @author João Macalós
#'
#' @keywords internal
#'
new_sfcr_tbl <- function(tbl, matrix, calls, external) {
  stopifnot(inherits(tbl, "tbl_df"))
  stopifnot(inherits(matrix, "matrix"))
  stopifnot(inherits(calls, "tbl_df"), names(calls) == c("lhs", "rhs", "block", "id"))
  stopifnot(inherits(external, "tbl_df"))

  structure(tbl,
            class = c("sfcr_tbl", "tbl_df", "tbl", "data.frame"),
            matrix = matrix,
            calls = calls,
            external = external)
}


#' Get Matrix form of \code{sfcr_tbl} object
#'
#' @param sfcr_tbl A \code{sfcr_tbl} object.
#'
#' @author João Macalós
#'
#' @export
#'
sfcr_get_matrix <- function(sfcr_tbl) {
  abortifnot(inherits(sfcr_tbl, "sfcr_tbl"), "Please provide a valid 'sfcr_tbl' object.")

  attr(sfcr_tbl, "matrix")
}


#' Get block structure of a \code{sfcr_tbl} object
#'
#' @param sfcr_tbl A \code{sfcr_tbl} object.
#'
#' @author João Macalós
#'
#' @export
#'
sfcr_get_blocks <- function(sfcr_tbl) {
  abortifnot(inherits(sfcr_tbl, "sfcr_tbl"), "Please provide a valid 'sfcr_tbl' object.")

  tbl <- attr(sfcr_tbl, "calls")

  tbl %>%
    dplyr::select(endogenous = .data$lhs, .data$block) %>%
    dplyr::arrange(.data$block)

}


#' Abort if duplicated variables
#'
#' @param dups name(s) of offending variables
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.abort_if_dup <- function(dups) {
  if (length(dups) == 1) {
    message = paste0("The endogenous variable `", dups, "` was defined more than once. Please check your model and try again.")
  } else {
    message = paste0("The endogenous variables `", paste0(dups, collapse = ", "), "` were defined more than once. Please check your model and try again.")
  }
  rlang::abort(message = message)
}


#' Check shocks for length consistency and warn about risks of using exogenous series
#'
#'
#' This function makes two checks:
#'
#' 1) The exogenous variable is a constant that is repeated over time;
#' 2) The exogenous variable has exactly the same length as the shock.
#'
#' Furthermore, it throws a warning that using exogenous series in a shock can lead to unexpected
#' behavior if the length of the shock is not the same as the periods in the scenario.
#'
#' @param external An .eq_as_tb() tibble with external variables.
#' @param periods The periods of the baseline model.
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.check_external_consistency <- function(external, periods=periods) {

  # We remove `sfcr_random` from sanity checks because it is certain that it will not lead
  # to mistakes.

  external <- dplyr::filter(external, stringr::str_detect(.data$rhs, "sfcr_random", negate=TRUE))

  if (is.null(external$rhs)) {
    return()
  }

  # Parse vars
  parse_vars <- purrr::map(external$rhs, ~eval(parse(text=.x)))
  vars_length <- purrr::map_dbl(parse_vars, length)

  if (mean(vars_length) > 1) {
    abortifnot(all(vars_length %in% c(1, periods)), "The exogenous variables must have either length 1 or exactly the same length as the baseline model.")

    # Warning
    rlang::warn("The utilization exogenous series within a baseline model is not recommended and will be disallowed in the future. Be careful when using this functionality.", .frequency_id = "scenario_warn", .frequency = "once")

  }

}


#' Simulate the baseline scenario of a stock-flow consistent model
#'
#' The \code{sfcr_baseline()} function is used to simulate a SFC model.
#'
#' @param equations A \code{sfcr_set} containing all the equations of the model to be simulated. The equations
#' must be written with the R formula syntax, with the left-hand side separated from the right-hand side
#' by a twiddle \code{~}.
#' @param periods A number specifying the total number of periods of the model to be simulated.
#' @param external,initial A \code{sfcr_set} of external variables (exogenous and parameters) or of initial
#' values. They should be written as equations using the R syntax.
#' @param hidden Named object that identify the two variables that make the hidden equality
#' in the SFC model, e.g., \code{c("H_h" = "H_s")}. Defaults to NULL.
#' If \code{hidden} is supplied, the model will evaluate if the hidden equation is satisfied.
#' @param max_iter Maximum iterations allowed per period.
#' @param .hidden_tol Error tolerance to accept the equality of the hidden equation. Defaults to 1.
#' In growth models, computational errors might buildup in the hidden equation, which renders any absolute
#' comparison inadequate. For such models, please turn \code{rhtol} to \code{TRUE}, and set the value
#' of \code{.hidden_tol} accordingly. See details for further information.
#' @param tol Tolerance accepted to determine convergence.
#' @param method The method to use to find a solution. Defaults to "Broyden".
#' @param rhtol A logical argument that defines whether the a relative measure is used to evaluate
#' the hidden equation or not. Defaults to \code{FALSE}, i.e., a absolute measure is used.
#' @param ... Extra arguments to pass to \code{rootSolve::multiroot()} function if "Newton" method
#' is selected.
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
#' right-hand side by a twiddle. Variables that represent lags of endogenous or exogenous
#' variables must be followed by \code{[-1]}. See examples for details on the syntax.
#'
#' Before solving the system of equations, two consecutive depth-first searches identify
#' and order the blocks of independent equations in the system. The system is then solved
#' sequentially, i.e., the variables that depend only on lagged or exogenous values are evaluated
#' first, and then the variables that depends on these variables, etc. The solving algorithms
#' are only applied to the blocks of mutually dependent equations. The great \code{igraph}
#' package is used to implement the two consecutive depth-first searches.
#'
#' * Methods:
#'
#'  The \code{sfcr} package provides three algorithms to solve the blocks of cyclical equations:
#'  the Gauss-Seidel algorithm, the Broyden algorithm, and the Newton-Raphson algorithm. The
#'  default method is "Broyden" as it tends to be fastest one.
#'
#'  See \insertCite{kinsella2011gauss}{sfcr} for details on the Gauss-Seidel algorithm and
#'  \insertCite{peressini1988nonlinear}{sfcr} for details on the Broyden and Newton-Raphson
#'  algorithms.
#'
#'  The "Broyden" algorithm uses the \code{rootSolve::jacobian.full()} function to get the
#'  initial Jacobian matrix, and compiled code from \code{RcppArmadillo} to invert the
#'  jacobians. See also https://www.math.usm.edu/lambers/mat419/lecture11.pdf.
#'
#'  The Gauss Seidel algorithm is implemented as described by \insertCite{kinsella2011gauss}{sfcr}.
#'  Finally, the "Newton" method uses the \code{rootSolve::multiroot()} function to solve the system.
#'
#'
#' * Hidden equation:
#'
#'  One of the defining aspects of a SFC model is its water tight accounting. One way
#'  to check whether the model was correctly defined is to see if the hidden (redundant)
#'  equation is satisfied after the model is simulated. In stationary models, an absolute
#'  comparison should suffice as the model converges to a stationary state. However,
#'  growth models converge to a stable growth rate where stocks are perpetually increasing.
#'  It is inadequate to use a absolute comparison in such models. In these cases, the
#'  \code{rhtol} argument ("relative hidden tolerance") must be set to \code{TRUE} in order
#'  to perform a relative comparison. The relative comparison evaluates the numerical
#'  discrepancy in the hidden equation as a ratio of one of its elements. For example,
#'  if \code{hidden = c("Bbs" = "Bbd")}, the hidden equation will be evaluated according to
#'  the following steps:
#'
#'  1. \code{d = (Bbs - Bbd)}
#'  1. \code{isTRUE(d/Bbs < .hidden_tol)}
#'
#'  In general, the \code{.hidden_tol} argument should be set to a small number (e.g. 1e-6).
#'  The function will check that this proportion remains the same for all simulated periods.
#'
#'
#' @references
#'
#' \insertRef{kinsella2011gauss}{sfcr}
#' \insertRef{peressini1988nonlinear}{sfcr}
#'
#'
#' @example inst/examples/example_sfcr_baseline.R
#'
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @author João Macalós, \email{joaomacalos@@gmail.com}
#'
sfcr_baseline <- function(equations, external, periods, initial = NULL, hidden = NULL, max_iter = 350, .hidden_tol = 0.1, tol = 1e-8, method = "Broyden", rhtol = FALSE, ...) {

  match.arg(method, c("Gauss", "Newton", "Broyden"))

  external <- .eq_as_tb(external)

  # Check for external length consistency:
  .check_external_consistency(external, periods)

  s1 <- .sfcr_find_order(equations)

  # Checks:

  # 1. Check for duplicates

  is_dup <- duplicated(s1$lhs)
  if (any(is_dup)) {
    dups <- s1$lhs[which(is_dup)]
    .abort_if_dup(dups)
  }

  # 2. Check for invalid variable name (.i)

  is_invalid_name <- stringr::str_detect(paste0(c(s1$lhs, s1$rhs, external$lhs), collapse = " "), "(?<=[:blank:]|^)\\.i(?=[:blank:]|$)")

  if (isTRUE(is_invalid_name)) {
    rlang::abort("Invalid name detected! Please don't use \".i\" to name any variable.")
  }

  # 3. Check that a variable defined as external was not defined as endogenous:

  if (mean(external$lhs %in% s1$lhs) > 0) {
    p1 <- which((external$lhs %in% s1$lhs) > 0)
    rlang::abort(paste0("The variable(s) in `", paste0(external$lhs[p1], collapse = ", "), "` was/were defined both as endogenous and external variable(s). Please check the equations and try again."))
  }

  # 4. Random vars not defined together with the endogenous variables
  # If defined there, the random numbers will change at each iteration and not at each period.

  if (mean(grepl('rnorm|rbinom|runif', s1$rhs)) > 0) {stop ("Please define random variations as an external parameter.")}

  # 5. Check initial names are correct

  if (!is.null(initial)) {
    init_vars <- .eq_as_tb(initial)$lhs
    if (mean(!(init_vars %in% c(external$lhs, s1$lhs))) > 0) {
      p1 <- which(!(init_vars %in% c(external$lhs, s1$lhs)))
      rlang::abort(paste0("The variable(s) in `", paste0(init_vars[p1], collapse = ", "), "` was/were given a initial value but are not present in the model. Check the equations and try again."))
    }
  }

  # 6. Check that exogenous variables passsed as a sequence/series and not as a constant
  # have the same length as the periods supplied

  # check_length_exogenous1 = purrr::map(external$rhs, function(x) eval(parse(text=x)))
  # check_length_exogenous2 = purrr::map_dbl(check_length_exogenous1, length)

  # 7. Check that periods are bigger than one
  if (periods < 2) {
    rlang::abort("The minimum periods required to simulate a model is 2.")

  }

  # if (mean(check_length_exogenous2) > 1) {
  #   rlang::abort("At the baseline construct level, exogenous variables can only be supplied as a constant.")
  #  }

  s2 <- .prep_equations(s1, external)

  s3 <- .make_matrix(s2, external, periods, initial = initial)

  .sfcr_eqs_check(s3, s2)

  if (method == "Gauss") {
    s4 <- .sfcr_gauss_seidel(s3, s2, periods, max_iter, tol)
  } else if (method == "Newton") {
    s4 <- .sfcr_newton(s3, s2, periods, max_iter, tol, ...)
  } else {
    s4 <- .sfcr_broyden(s3, s2, periods, max_iter, tol)
  }

  # Check if hidden is fulfilled

  .h1 <- names(hidden)
  .h2 <- hidden

  # If rhtol is TRUE (relative discrepancy is on)

  if (isTRUE(rhtol)) {
    abortifnot(all(abs(s4[, .h1] - s4[, .h2])/(s4[, .h1] + 1e-15) < .hidden_tol), "Hidden equation is not fulfilled. Check the model try again. If the problem persists, try `hidden = NULL` to see if it is related to the tolerance level.")
  }

  # Else absolute discrepancy
  else {
    abortifnot(all(abs(s4[, .h1] - s4[, .h2]) < .hidden_tol), "Hidden equation is not fulfilled. Check the model try again. If the problem persists, try `hidden = NULL` to see if it is related to the tolerance level.")
  }

  # Rows
  s5 <- tibble::tibble(data.frame(s4))
  s5["period"] <- 1:nrow(s5)
  s5 <- dplyr::select(s5, -tidyselect::contains("block"))
  s5 <- dplyr::select(s5, .data$period, tidyselect::everything())

  x <- new_sfcr_tbl(tbl = s5, matrix = s4, calls = s2, external = external)

  # Raise message to suggest the utilization of `sfcr_random()` instead of `rnorm()` and related functions
  if (any(purrr::map_lgl(external$rhs, ~stringr::str_detect(.x, "rnorm\\(|rbinom\\(|runif\\(")))) {
    rlang::inform("The utilization of `rnorm()` and related functions to add random variation to endogenous variables will be disallowed in the future. Use `sfcr_random()` instead.")
  }

  return(x)
}


