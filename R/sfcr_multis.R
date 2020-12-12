#' sfcr_mlt constructor
#'
#' @param multis A list with multiple \code{sfcr_tbl}
#' @param fixed The fixed argument
#'
new_sfcr_mlt <- function(multis, fixed) {
  stopifnot(inherits(multis[[1]], "sfcr_tbl"))
  stopifnot(inherits(fixed, c("sfcr_set", "sfcr_tbl", "sfcr_shock")))

  structure(multis,
            class = c("sfcr_mlt", "list"),
            fixed = fixed)
}



#' Simulate multiple SFC models at the same time
#'
#' The \code{sfcr_multis()} function is used to simulate multiple models
#' at the same time, returning a list of \code{sfcr_tbl}s.
#'
#' @details
#'
#' The \code{sfcr_multis()} function takes an \code{expanded} object and
#' a \code{fixed} to simulate multiple models that will share the content
#' of \code{fixed} but vary on the \code{expanded}.
#'
#' This function is a **generic**, which means that its implementation
#' depends on the class of the \code{expanded} argument.
#'
#' The available methods for the \code{sfcr_multis()} function depends
#' on the \code{expanded} argument. There are three possible methods:
#'
#' * \code{sfcr_mlt_set}:
#'   When the \code{sfcr_multis()} takes an \code{sfcr_mlt_external} class
#'   as the input of \code{expanded}, it must take a list of equations of
#'   the \code{sfcr_set} class as the \code{fixed} input. This method
#'   simulates many baseline models that accept the same set of equations
#'   and vary on the external variables supplied with the \code{expanded}
#'   argument.
#'
#' * \code{sfcr_mlt_shock}:
#'   When the \code{sfcr_multis()} takes an \code{sfcr_mlt_shock} class
#'   as the input of \code{expanded}, it must also take an object of
#'   \code{sfcr_tbl} class as the input of \code{fixed}. It will simulate
#'   multiple scenario models that takes the same baseline model
#'   and diverge on the content of the multiple shocks provided with the
#'   \code{expanded} argument that are applied to it.
#'
#' * \code{sfcr_mlt}:
#'   When the \code{sfcr_multis()} function takes a \code{sfcr_mlt} class
#'   object as the input of the \code{expanded} argument, a \code{sfcr_shock}
#'   object must be supplied with the \code{fixed} argument. This method
#'   simulates multiple scenario models that applies the same shock to a
#'   varying number of baseline models.
#'
#' @param expanded A \code{sfcr_mlt_set}, \code{sfcr_mlt_shock}, or a
#' \code{sfcr_mlt} object.
#' @param fixed A \code{sfcr_set}, \code{sfcr_tbl}, or \code{sfcr_shock} object.
#' @param ... Additional arguments to pass to the underlying implementation of the
#' \code{sfcr_baseline()} or \code{sfcr_scenario()} functions.
#'
#' @inheritParams sfcr_baseline
#'
#' @example inst/examples/example_sfcr_multis.R
#'
#' @author João Macalós
#'
#' @export
#'
sfcr_multis <- function(expanded, fixed, periods, ...) {
  UseMethod("sfcr_multis")
}


#' @method sfcr_multis sfcr_mlt_set
#'
#' @export
#'
sfcr_multis.sfcr_mlt_set <- function(expanded, fixed, periods = 50, ...) {

  abortifnot(inherits(fixed, "sfcr_set"), "Please supply a valid `sfcr_set` of equations with the `fixed` argument.")

  multis <- purrr::map(seq_along(expanded), function(x) {
    sfcr_baseline(equations = fixed, external = expanded[[x]], periods = periods, ...) %>%
      dplyr::mutate(simulation = x)
  })

  multis <- new_sfcr_mlt(multis, fixed)

  return(multis)
}

#' @export
print.sfcr_mlt <- function(x, ...) {
  attr(x, "class") <- NULL
  attr(x, "fixed") <- NULL
  print.default(x, ...)
}

#' @method sfcr_multis sfcr_mlt_shock
#'
#' @export
#'
sfcr_multis.sfcr_mlt_shock <- function(expanded, fixed, periods = 50, ...) {

  abortifnot(inherits(fixed, "sfcr_tbl"), "Please provide a valid `sfcr_tbl` with the `fixed` argument.")

  multis <- purrr::map(seq_along(expanded), function(x) {
    sfcr_scenario(fixed, scenario = list(expanded[[x]]), periods = periods, ...) %>%
      dplyr::mutate(simulation = x)
  })

  multis <- new_sfcr_mlt(multis, fixed)

  return(multis)
}

#' @method sfcr_multis sfcr_mlt
#'
#' @export
#'
sfcr_multis.sfcr_mlt <- function(expanded, fixed, periods = 50, ...) {

  abortifnot(inherits(fixed, "sfcr_shock"), "Please provid a valid `sfcr_shock` with `fixed` argument.")

  multis <- purrr::map(seq_along(expanded), function(x) {
    sfcr_scenario(expanded[[x]], scenario = fixed, periods = periods, ...) %>%
      dplyr::mutate(simulation = x)
  })

  multis <- new_sfcr_mlt(multis, fixed)

  return(multis)
}
