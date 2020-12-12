#' Expand variables to implement sensitivity analysis
#'
#' The \code{sfcr_expand()} function is a s3 **generic** that takes
#' a list of external objects and returns a expanded set of these lists.
#' It has methodsf or \code{sfcr_set} objects and \code{sfcr_shock} objects.
#'
#' @param x A external set created with \code{sfcr_set()} or
#' a shock set created with \code{sfcr_shock()}
#' @param variable the name of variable to be expanded.
#' @param values a vector containing the new values of the external or
#' shock variable.
#'
#' @details
#'
#' There are two available methods for the \code{sfcr_expand()} function:
#' * \code{sfcr_set}:
#'  Takes a \code{sfcr_set} object with **external** variables and creates
#'  a list of sets that inherits all the aspects of the \code{x} set supplied
#'  but set the values of the \code{variable} to the each element of \code{value}.
#'  The output is a \code{sfcr_mlt_set} object.
#'
#' * \code{sfcr_shock}:
#'  Takes a \code{sfcr_shock} object and creates a list of shocks that inherits
#'  all the aspects of the \code{x} shock but set the \code{values} of the
#'  \code{variable} to each element of \code{value}. The output of this
#'  method is a \code{sfcr_mlt_shock} object.
#'
#'
#' @examples
#' # 1. Expand a external set:
#' external <- sfcr_set(G_d ~ 20, W ~ 1, alpha1 ~ 0.6, alpha2 ~ 0.4, theta ~ 0.2)
#' sfcr_expand(external, alpha2, c(0.1, 0.2))
#'
#' # 2. Expand a shock:
#' shock <- sfcr_shock(variables = sfcr_set(alpha1 ~ 0.8), start = 5, end = 50)
#' sfcr_expand(shock, alpha1, c(0.7, 0.8, 0.9))
#'
#' @author João Macalós
#'
#' @export
sfcr_expand <- function(x, variable, values) {
  UseMethod("sfcr_expand")
}

#' sfcr_mlt_set constructor
#'
#' @param x A expanded list of sfcr_set's
#' @param original the original sfcr_set
#'
new_sfcr_mlt_set <- function(x, original) {
  stopifnot(inherits(x, "list"))
  stopifnot(inherits(original, "sfcr_set"))

  structure(x,
            class = c("sfcr_mlt_set", "list"),
            original = original)
}


#' @method sfcr_expand sfcr_set
#'
#' @export
#'
sfcr_expand.sfcr_set <- function(x, variable, values) {

  abortifnot(is.numeric(values), "Please supply a numeric vector as values.")
  tryCatch(
    error = function(cnd) {
      rlang::abort("Please supply a valid variable name.")
    },
    variable <- rlang::as_string(rlang::enexpr(variable))
  )

  abortifnot(variable %in% .eq_as_tb(x)$lhs, "Please supply a valid variable name that is present in the external set.")


  # Remove name from external
  new <- purrr::map_lgl(x, ~!stringr::str_detect(deparse(.x, width.cutoff = 500L), variable))
  modify <- x[new]

  # Make formula for x
  expr <- paste0(variable, " ~ ", as.character(values))
  expr <- purrr::map(expr, ~as.formula(.x))

  # Replicate external
  exp_external <- rep(list(modify), length(values))
  exp_external <- purrr::map(seq_along(values), ~c(exp_external[[.x]], expr[[.x]]))

  exp_external <- new_sfcr_mlt_set(exp_external, x)

  return(exp_external)
}


#' sfcr_mlt_shock constructor
#'
#' @param x A expanded list of sfcr_shock's
#' @param original the original sfcr_shock
#'
new_sfcr_mlt_shock <- function(x, original) {
  stopifnot(inherits(x, "list"))
  stopifnot(inherits(original, "sfcr_shock"))

  structure(x,
            class = c("sfcr_mlt_shock", "list"),
            original = original)
}


#' @method sfcr_expand sfcr_shock
#'
#' @export
#'
sfcr_expand.sfcr_shock <- function(x, variable, values) {

  abortifnot(is.numeric(values), "Please supply a numeric vector as values.")
  tryCatch(
    error = function(cnd) {
      rlang::abort("Please supply a valid variable name.")
    },
    variable <- rlang::as_string(rlang::enexpr(variable))
  )

  abortifnot(variable %in% .eq_as_tb(x$variables)$lhs, "Please supply a valid variable name that is present in the external set.")

  # Remove name from external
  new <- purrr::map_lgl(x$variables, ~!stringr::str_detect(deparse(.x, width.cutoff = 500L), variable))
  x$variables <- x$variables[new]

  # Make formula for x
  expr <- paste0(variable, " ~ ", as.character(values))
  expr <- purrr::map(expr, ~as.formula(.x))

  # Replicate external
  exp_shock <- rep(list(x), length(values))
  exp_variables <- purrr::map(seq_along(values), ~c(exp_shock$variables[[.x]], expr[[.x]]))

  for (i in seq_along(exp_shock)) {
    exp_shock[[i]]$variables <- exp_variables[[i]]
  }

  exp_shock <- new_sfcr_mlt_shock(x = exp_shock, original = x)

  return(exp_shock)
}
