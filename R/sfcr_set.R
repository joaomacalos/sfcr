#' sfcr_set constructor
#
#' @param list A list
#'
#' @author João Macalós
#'
#' @keywords internal
#'
new_sfcr_set <- function(list) {
  stopifnot(inherits(list, "list"))

  if (mean(vapply(list, rlang::is_formula, logical(1))) < 1) rlang::abort("Invalid arguments. Please use the R formula syntax (`~` instead of `=`) to separate the equations.")

  structure(list,
            class = c("sfcr_set", "list"))

}



#' Define the formulas of the model
#'
#' The \code{sfcr_set()} function is used to create the lists of equations,
#' external variables, initial values, and also to modify the variables inside
#' the \code{sfcr_shock()} function.
#'
#'
#' @details
#'
#' This function is a S3 generic that applicable to only two inputs: \code{formula} and
#' \code{sfcr_set}. It is used to create a new set of equations or to modify an existing
#' one.
#'
#' Therefore, the equations must be written using the R formula syntax, i.e., the left-hand
#' side of each equation is separated from the right-hand side with a \code{~} ("twiddle")
#' instead of a \code{=}.
#'
#' Furthermore, the \code{sfcr_set()} function recognizes two symbols that are not
#' native to R language: \code{[-1]}, and \code{d()}.
#'
#' * If a variable defined with \code{sfcr_set()} is followed by \code{[-1]}, it will
#'   be recognized as a lagged variable.
#'
#' * If a variable is defined inside \code{d()}, the \code{sfcr} engines will transform
#' them into a first difference equation. For example, \code{d(Hh)} is internally transformed
#' into \code{(Hh - Hh[-1])}.
#'
#' Random variables can be created using the \code{sfcr_random()} function. See
#' \code{\link{sfcr_random}} for further details.
#'
#'
#' @param ... The formulas used to define the equations and external
#' values of the system
#' @param exclude One or more indices of equations to be excluded. The
#' correct indices can be found with \code{sfcr_set_index()}.
#'
#' @author João Macalós
#'
#' @examples
#' # Endogenous set
#' equations <- sfcr_set(
#'   TXs ~ TXd,
#'   YD ~ W * Ns - TXs,
#'   Cd ~ alpha1 * YD + alpha2 * Hh[-1],
#'   Hh ~ YD - Cd + Hh[-1],
#'   Ns ~ Nd,
#'   Nd ~ Y / W,
#'   Cs ~ Cd,
#'   Gs ~ Gd,
#'   Y ~ Cs + Gs,
#'   TXd ~ theta * W * Ns,
#'   Hs ~ Gd - TXd + Hs[-1]
#'   )
#'
#' # Exogenous set
#' exogenous <- sfcr_set(alpha1 ~ 0.8, alpha2 ~ 0.15)
#'
#' # Modify an existing set
#' equations2 <- sfcr_set(equations, Hh ~ Hh[-1] + d(Hs), exclude = 4)
#'
#' # Add normal random variable
#' sfcr_set(Ra ~ sfcr_random("rnorm", mean=10, sd=2))
#'
#' @export
#'
sfcr_set <- function(..., exclude = NULL) {
  UseMethod("sfcr_set")
}

#' @method sfcr_set formula
#'
#' @author João Macalós
#'
#' @export
#'
sfcr_set.formula <- function(..., exclude = NULL) {

  formulas <- rlang::list2(...)

  formulas <- new_sfcr_set(formulas)

  formulas[exclude] <- NULL

  return(formulas)
}

#' @method sfcr_set sfcr_set
#'
#' @author João Macalós
#'
#' @export
#'
sfcr_set.sfcr_set <- function(..., exclude = NULL) {
  formulas <- c(...)

  formulas <- new_sfcr_set(formulas)

  formulas[exclude] <- NULL

  return(formulas)
}



#' Get names of endogenous vars and their index
#'
#' The \code{sfcr_set_index()} function takes a list of equations as its input and returns
#' a tibble containing the name of the variable on the left-hand side of the equations
#' and their position in the equations list.
#'
#' This function aims to facilitate locating a specific equation in the list in order to
#' modify the list of equations.
#'
#' To add random variation to endogenous variables, use \code{sfcr_random()}.
#'
#' @param eqs A list of equations created with \code{sfcr_set()}
#'
#' @export
#'
#' @author João Macalós
#'
sfcr_set_index <- function(eqs) {

  abortifnot(inherits(eqs, "sfcr_set"), "Please supply a list of equations created with `sfcr_set()`.")

  purrr::map_df(eqs, ~ tibble::enframe(name = "id", deparse(.x, width.cutoff = 500))) %>%
    dplyr::mutate(id = 1:length(eqs)) %>%
    tidyr::separate(.data$value, into = c("lhs", "rhs"), " ~ ")
}
