#' Create shock(s) to add to a \code{sfcr_scenario()}.
#'
#' @param variables A \code{sfcr_set()} with formula(e) containing the name of the variable(s)
#' that will be shocked on the left-hand side and their new values on the right-
#' hand side.
#'
#' It is possible to add exogenous series a shock instead of constant variables.
#' However, the length of such series must be exactly the same as the period of the shock (i.e.,
#' the difference between start and end).
#'
#' @param start An integer indicating the period when the shock takes place.
#' @param end An integer indicating the period when the shock ends.
#'
#' @examples
#'
#' sfcr_shock(
#'  variables = sfcr_set(G_d ~ 30, W ~ 1.5),
#'  start = 5,
#'  end = 66)
#'
#' sfcr_shock(
#'  variables = sfcr_set(G_d ~ seq(30, 40, length.out=62)),
#'  start = 5,
#'  end = 66)
#'
#' @export
#'
#' @author João Macalós, \email{joaomacalos@@gmail.com}
#
sfcr_shock <- function(variables, start, end) {
  if (!rlang::is_list(variables)) rlang::abort("Please define the variables in a `sfcr_shock()`.")

  if (all(vapply(variables, rlang::is_formula, logical(1)) != 1)) rlang::abort("Please use R equations syntax to define the values of the variables.")

  structure(
    list(
      variables = variables,
      start = start,
      end = end
    ),
    class = c("sfcr_shock", "list")
  )
}
