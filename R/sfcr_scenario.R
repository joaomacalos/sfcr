#' Make matrix for scenario calculations
#'
#' @param sfcr_sim a model calculated with the \code{sfcr_sim()} function
#' @param scenario a List holding the different scenarios
#' @param periods The total number of periods in the model
#'
#' @details This function generates the base matrix that is going to be
#' modified in place by the \code{sfcr_gauss_seidel()} algorithm.
#'
#'
#' @author João Macalós
#'
.sfcr_make_scenario_matrix <- function(sfcr_sim, scenario, periods) {

  steady <- utils::tail(attributes(sfcr_sim)$matrix, n = 1)

  m <- steady[rep(seq_len(nrow(steady)), periods), ]

  scenario_eqs <- purrr::map(scenario, function(x) .eq_as_tb(x[[1]]))

  scenario_names <- purrr::map(scenario_eqs, function(x) x$lhs)
  scenario_exprs <- purrr::map(scenario_eqs, function(x) purrr::map(x$rhs, function(y) parse(text = y)))

  scenario_start <- purrr::map(scenario, function(x) x[[2]])
  scenario_end <- purrr::map(scenario, function(x) x[[3]])


  for (scenario in seq_len(vctrs::vec_size(scenario_eqs))) {
    scenario_nms <- scenario_names[[scenario]]
    scenario_xprs <- scenario_exprs[[scenario]]

    for (var in seq_along(scenario_nms)) {
      m[scenario_start[[scenario]]:scenario_end[[scenario]], scenario_nms[[var]]] <- eval(scenario_xprs[[var]])
    }
  }


  return(m)
}

#' Create schock(s) to add to a \code{sfcr_scenario()}.
#'
#' @param variables A list with formula(e) containing the name of the variable(s)
#' that will be shocked on the left-hand side and their new values on the right-
#' hand side.
#' @param start An integer indicating the period when the shock takes place.
#' @param end An integer indicating the period when the shock ends.
#'
#' @examples
#'
#' sfcr_shock(
#'  variables = list(G_d ~ 30, W ~ 1.5),
#'  start = 5,
#'  end = 66)
#'
#' @export
#'
#' @author João Macalós, \email{joaomacalos@@gmail.com}
#
sfcr_shock <- function(variables, start, end) {
  structure(
    list(
    variables = variables,
    start = start,
    end = end
  ),
  class = c("sfcr_shock", "list")
  )
}

#' Add scenarios to a \code{sfcr} model.
#'
#' @param sfcr_sim A model generated with the \code{sfcr_sim()} function.
#' @param scenario A list containing one or more shocks created with
#' \code{sfcr_shock()} function.
#'
#' @param periods The total periods in the model.
#' @param max_iter The maximum allowed iteration per period.
#'
#' @details Add scenario(s) to a model generated with \code{sfcr_sim()} functions.
#'
#' This function inherits the block structure from the steady state model
#' and also uses the Gauss Seidel algorithm.
#'
#' @seealso \code{\link{sfcr_sim}}
#'
#' @example inst/examples/example_sfcr_scenario.R
#'
#' @export
#'
#' @author João Macalós, \email{joaomacalos@@gmail.com}
#'
sfcr_scenario <- function(sfcr_sim, scenario, periods, max_iter = 350) {

  if (isFALSE(rlang::is_bare_list(scenario))) {stop ("Please surround the shocks with a list.")}

  if (isTRUE(class(scenario[[1]]) != "sfcr_shock")) {stop ("Please use `sfcr_shock()` to create shocks.")}

  m <- .sfcr_make_scenario_matrix(sfcr_sim, scenario, periods)

  eqs <- attributes(sfcr_sim)$calls

  s1 <- .sfcr_gauss_seidel(m, eqs, periods, max_iter)

  s2 <- tibble::tibble(data.frame(s1)) %>%
    dplyr::mutate(period = dplyr::row_number()) %>%
    dplyr::select(-tidyselect::contains('block')) %>%
    dplyr::select(.data$period, tidyselect::everything()) %>%
    dplyr::mutate(dplyr::across(-c(.data$period), ~round(.x, digits = 4)))

  attr(s2, "matrix") <- s1
  attr(s2, "calls") <- eqs

  class(s2) <- c("sfcr", "tbl_df", "tbl", "data.frame")

  return(s2)
}
