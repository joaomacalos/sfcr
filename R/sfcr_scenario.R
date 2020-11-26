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

#' Add scenarios to a \code{sfcr} model.
#'
#' @param baseline A model generated with the \code{sfcr_baseline()} function.
#' @param scenario A list containing one or more shocks created with
#' \code{sfcr_shock()} function.
#'
#' @param periods The total periods in the model.
#' @param max_iter The maximum allowed iteration per period.
#'
#' @inheritParams sfcr_baseline
#'
#' @details Add scenario(s) to a model generated with \code{sfcr_baseline()} functions.
#'
#' This function inherits the block structure from the steady state model
#' and also uses the Gauss Seidel algorithm.
#'
#' @seealso \code{\link{sfcr_baseline}}
#'
#' @example inst/examples/example_sfcr_scenario.R
#'
#' @export
#'
#' @author João Macalós, \email{joaomacalos@@gmail.com}
#'
sfcr_scenario <- function(baseline, scenario, periods, max_iter = 350, tol = 1e-10) {

  if (inherits(scenario, "sfcr_shock")) {
    scenario <- list(scenario)
  }

  #if (isFALSE(rlang::is_bare_list(scenario))) {stop ("Please surround the shocks with a list.")}

  if (isTRUE(class(scenario[[1]]) != "sfcr_shock")) {stop ("Please use `sfcr_shock()` to create shocks.")}

  m <- .sfcr_make_scenario_matrix(baseline, scenario, periods)

  eqs <- attr(baseline, "calls")

  s1 <- .sfcr_gauss_seidel(m, eqs, periods, max_iter, tol)

  s2 <- tibble::tibble(data.frame(s1)) %>%
    dplyr::mutate(period = dplyr::row_number()) %>%
    dplyr::select(-tidyselect::contains('block')) %>%
    dplyr::select(.data$period, tidyselect::everything())
    #dplyr::mutate(dplyr::across(-c(.data$period), ~round(.x, digits = 4)))

  #attr(s2, "matrix") <- s1
  #attr(s2, "calls") <- eqs

  #class(s2) <- c("sfcr", "tbl_df", "tbl", "data.frame")

  ext <- attr(baseline, "external")

  x <- new_sfcr_tbl(s2, s1, eqs, ext)

  return(x)
}
