#' Make matrix for scenario calculations
#'
#' @param sfcr_sim a model calculated with the \code{sfcr_sim()} function
#' @param scenario a List holding the different scenarios
#' @param periods The total number of periods in the model
#'
#' @details This function generates the base matrix that is going to be
#' modified in place by the \code{sfcr_gauss_seidel()} algorithm.
#'
#' @author João Macalós
#'
.sfcr_make_scenario_matrix <- function(sfcr_sim, scenario, periods) {

  steady <- tail(attributes(sfcr_sim)$matrix, n = 1)

  m <- steady[rep(seq_len(nrow(steady)), periods), ]

  scenario_eqs <- purrr::map(scenario, function(x) .eq_as_tb(x$variables))
  scenario_names <- purrr::map(scenario_eqs, function(x) x$lhs)
  scenario_exprs <- purrr::map(scenario_eqs, function(x) purrr::map(x$rhs, function(y) parse(text = y)))

  scenario_start <- purrr::map(scenario, function(x) x$start)
  scenario_end <- purrr::map(scenario, function(x) x$end)


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
#' @param sfcr_sim A model generated with the \code{sfcr_sim()} function.
#' @param scenario A list containing one or more lists that hold the information
#' about the scenario(s). Each list must contain three information:
#' 1. A list describing the variables that are going to be modified and their new
#' values. This must be written with R equation syntax.
#' 2. An integer defining the period the the scenario starts.
#' 3. An integer defining the period when the scenario ends.
#'
#' See examples for details.
#' @param periods The total periods in the model.
#' @param max_iter The maximum allowed iteration per period.
#'
#' @details Add scenario(s) to a model generated with \code{sfcr_sim()} functions.
#'
#' @seealso \code{\link{sfcr_sim}}
#'
#' @export
#'
sfcr_scenario <- function(sfcr_sim, scenario, periods, max_iter = 350) {
  if (class(sfcr_sim) != "sfcr_tbl") {stop("Please provide a model obtained with `sfcr_sim()` function.")}
  if (rlang::is_null(attributes(sfcr_sim)$matrix)) {stop("Please don't modify the model")}
  if (rlang::is_null(scenario[[1]]$variable)) {stop("Please verify the syntax of the scenarios. Is it a list? The `variables`, `start`, and `end` items were named correctly?")}

  m <- .sfcr_make_scenario_matrix(sfcr_sim, scenario, periods)

  eqs <- attributes(sfcr_sim)$calls

  s1 <- .sfcr_GaussSeidel(m, eqs, periods, max_iter)

  s2 <- tibble::tibble(data.frame(s1)) %>%
    dplyr::mutate(period = dplyr::row_number()) %>%
    dplyr::select(-tidyselect::contains('block')) %>%
    dplyr::select(period, tidyselect::everything())

  attr(s2, "matrix") <- s1
  attr(s2, "calls") <- eqs

  class(s2) <- c("sfcr", "tbl_df", "tbl", "data.frame")

  return(s2)
}
