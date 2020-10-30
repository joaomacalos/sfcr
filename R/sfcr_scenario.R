#' Add shocks to SFC models
#'
#' The \code{sfcr_scenario} function is used to add a shock to a SFC model
#' generated with the \code{\link{sfcr_sim}} function.
#'
#' @param steady_state A simulated SFC model with \code{\link{sfcr_sim}} function to provide the steady
#' state values of the variables.
#' @param exogenous,parameters Named lists with the initial values of the exogenous and parameters.
#' This should be exactly the same values as supplied to \code{\link{sfcr_sim}}.
#' @param shock_exg,shock_param Named lists with the new values for the exogenous and/or parameters
#' that are being shocked to calculate a different scenario.
#' @param one_time_shock Named list containing the name, the value, and the period in which the one
#' time shock must take place. Currently works only with exogenous variables.
#'
#' @inheritParams sfcr_sim
#'
#' @details The output of  \code{\link{sfcr_sim}} will contain a tibble with the exogenous and endogenous
#' variables, as well as the parameters, of the simulated model as columns.
#'
#' The equations at the `equations` argument should take the usual R formula syntax.
#' Also, endogenous and exogenous variables should explicitly indicate if they're
#' to be considered contemporaneously or with lags, while parameters in should not
#' be accompanied by period of evaluation indication.
#'
#' On the other hand, the named lists should not include time indices.
#'
#' See \code{\link{sfcr_sim}} for further information.
#'
#' @example inst/examples/example_sfcr_scenario.R
#'
#' @importFrom rlang :=
#'
#' @export
#'
sfcr_scenario <- function(steady_state, equations, t = 100, exogenous, parameters, random = NULL, shock_exg = NULL, shock_param = NULL, one_time_shock = NULL, max_iter = 100) {

  # Get equations as a tibble
  eqs <- .eq_as_tb(equations)

  # Left-hand side
  lhs_eqs <- list(eqs$lhs)

  # Right-hand side
  rhs_eqs <- list(eqs$rhs)

  # Get names of endogenous variables
  lhs_names <- .lhs_names(lhs_eqs)

  # Load steady state values
  steady_vals <- steady_state %>%
    dplyr::slice_tail(n = 1)

  # Steady state values for endogenous variables
  steady_endogenous <- steady_vals %>%
    dplyr::select(lhs_names$name)

  if (!is.null(random)) {
    .ev_rand(names(random), random, t = t, n = 1)
  }


  # Initiate endogenous variables numerically
  .initiate_vals(names(steady_endogenous), steady_endogenous, t = t, from_start = T)

  # Initiate exogenous variables
  .initiate_vals(names(exogenous), exogenous, t = t, from_start = T)

  # Add shock to exogenous variables
  if (!is.null(shock_exg)) {
    purrr::map2(names(shock_exg), shock_exg, function(.x, .y) {
      eval(str2expression(paste(.x, "[6:t] <- ", .y)),
           envir = parent.frame(n = 2))
    })
  }

  if (!is.null(one_time_shock)) {
    purrr::map2(names(one_time_shock), one_time_shock, function(.x, .y) {
      eval(str2expression(paste(.x, "[", .y[2], "] <- ", .y[1])),
           envir = parent.frame(n = 2))
    })
  }


  # Define parameter values
  purrr::map2(names(parameters), parameters, function(.x, .y) {
    .ev(.x, .y, 3)
  })


  # Calculate SFC model


  # for (t in 2:t) {
  #  if (t <= 5) {
  #    for (iterations in 1:40) {
  #      purrr::map2(lhs_eqs, rhs_eqs, function(x, y) .ev(x, y, 3))
  #      }
  # }
  #    else {
  #      if (!is.null(shock_param)) {
  #                 purrr::map2(names(shock_param), shock_param, function(.x, .y) {
  #                   .ev(.x, .y, 3)
  #                 })
  #      }
  #      for (iterations in 1:40) {
  #      purrr::map2(lhs_eqs, rhs_eqs, function(x, y) .ev(x, y, 3))
  #      }
  #    }
  # }

  for (t in 2:t) {
    purrr::map(1:length(lhs_eqs[[1]]), function(.x) eval(str2expression(paste0("tmp",.x, "<- rep(0, t)")),
                                                         parent.frame(n = 2)))

    if (t <= 5) {
      for (iterations in 1:10) {
        purrr::map2(lhs_eqs, rhs_eqs, function(x, y) .ev(x, y, 3))
      }
    }

    else {
      if (!is.null(shock_param)) {
        purrr::map2(names(shock_param), shock_param, function(.x, .y) {
          .ev(.x, .y, 3)
        })
      }
      for (iterations in 1:max_iter) {
        purrr::map2(lhs_eqs, rhs_eqs, function(.x, .y) .ev(.x, .y, 3))

        purrr::map2(1:length(lhs_eqs[[1]]), lhs_eqs[[1]], function(.x, .y) {
          eval(str2expression(paste0('cdt', .x, '<- isTRUE(all.equal(tmp',.x,',', .y, '))')),
               parent.frame(n = 2))})


        text_to_eval <- paste0("cdt",1:length(lhs_eqs[[1]]),collapse=",")
        mean_cdt <- eval(str2expression(paste0('mean(c(',text_to_eval,'))')))

        if (mean_cdt == 1) {break} else {
          purrr::map2(1:length(lhs_eqs[[1]]), lhs_eqs[[1]], function(.x, .y) {
            eval(str2expression(paste0('tmp',.x, '<-', .y)),
                 parent.frame(n = 2))
          }
          )}
      }

    }
  }




  # # Tibble with params
  params_tb <- tibble::tibble(t = 1:t, dplyr::bind_cols(parameters))

  if (!is.null(shock_param)) {
     shock_params_tb <- purrr::map2(names(shock_param), shock_param, function(.x, .y)
       params_tb %>%
         dplyr::select(t, tidyselect::matches(.x)) %>%
         dplyr::mutate(!!.x := dplyr::case_when(t < 6 ~ eval(str2expression(.x)),
                                  T ~ .y))) %>%
       purrr::reduce(dplyr::left_join, by = "t")

     params_tb <- dplyr::bind_cols(shock_params_tb, dplyr::select(params_tb, -colnames(shock_params_tb))) %>%
       dplyr::select(-t)

  }

  # Endogenous and Exogenous a a list
  vars_names <- c(lhs_names$name, names(exogenous))

  vars_tb <- purrr::map_dfc(vars_names, function(.x) tibble::tibble(!!.x := eval(str2expression(.x))))

  # Final output
  final_tb <- .final_tb(vars_tb, params_tb)

  return(final_tb)

}
