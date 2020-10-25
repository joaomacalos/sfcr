.eq_as_tb <- function(sectors) {
  suppressMessages({
    purrr::map(unlist(sectors), ~ deparse(.x, width.cutoff = 500)) %>%
      dplyr::bind_cols(.name_repair = "universal") %>%
      t() %>%
      tibble::as_tibble(.name_repair = "universal") %>%
      tidyr::separate("...1", c("lhs", "rhs"), sep = " ~ ")
  })
}


.lhs_names <- function(lhs_eqs) {
  name <- NULL
  value <- NULL
  unlist(lhs_eqs) %>%
    tibble::enframe(name = "name", value = "value") %>%
    dplyr::mutate(value = gsub("\\[t\\]", "", value)) %>%
    dplyr::select(-name) %>%
    dplyr::mutate(v = 1) %>%
    purrr::set_names("name", "value")
}

# Eval for initial values
.ev_iv <- function(a, b, n, from_start = F) {
  if (isTRUE(from_start)) {
    eval(str2expression(paste(a, "<- rep(", b, ", t)")),
      envir = parent.frame(n = n)
    )
  } else {
    eval(str2expression(paste(a, "<- c(1, rep(", b, ", t-1))")),
      envir = parent.frame(n = n)
    )
  }
}


.initiate_vals <- function(names, values, t = t, n = 4, from_start = F) {
  purrr::map2(names, values, function(.x, .y) {
    .ev_iv(.x, .y, n, from_start)
  })
}


.ev <- function(a, b, n) eval(str2expression(paste(a, "<-", b)), envir = parent.frame(n = n))


.final_tb <- function(vars, params) {
  dplyr::bind_cols(vars, params) %>%
    dplyr::mutate(t = dplyr::row_number()) %>%
    dplyr::select(t, dplyr::everything())
}

#' @importFrom rlang :=
#'
.gen_steady_internal <- function(equations, t = 100, exogenous, parameters, initial = NULL) {

  # Get equations as a tibble
  eqs <- .eq_as_tb(equations)

  # Left-hand side
  lhs_eqs <- list(eqs$lhs)

  # Right-hand side
  rhs_eqs <- list(eqs$rhs)

  # Get names of endogenous variables
  lhs_names <- .lhs_names(lhs_eqs)

  # Initiate variables numerically --

  # Endogenous vars
  .initiate_vals(lhs_names$name, lhs_names$value, t = t)

  # If initial values are specified:
  if (!is.null(initial)) {
    .initiate_vals(names(initial), initial, t = t, from_start = T)
  }

  # Exogenous variables
  .initiate_vals(names(exogenous), exogenous, t = t)

  # --

  # Assign parameter values
  purrr::map2(names(parameters), parameters, function(.x, .y) {
    .ev(.x, .y, 3)
  })

  # Calculate SFC model
  for (t in 2:t) {
    for (iterations in 1:40) {
      purrr::map2(lhs_eqs, rhs_eqs, function(x, y) .ev(x, y, 3))
    }
  }

  # Endogenous and Exogenous a a list
  vars_names <- c(lhs_names$name, names(exogenous))

  vars_tb <- purrr::map_dfc(vars_names, function(.x) tibble::tibble(!!.x := eval(str2expression(.x))))

  # Tibble with exogenous values
  params_tb <- dplyr::bind_cols(parameters)

  # Final output
  final_tb <- .final_tb(vars_tb, params_tb)

  return(final_tb)
}


# Collect warnings function
.collect_warnings <- catchr::make_catch_fn(warning = c(collect, muffle))


#' Simulate a stock-flow consistent model
#'
#' The \code{sfcr_sim} function is used to simulate a SFC model. With adequate number of
#' periods, the simulated model should converge to a steady state scenario.
#'
#' @param equations A list containing all the equations of the model to be simulated.
#' See details.
#' @param t A number specifying the total number of periods of the model to be simulated. It should be at least 2 periods.
#' @param exogenous,parameters,initial Named lists with exogenous, parameters, and initial values
#' of endogenous variables as values. See details.
#' @param hidden Named list that identify the two variables that make the hidden equality
#' in the SFC model, e.g., \code{list("H_h" = "H_s")}. Defaults to NULL.
#' If \code{hidden} is supplied, the model will evaluate if the hidden equation is satisfied.
#' If it is not, it will throw out an error.
#'
#' @details The output of  \code{sfcr_sim()} will contain a tibble with the exogenous and endogenous
#' variables, as well as the parameters, of the simulated model as columns.
#'
#' The equations at the `equations` argument should take the usual R formula syntax.
#' Also, endogenous and exogenous variables should explicitly indicate if they're
#' to be considered contemporaneously or with lags, while parameters in should not
#' be accompanied by period of evaluation indication. For example, a consumption
#' equation should be written like C_d\[t\] ~ alpha1 * YD\[t\] + alpha2 * H_h\[t-1\].
#'
#'
#' The named lists should not include time indications. See examples.
#'
#'
#' @example inst/examples/example_sfcr_sim.R
#'
#' @export
#'
sfcr_sim <- function(equations, t = 100, exogenous, parameters, initial = NULL, hidden = NULL) {
  if (!is.list(exogenous)) stop("`exogenous` must be a list.")

  if (!is.list(parameters)) stop("`parameters` must be a list.")

  if (!is.null(initial) & !is.list(initial)) stop("`initial` must be a list.")

  if (t < 2) stop('Required at least two time periods.')

  if (t > 150) stop('Maximum time periods allowed are 150.')

  look_for_errors <- .collect_warnings(.gen_steady_internal(equations, t = 2, exogenous, parameters, initial))

  if (rlang::is_empty(look_for_errors$warning) == F) {
    {
      name <- NULL
      value <- NULL

      found_errors <- look_for_errors$warning %>%
        unlist() %>%
        tibble::enframe(name = "name", value = "value") %>%
        dplyr::filter(name == "call") %>%
        dplyr::mutate(value = rlang::as_label(value)) %>%
        dplyr::distinct()

      rlang::abort("Errors in the equations' syntax. Run `rlang::last_error()$x` to find the problematic equations.\n\n\nTip: problems are usually missing time indication (e.g. [t]) in some endogenous/exogenous variable.", x = found_errors)
    }
    return(x)
  }

  x <- .gen_steady_internal(equations, t = t, exogenous, parameters, initial)

  if (!is.null(hidden)) {
    h1 <- dplyr::pull(x, names(hidden))
    h2 <- dplyr::pull(x, hidden[[1]])

    is_hidden_true <- all.equal(h1, h2, tolerance = 0.01)

    if (!isTRUE(is_hidden_true)) stop("Hidden equation is not fulfilled. Check again the equations in the model.")

  }

  return(x)
}
