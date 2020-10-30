.eq_as_tb <- function(sectors) {
  suppressMessages({
    purrr::map(unlist(sectors), ~paste0(deparse(.x, width.cutoff = 500),collapse = "")) %>%
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

# Eval for random vars
.ev_rand <- function(a, b, c, n) {
  eval(str2expression(paste(a, "<- rnorm(t, mean = ", b, ", sd =", c, ')')),
       envir = parent.frame(n = n))
}

.initiate_random <- function(random, t = t, n = 2) {
  map2(names(random), random, function(.x, .y) {
    .ev_rand(.x, .y[1], .y[2], n = n)
  })
}


.ev <- function(a, b, n) eval(str2expression(paste(a, "<-", b)), envir = parent.frame(n = n))


.final_tb <- function(vars, params) {
  final_tb <- dplyr::bind_cols(vars, params)
  final_tb['t'] <- 1:nrow(final_tb)
  dplyr::select(final_tb, c(t, dplyr::everything()))

  #dplyr::bind_cols(vars, params) %>%
  #  dplyr::mutate(t = dplyr::row_number()) %>%
  #  dplyr::select(t, dplyr::everything())
}

# Add time stamps to the variables
# .add_time_stamps <- function(eq_as_tb, pat1, pat2, pat3) {
#   eq_as_tb %>%
#     dplyr::mutate(lhs = gsub(pat1, "\\1\\[t\\]", lhs, perl= T)) %>%
#     dplyr::mutate(rhs = gsub(pat1, "\\1\\[t\\]", rhs, perl= T)) %>%
#     dplyr::mutate(rhs = gsub(pat2, "\\1\\[t\\]\\)", rhs, perl = T)) %>%
#     dplyr::mutate(rhs = gsub(pat3, "\\1\\[t\\]\\/", rhs, perl = T)) %>%
#     dplyr::mutate(rhs = gsub("\\[-1\\]", "\\[t-1\\]", rhs, perl = T))
# }

.add_time_stamps <- function(eq_as_tb, pend, pexg) {
  eq_as_tb %>%
    dplyr::mutate(lhs = gsub(pend, "\\1\\[t\\]", lhs, perl= T),
                  rhs = gsub(pend, "\\1\\[t\\]", rhs, perl = T),
                  rhs = gsub(pexg, "\\1\\[t\\]", rhs, perl = T),
                  rhs = gsub("\\[-1\\]", "\\[t-1\\]", rhs))
}


# Find dependencies and order the equations
.find_dependencies <- function(tb_eqs, pattern) {
  tb_eqs %>%
    dplyr::mutate(lhs = forcats::fct_inorder(lhs)) %>%
    dplyr::nest_by(lhs) %>%
    dplyr::mutate(depends = stringr::str_extract_all(data, paste0("(",paste0(pattern, collapse = "|"),")"))) %>%
    dplyr::select(-data) %>%
    dplyr::mutate(depends = purrr::simplify_all(list(purrr::map(depends, ~gsub("\\[t\\]|\\[t-1\\]", "", .x))))) %>%
    dplyr::mutate(depends = list(unique(depends))) %>%
    dplyr::mutate(l = length(depends)) %>%
    dplyr::mutate(lhs = as.character.factor(lhs)) %>%
    dplyr::mutate(lhs = gsub("\\[t\\]", "", lhs)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n = dplyr::row_number())
}

.midsteps <- function(m, pat) {m %>%
    dplyr::rowwise() %>%
    dplyr::mutate(depends = purrr::simplify_all(list(purrr::map(depends, function(.x) .x[!grepl(pat, .x, perl = T)])))) %>%
    dplyr::mutate(l = length(depends))}

.sfcr_order_eqs <- function(equations, exogenous, parameters) {
  eqs <- .eq_as_tb(equations)

  #collapsed_names <- paste0(c(names(exogenous), names(parameters), eqs$lhs), collapse = "|")

  # pat1 <- paste0("(", collapsed_names, ")($|[[:space:]])")
  # pat2 <- paste0("(", collapsed_names, ")(\\))")
  # pat3 <- paste0("(", collapsed_names, ")(\\/)")

  pend <- paste0("(?<![[:alnum:]])(",paste0(eqs$lhs, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|_)")
  pexg <- paste0("(?<![[:alnum:]])(",paste0(c(names(exogenous), names(parameters)), collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|_)")

  # eqs <- eqs %>% .add_time_stamps(pat1, pat2, pat3)
  eqs <- eqs %>% .add_time_stamps(pend, pexg)

  # Re arrange equations for a optimal estimation

  # 1. Get a pattern to match out the endogenous variables on the right-hand side of the equations
  m2 <- eqs %>% dplyr::mutate(lhs = gsub("\\[t\\]", "\\\\[t\\\\]", lhs)) %>% {.[,]$lhs}

  # 2. LookUp tibble that will be looked upon to find an optimal order:
  look_up_tb <- .find_dependencies(eqs, m2)


  # In this loop, the code check the equations first to see all that depends only on lagged values
  # Or exogenous variables. It places these variables in the beginning of the `block` vector.
  # It then searches for the variables that depend on the variables that were already identified.
  # And does it successively until it finds all the variables.

  block <- NULL
  pat <- NULL
  for (i in seq_along(look_up_tb$lhs)) {
    if (is.null(pat)) {
      block <- look_up_tb[, "n"][look_up_tb[, 'l'] == 0]

    } else {
      new_block <- look_up_tb[-block,] %>% .midsteps(pat) %>% {.[, "n"][.[, "l"] == 0]}
      block <- c(block, new_block)
    }

    pat <- paste0("^(", paste0(look_up_tb[block, 'lhs']$lhs, collapse = "|"), ")$")

    if (length(look_up_tb$lhs) == length(block)) {break}
  }

  eqs <- eqs[block,]
  return(eqs)
}


# GaussSeidel algorithm

#' @importFrom rlang :=
#'
.gen_steady_internal <- function(equations, t = 100, exogenous, parameters, initial = NULL, random = NULL, max_iter = 100) {

  # Get equations as a tibble
  #eqs <- .eq_as_tb(equations) %>%
  #  mutate(across(c(lhs, rhs), ~.mod_str(.x)))

  # Left-hand side
  lhs_eqs <- list(equations$lhs)

  # Right-hand side
  rhs_eqs <- list(equations$rhs)

  # Get names of endogenous variables
  lhs_names <- .lhs_names(lhs_eqs)

  # Initiate variables numerically --

  # Endogenous vars
  .initiate_vals(lhs_names$name, lhs_names$value, t = t)

  # If initial values are specified:
  if (!is.null(initial)) {
    .initiate_vals(names(initial), initial, t = t, from_start = T)
  }

  if (!is.null(random)) {
    .initiate_random(random, n = 4)
  }

  # Exogenous variables
  .initiate_vals(names(exogenous), exogenous, t = t)

  # --

  # Initiate parameters
  .initiate_vals(names(parameters), parameters, t = t, from_start = T)

  # Calculate SFC model
  # for (t in 2:t) {
  #   for (iterations in 1:40) {
  #     purrr::map2(lhs_eqs, rhs_eqs, function(x, y) .ev(x, y, 3))
  #   }
  # }


  # tmp vars to check convergence
  purrr::map(seq_along(lhs_eqs[[1]]), function(.x) eval(str2expression(paste0("tmp",.x, "<- 0")),
                                                        parent.frame(n = 2)))


  #start <- Sys.time()
  for (t in 2:t) {
    for (iterations in 1:max_iter) {
      # Calculate the variables
      purrr::map2(lhs_eqs, rhs_eqs, function(.x, .y) .ev(.x, .y, 3))

      # Check
      purrr::map2(1:length(lhs_eqs[[1]]), lhs_eqs[[1]], function(.x, .y) {
        eval(str2expression(paste0('cdt', .x, '<- isTRUE(all.equal.numeric(tmp',.x,',', .y, '))')),
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
  #end <- Sys.time()
  #end - start


  # Endogenous and Exogenous a a list
  vars_names <- c(lhs_names$name, names(exogenous))

  vars_tb <- purrr::map_dfc(vars_names, function(.x) tibble::tibble(!!.x := eval(str2expression(.x))))

  # Tibble with exogenous values
  #params_tb <- dplyr::bind_cols(parameters)

  # Final output
  final_tb <- .final_tb(vars_tb, parameters)

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
#' @param random Named list with the name of the random component as name, and the desired mean
#' and standard deviation as values.
#' @param hidden Named list that identify the two variables that make the hidden equality
#' in the SFC model, e.g., \code{list("H_h" = "H_s")}. Defaults to NULL.
#' If \code{hidden} is supplied, the model will evaluate if the hidden equation is satisfied.
#' If it is not, it will throw out an error.
#' @param max_iter Maximum iterations allowed per period.
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
sfcr_sim <- function(equations, t = 100, exogenous, parameters, random = NULL, initial = NULL, hidden = NULL, max_iter = 100) {
  if (!is.list(exogenous)) stop("`exogenous` must be a list.")

  if (!is.list(parameters)) stop("`parameters` must be a list.")

  if (!is.null(initial) & !is.list(initial)) stop("`initial` must be a list.")

  if (t < 2) stop('Required at least two time periods.')

  if (t > 150) stop('Maximum time periods allowed are 150.')

  equations <- .sfcr_order_eqs(equations, exogenous, parameters)

  look_for_errors <- .collect_warnings(.gen_steady_internal(equations, t = 2, exogenous, parameters, initial, random, max_iter = 1))

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

  x <- .gen_steady_internal(equations, t = t, exogenous, parameters, initial, random, max_iter)

  if (!is.null(hidden)) {
    h1 <- dplyr::pull(x, names(hidden))
    h2 <- dplyr::pull(x, hidden[[1]])

    is_hidden_true <- all.equal(h1, h2, tolerance = 0.01)

    if (!isTRUE(is_hidden_true)) stop("Hidden equation is not fulfilled. Check again the equations in the model.")

  }

  return(x)
}
