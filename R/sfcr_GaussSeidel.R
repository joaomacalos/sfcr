#exogenous <- .eq_as_tb(exogenous)
#parameters <- .eq_as_tb(parameters)

.prep_equations <- function(ordered_eqs, external) {

  pend <- paste0("(?<![[:alnum:]])(", paste0(ordered_eqs$lhs, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")
  pendlag <- paste0("(?<![[:alnum:]])(", paste0(ordered_eqs$lhs, collapse = "|"), ")(?=___)")
  pexg <- paste0("(?<![[:alnum:]])(", paste0(c(external$lhs), collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")
  pexglag <- paste0("(?<![[:alnum:]])(", paste0(c(external$lhs), collapse = "|"), ")(?=___)")

  x <- ordered_eqs %>%
   dplyr::mutate(rhs = gsub(pend, "m\\[i,'\\1'\\]", rhs, perl = T),
                 rhs = gsub(pendlag, "m\\[i-1,'\\1'\\]", rhs, perl = T),
                 rhs = gsub(pexg, "m\\[i,'\\1'\\]", rhs, perl = T),
                 rhs = gsub(pexglag, "m\\[i-1,'\\1'\\]", rhs, perl = T),
                 rhs = gsub("___", "", rhs),
                 id = dplyr::row_number())

  # Uncomment to loop on a list instead of a matrix
  # x <- ordered_eqs %>%
  #   dplyr::mutate(rhs = gsub(pend, "m\\[\\['\\1'\\]\\]\\[\\[i\\]\\]", rhs, perl = T),
  #                 rhs = gsub(pendlag, "m\\[\\['\\1'\\]\\]\\[\\[i-1\\]\\]", rhs, perl = T),
  #                 rhs = gsub(pexg, "m\\[\\['\\1'\\]\\]\\[\\[i\\]\\]", rhs, perl = T),
  #                 rhs = gsub(pexglag, "m\\[\\['\\1'\\]\\]\\[\\[i-1\\]\\]", rhs, perl = T),
  #                 rhs = gsub("___", "", rhs),
  #                 id = dplyr::row_number())

  return(x)
}


.make_matrix <- function(equations, external, t, initial) {

  # If no initial values supplied, start with a 1
  ends <- rep(1, vctrs::vec_size(equations$lhs))
  names(ends) <- equations$lhs

  # Exogenous variables are supplied
  #exgs <- unlist(c(exogenous, parameters))
  exgs <- rep(1, vctrs::vec_size(external$lhs))
  exgs_names <- external$lhs
  exg_exprs <- purrr::map(external$rhs, function(x) parse(text=x))

  # Blocks of independent equations
  blocks <- unique(equations$block)
  blocks <- paste0("block", blocks)
  lblocks <- rep(0, vctrs::vec_size(blocks))
  names(lblocks) <- blocks

  mcols <- vctrs::vec_size(ends) + vctrs::vec_size(exgs) + vctrs::vec_size(lblocks)
  mnames <- c(names(ends), exgs_names, names(lblocks))

  # Matrix with variables
  m1 <- matrix(c(ends, exgs, lblocks), nrow = t, ncol = mcols, dimnames = list(1:t, mnames), byrow = T)

  for (var in seq_along(exgs_names)) {
    m1[, exgs_names[[var]]] <- eval(exg_exprs[[var]])
  }

  # All variables start at 1 (including exogenous)
  # Otherwise problems may arise if some endogenous
  # variable depends on lagged exogenous

  m1[1, ] <- 1

  if (!is.null(initial)) {
    initial <- .eq_as_tb(initial)
    initial_names <- initial$lhs
    initial_exprs <- purrr::map(initial$rhs, function(x) parse(text = x))

    for (var in seq_along(initial_names)) {
      m1[1, initial_names[[var]]] <- eval(initial_exprs[[var]])
    }
  }


  # Loop on a list instead of a matrix
  #m1 <- as.list(data.frame(m1))

  return(m1)
}


.sfcr_GaussSeidel <- function(m, equations, t, max_ite) {

  exprs <- purrr::map(equations$rhs, function(x) parse(text=x))

  checks <- rep(0, vctrs::vec_size(equations$lhs))
  names(checks) <- equations$lhs

  holdouts <- rep(3, vctrs::vec_size(equations$lhs))
  names(holdouts) <- equations$lhs

  for (i in 2:t) {
    for (block in unique(equations$block)) {
      block_name <- paste0('block', block)

      # Just keeping this single line inside the loop call was more than doubling execution time
      equations_id <- equations[, 'id'][equations[, 'block'] == block]

      for (ite in 1:max_ite) {
        for (var in equations_id) {

          m[i, equations$lhs[[var]]] <- eval(exprs[[var]])
          #m[[equations$lhs[[var]]]][[i]] <- eval(exprs[[var]])

          checks[[var]] <- abs((m[[i, var]] - holdouts[[var]]) / (holdouts[[var]] + 1e-15))

          holdouts[[var]] <- m[[i, var]]
        }

        m[i, block_name] <- ite
        #m[[paste0('block', block)]][[i]] <- ite

        if (isTRUE(all(checks < 1e-4))) {break}

      }
    }
  }
  return(m)
}
