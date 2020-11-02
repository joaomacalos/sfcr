.prep_equations <- function(ordered_eqs, exogenous, parameters) {

  pend <- paste0("(?<![[:alnum:]])(", paste0(ordered_eqs$lhs, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")
  pendlag <- paste0("(?<![[:alnum:]])(", paste0(ordered_eqs$lhs, collapse = "|"), ")(?=___)")
  pexg <- paste0("(?<![[:alnum:]])(", paste0(c(names(exogenous), names(parameters)), collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")
  pexglag <- paste0("(?<![[:alnum:]])(", paste0(c(names(exogenous), names(parameters)), collapse = "|"), ")(?=___)")

  x <- ordered_eqs %>%
    dplyr::mutate(rhs = gsub(pend, "m\\[i,'\\1'\\]", rhs, perl = T),
                  rhs = gsub(pendlag, "m\\[i-1,'\\1'\\]", rhs, perl = T),
                  rhs = gsub(pexg, "m\\[i,'\\1'\\]", rhs, perl = T),
                  rhs = gsub(pexglag, "m\\[i-1,'\\1'\\]", rhs, perl = T),
                  rhs = gsub("___", "", rhs),
                  id = dplyr::row_number())

  return(x)
}


.make_matrix <- function(equations, exogenous, parameters, t) {

  # If no initial values supplied, start with a 1
  ends <- rep(1, vctrs::vec_size(equations$lhs))
  names(ends) <- equations$lhs

  # Exogenous variables are supplied
  exgs <- unlist(c(exogenous, parameters))

  # Blocks of independent equations
  blocks <- unique(equations$block)
  blocks <- paste0("block", blocks)
  lblocks <- rep(0, vctrs::vec_size(blocks))
  names(lblocks) <- blocks

  mcols <- vctrs::vec_size(ends) + vctrs::vec_size(exgs) + vctrs::vec_size(lblocks)
  mnames <- c(names(ends), names(exgs), names(lblocks))

  # Matrix with variables
  m1 <- matrix(c(ends, exgs, lblocks), nrow = t, ncol = mcols, dimnames = list(1:t, mnames), byrow = T)

  # All variables start at 1 (including exogenous)
  # Otherwise problems may arise if some endogenous
  # variable depends on lagged exogenous

  m1[1, ] <- 1

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
      for (ite in 1:max_ite) {
        for (var in equations[, 'id'][equations[, 'block'] == block]) {

          m[i, equations$lhs[[var]]] <- eval(exprs[[var]])

          checks[[var]] <- abs((m[[i, var]] - holdouts[[var]]) / (holdouts[[var]] + 1e-15))

          holdouts[[var]] <- m[[i, var]]
        }

        m[i, paste0('block', block)] <- ite

        if (isTRUE(all(checks < 1e-4))) {break}

      }
    }
  }
  return(m)
}
