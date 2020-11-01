# .eq_as_tb <- function(sectors) {
#   suppressMessages({
#     purrr::map(unlist(sectors), ~paste0(deparse(.x, width.cutoff = 500),collapse = "")) %>%
#       dplyr::bind_cols(.name_repair = "universal") %>%
#       t() %>%
#       tibble::as_tibble(.name_repair = "universal") %>%
#       tidyr::separate("...1", c("lhs", "rhs"), sep = " ~ ")
#   })
# }

.eq_as_tb <- function(equations) {
  purrr::map(equations, ~deparse(.x, width.cutoff = 500)) %>%
    unlist %>%
    tibble::tibble(vars = .) %>%
    tidyr::separate(vars, c('lhs', 'rhs'), ' ~ ')
}

# Find dependencies and order the equations
.add_time_stamps <- function(eq_as_tb) {
  eq_as_tb %>%
    dplyr::mutate(rhs = gsub("\\[-1\\]", "___", rhs))
}

.find_dependencies <- function(tb_eqs, pattern) {
  tb_eqs %>%
    dplyr::mutate(lhs = forcats::fct_inorder(lhs)) %>%
    dplyr::nest_by(lhs) %>%
    dplyr::mutate(depends = stringr::str_extract_all(data, pattern)) %>%
    dplyr::mutate(depends = list(unique(depends))) %>%
    dplyr::mutate(l = vctrs::vec_size(depends)) %>%
    dplyr::mutate(lhs = as.character.factor(lhs)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n = dplyr::row_number())
}

.midsteps <- function(m, pat) {
  m %>%
    dplyr::rowwise() %>%
    dplyr::mutate(depends = purrr::simplify_all(list(purrr::map(depends, function(.x) .x[!grepl(pat, .x, perl = T)])))) %>%
    dplyr::mutate(l = vctrs::vec_size(depends))}

.sfcr_order_eqs <- function(equations, .simultaneous = FALSE) {
  eqs <- .eq_as_tb(equations)

  eqs <- .add_time_stamps(eqs)

  # Re arrange equations for a optimal estimation

  # 1. Get a pattern to match out the endogenous variables on the right-hand side of the equations
  pend <- paste0("(?<![[:alnum:]])(",paste0(eqs$lhs, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")

  # 2. LookUp tibble that will be looked upon to find an optimal order:
  look_up_tb <- .find_dependencies(eqs, pend)


  # In this loop, the code check the equations first to see all that depends only on lagged values
  # Or exogenous variables. It places these variables in the beginning of the `block` vector.
  # It then searches for the variables that depend on the variables that were already identified.
  # And does it successively until it finds all the variables.

  #TODO: Remove .simultaneous option and throw a message if the model is cyclical

  block <- NULL
  pat <- as.character()
  iter <- NULL
  for (i in seq_along(look_up_tb$lhs)) {
    if (purrr::is_empty(pat)) {
      block <- look_up_tb[, "n"][look_up_tb[, 'l'] == 0]
      iter <- rep(i, vctrs::vec_size(block))

    } else {
      if (isTRUE(.simultaneous)) {
        new_block <- look_up_tb[-block,] %>% .midsteps(pat) %>% {.[, "n"][.[, "l"] == min(.[, "l"])]}
      }
       else {
        new_block <- look_up_tb[-block,] %>% .midsteps(pat) %>% {.[, "n"][.[, "l"] == 0]}
      }
      if (vctrs::vec_size(new_block) == 0) stop('Please set `.simultaneous = TRUE` to run models that are simultaneously determined.')
      block <- c(block, new_block)
      iter <- c(iter, rep(i, vctrs::vec_size(new_block)))
    }

    pat <- paste0("^(", paste0(look_up_tb[block, 'lhs']$lhs, collapse = "|"), ")$")

    if (vctrs::vec_size(look_up_tb) == vctrs::vec_size(block)) {break}
  }

  eqs <- eqs[block,]
  eqs['block'] <- iter

  return(eqs)
}
