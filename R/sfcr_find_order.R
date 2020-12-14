#' Split the formulae into a `tibble` with the left-hand side and
#' right-hand side as columns
#'
#' @param equations list of equations
#'
#' @importFrom rlang .data
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.eq_as_tb <- function(equations) {
  vars <- purrr::map(equations, ~paste0(deparse(.x, width.cutoff = 500), collapse = "")) %>%
    unlist

  tibble::tibble(vars) %>%
    tidyr::separate(.data$vars, c('lhs', 'rhs'), ' ~ ') %>%
    dplyr::mutate(rhs = stringr::str_replace_all(.data$rhs, "d\\((.*?)\\)", "\\(\\1 - \\1\\[-1\\]\\)"))
}

#' Find dependencies and order the equations
#'
#' @param eq_as_tb A tibble generated with \code{.eq_as_tb()}.
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.add_time_stamps <- function(eq_as_tb) {
  eq_as_tb %>%
    dplyr::mutate(rhs = gsub("\\[-1\\]", "___", .data$rhs))
}

#' Find dependencies and order the equations
#'
#' @param x A vector to modify
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.add_time2 <- function(x) {
  gsub("\\[-1\\]", "___", x)
}

#' Pattern replacement var
#' @param x vector of variables
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.pvar <- function(x) {paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")}

#' Pattern replacement lag
#' @param x vector of variables
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.pvarlag <- function(x) {paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?=___)")}


#' Find adjacency matrix for a system of equations
#'
#' @param equations A system of equations already time stamped
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.sfcr_find_adjacency <- function(equations) {

  km <- matrix(nrow = length(equations$lhs), ncol = length(equations$lhs))
  rownames(km) <- equations$lhs
  colnames(km) <- equations$lhs

  km[is.na(km)] <- 0

  # Extract them from equations
  k3 <- equations %>%
    dplyr::mutate(rhs = stringr::str_extract_all(.data$rhs, .pvar(equations$lhs)))

  # Loop to fill the adjacency matrix
  for (var in seq_along(k3$lhs)) {
    km[k3$lhs[[var]], k3$rhs[[var]]] <- 1
  }

  return(km)
}

#' Find blocks of independent equations (wrapper around \code{igraph} functions)
#'
#' @param adj Adjacency matrix
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.find_blocks <- function(adj) {
  g <- igraph::graph.adjacency(adjmatrix = t(adj),mode = "directed")

  blocks <- igraph::components(g, "strong")$membership

  return(blocks)
}


#' Place the equations in the correct order for estimation
#'
#' @param equations Equations supplied by the user.
#'
#' @details Create an adjacency matrix and apply
#' \code{.find_blocks()} function to identify the blocks
#' of independent equations.
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.sfcr_find_order <- function(equations) {

  k1 <- .eq_as_tb(equations)

  k2 <- k1 %>%
    dplyr::mutate(rhs = .add_time2(.data$rhs))

  # STEP 1
  # Create adjacency matrix

  km <- .sfcr_find_adjacency(k2)

  # STEP2
  # Use the igraph algorithm to find the block structure

  blocks <- .find_blocks(km)

  k2[['block']] <- blocks

  return(k2)
}
