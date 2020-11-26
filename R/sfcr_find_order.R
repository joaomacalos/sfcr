#' Split the formulae into a `tibble` with the left-hand side and
#' right-hand side as columns
#'
#' @param equations list of equations
#'
#' @importFrom rlang .data
#'
#' @author João Macalós
#'
.eq_as_tb <- function(equations) {
  vars <- purrr::map(equations, ~paste0(deparse(.x, width.cutoff = 500), collapse = "")) %>%
    unlist

  tibble::tibble(vars) %>%
    tidyr::separate(.data$vars, c('lhs', 'rhs'), ' ~ ')
}

#' Find dependencies and order the equations
#'
#' @param eq_as_tb A tibble generated with \code{.eq_as_tb()}.
#'
#' @author João Macalós
#'
.add_time_stamps <- function(eq_as_tb) {
  eq_as_tb %>%
    dplyr::mutate(rhs = gsub("\\[-1\\]", "___", .data$rhs))
}

.add_time2 <- function(x) {
  gsub("\\[-1\\]", "___", x)
}

#' Pattern replacement var
#' @param x vector of variables
#'
.pvar <- function(x) {paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")}

#' Pattern replacement lag
#' @param x vector of variables
#'
.pvarlag <- function(x) {paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(x, collapse = "|"), ")(?=___)")}



.sfcr_find_adjacency <- function(equations) {

  km <- matrix(nrow = length(equations$lhs), ncol = length(equations$lhs))
  rownames(km) <- equations$lhs
  colnames(km) <- equations$lhs

  km[is.na(km)] <- 0

  # Detect all endogenous vars
  #ken <- paste0("(?<![[:alnum:]]|\\_|\\.)(", paste0(equations$lhs, collapse = '|'), ")(?![[:alnum:]]|\\_|\\.)")

  # Extract them from equations
  k3 <- equations %>%
    dplyr::mutate(rhs = stringr::str_extract_all(.data$rhs, .pvar(equations$lhs)))

  #k3 <- equations %>%
  #  dplyr::mutate(rhs = stringr::str_extract_all(.data$rhs, ken))# %>%
    #dplyr::rowwise() %>%
    #dplyr::filter(vctrs::vec_size(.data$rhs) > 0)

  # Loop to fill the adjacency matrix
  for (var in seq_along(k3$lhs)) {
    km[k3$lhs[[var]], k3$rhs[[var]]] <- 1
  }

  return(km)
}

#' Find blocks of independent equations
#'
#' @param adj Adjacency matrix
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
.sfcr_find_order <- function(equations) {

  k1 <- .eq_as_tb(equations)

  k2 <- k1 %>%
    dplyr::mutate(rhs = .add_time2(.data$rhs))

  # k2 <- .add_time_stamps(k1)

  # STEP 1
  # Create adjacency matrix

  km <- .sfcr_find_adjacency(k2)

  # STEP2
  # Use the igraph algorithm to find the block structure

  blocks <- .find_blocks(km)

  #blocks <- .find_blocks3(km)

  #blocks <- sort(blocks)
  #ordered <- names(sort(blocks))

  #k2 <- k2[match(ordered, k2$lhs), ]
  k2[['block']] <- blocks

  return(k2)
}
