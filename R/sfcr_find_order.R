#' Split the formulae into a `tibble` with the left-hand side and
#' right-hand side as columns
#'
#' @param equations list of equations
#'
#' @author João Macalós
#'
.eq_as_tb <- function(equations) {
  purrr::map(equations, ~paste0(deparse(.x, width.cutoff = 500), collapse = "")) %>%
    unlist %>%
    tibble::tibble(vars = .) %>%
    tidyr::separate(vars, c('lhs', 'rhs'), ' ~ ')
}

#' Find dependencies and order the equations
#'
.add_time_stamps <- function(eq_as_tb) {
  eq_as_tb %>%
    dplyr::mutate(rhs = gsub("\\[-1\\]", "___", rhs))
}

#' Algorithm to identify the blocks of independent equations inside the model.
#'
#' @param m A adjacency matrix of the endogenous variables.
#'
#' @details This algorithm finds the block of independent equations sequentially.
#' It first looks for all variables that depends only on exogenous variables or
#' on lagged values. It saves these variables as the first block and eliminate
#' them from the adjacency matrix.
#' It repeats this process until all blocks are identified or until it finds
#' a cycle.
#' If a cycle is found, it jumps to the bottom and finds all variables that
#' does not have any children. The algorithm assign these variables to the
#' end of the block list and eliminates them sequentially until it reaches
#' the cycle block.
#'
#' All the cyclical variables are treated as a single cycle. Hence, this algorithm
#' is unable to identify independent cycles. However, this is not a common
#' structure of SFC models.
#'
#' @author João Macalós
#'
.find_blocks <- function(m) {
  # Step 4: Loop to find dependencies
  # My strategy is to isolate nodes without children and without parents recursively.
  # The remaining nodes are treated as a big cycle.
  # In this way I miss independent cycles, but I find the recursive sections.

  max_blocks <- nrow(m)
  blocks <- rep(NA_integer_, max_blocks)
  names(blocks) <- colnames(m)

  for (iter in 1:max_blocks) {
    vars_in_block <- colnames(m[1, rowSums(m) == 0, drop = F])

    # Break if no vars without parents and look for vars without children
    if (vctrs::vec_size(vars_in_block) == 0) {
      vars_in_block <- rownames(m[colSums(m) == 0, 1, drop = F])
      #break

      # Break if no vars without children and return remaining vars as a
      # big cycle

      if (vctrs::vec_size(vars_in_block) == 0) {
        vars_in_block <- rownames(m)

        for (var in vars_in_block) {
          blocks[[var]] <- max_blocks - iter
        }

        break
      } else {

        for (var in vars_in_block) {
          blocks[[var]] <- max_blocks - iter
        }

        if (nrow(m) == 1) {
          break
        } else {
          m <- m[colSums(m) > 0, colSums(m) > 0, drop = F]
        }
      }

    } else {
      for (var in vars_in_block) {
        blocks[[var]] <- iter
      }

      if (nrow(m) == 1) {
        break
      } else {
        m <- m[rowSums(m) > 0, rowSums(m) > 0, drop = F]
      }
    }

  }
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

  k2 <- .add_time_stamps(k1)

  # STEP 1
  # Create adjacency matrix

  # Create empty matrix
  km <- matrix(nrow = length(k2$lhs), ncol = length(k2$lhs))
  rownames(km) <- k2$lhs
  colnames(km) <- k2$lhs

  km[is.na(km)] <- 0

  # Detect all endogenous vars
  ken <- paste0("(?<![[:alnum:]])(", paste0(k2$lhs, collapse = '|'), ")(?![[:alnum:]]|\\_|\\.)")

  # Extract them from equations
  k3 <- k2 %>%
    dplyr::mutate(rhs = stringr::str_extract_all(rhs, ken)) %>%
    dplyr::rowwise() %>%
    dplyr::filter(vctrs::vec_size(rhs) > 0)

  # Loop to fill the adjacency matrix
  for (var in seq_along(k3$lhs)) {
    km[k3$lhs[[var]], k3$rhs[[var]]] <- 1
  }

  # STEP2
  # Use the algorithm to find the blocks

  blocks <- .find_blocks(km)

  blocks <- sort(blocks)
  ordered <- names(sort(blocks))

  k2 <- k2[match(ordered, k2$lhs), ]
  k2$block <- blocks

  return(k2)
}
