.eq_as_tb <- function(equations) {
  purrr::map(equations, ~paste0(deparse(.x, width.cutoff = 500), collapse = "")) %>%
    unlist %>%
    tibble::tibble(vars = .) %>%
    tidyr::separate(vars, c('lhs', 'rhs'), ' ~ ')
}

# Find dependencies and order the equations
.add_time_stamps <- function(eq_as_tb) {
  eq_as_tb %>%
    dplyr::mutate(rhs = gsub("\\[-1\\]", "___", rhs))
}


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


.sfcr_find_order <- function(equations) {
  # Step 1: eq as tb
  k1 <- .eq_as_tb(equations)

  # Step 2: add time stamps
  k2 <- .add_time_stamps(k1)

  # Step 3: Make adjacency matrix
  # 3.1 empty matrix
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

  blocks <- .find_blocks(km)

  blocks <- sort(blocks)
  ordered <- names(sort(blocks))

  k2 <- k2[match(ordered, k2$lhs), ]
  k2$block <- blocks

  return(k2)
}
