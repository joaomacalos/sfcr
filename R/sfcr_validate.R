#' Check if all values in x are equal
#'
#' @param x A numeric vector
#' @param tol Tolerance to declare equality
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.all_equal <- function(x, tol) {diff(range(x)) < tol}


#' Check if two values are equal
#'
#' @param x,y numeric values
#' @param tol Tolerance to declare equality
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.is_equal <- function(x, y, tol) {abs(x - y) < tol}


#' Abort if row validation is not fulfilled
#'
#' @param r2names Names of offending rows
#' @param which Balance-sheet or transactions-flow matrix?
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.abort_water_leakr <- function(r2names, which) {

  if (which == "tfm") {

    if (length(r2names) == 1) {
      message <- paste0("Ooops, water is leaking!\n`", r2names, "` row does not sum to zero. Please make sure that the transactions-flow matrix is written consistently with the model equations.")
    } else {
      message <- paste0("Ooops, water is leaking!\n`", paste0(r2names, collapse = ", "), "` rows do not sum to zero. Please make sure that the transactions-flow matrix is written consistently with the model equations.")
    }
  } else {
    if (length(r2names) == 1) {
      message <- paste0("Ooops, water is leaking!\n`", r2names, "` row does not sum to zero. Please make sure that the balance-sheet matrix is written consistently with the model equations.")
    } else {
      message <- paste0("Ooops, water is leaking!\n`", paste0(r2names, collapse = ", "), "` rows do not sum to zero. Please make sure that the balance-sheet matrix is written consistently with the model equations.")
    }
  }

  rlang::abort(message = message)
}


#' Abort if column validation is not fulfilled
#'
#' @param c2names Names of offending columns
#' @param which Balance-sheet or transactions-flow matrix?
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.abort_water_leakc <- function(c2names, which) {

  if (which == "tfm") {

    if (length(c2names) == 1) {
      message <- paste0("Ooops, water is leaking!\n`", c2names, "` column does not sum to zero. Please make sure that the transactions-flow matrix is written consistently with the model equations.")
    } else {
      message <- paste0("Ooops, water is leaking!\n`", paste0(c2names, collapse = ", "), "` columns do not sum to zero. Please make sure that the transactions-flow matrix is written consistently with the model equations.")
    }
  } else {
    if (length(c2names) == 1) {
      message <- paste0("Ooops, water is leaking!\n`", c2names, "` column does not sum to zero. Please make sure that the balance-sheet matrix is written consistently with the model equations.")
    } else {
      message <- paste0("Ooops, water is leaking!\n`", paste0(c2names, collapse = ", "), "` columns do not sum to zero. Please make sure that the balance-sheet matrix is written consistently with the model equations.")
    }
  }

  rlang::abort(message = message)
}




#' Get numeric matrix for evaluation from balance-sheet or
#' transactions-flow matrices
#'
#' @param mtrx Balance-sheet or transactions-flow matrix
#' @param bl1 calls from baseline model
#' @param bl2 external from baseline model
#'
#' @author João Macalós
#'
#' @keywords internal
.get_matrix <- function(mtrx, bl1, bl2) {

  nms <- colnames(mtrx)
  colnames(mtrx) <- c("name", nms[2:length(nms)])

  mtrx <- mtrx %>%
    dplyr::mutate(dplyr::across(-1, ~stringr::str_replace_all(.x, "d\\((.*?)\\)", "\\(\\1 - \\1\\[-1\\]\\)"))) %>%
    dplyr::mutate(dplyr::across(-1, ~.add_time2(.x))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(.pvar(bl1$lhs), "m\\[.i,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(.pvar(bl2), "m\\[.i,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(.pvarlag(bl1$lhs), "m\\[.i-1,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(.pvarlag(bl2), "m\\[.i-1,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub("___", "", .x)))

}


#' Validate a balance-sheet or transactions-flow matrix on the simulated data
#'
#' @param mtrx A balance-sheet or transactions-flow matrix
#' @param m A baseline model in matrix format -- get from attributes or with
#' \code{sfcr_get_matrix()} function.
#' @param which A balance-sheet or a transactions-flow matrix?
#' @param tol Tolerance for convergence
#' @param rtol Relative tolerance?
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.validate_matrix <- function(mtrx, m, which = "tfm", tol, rtol = FALSE) {
  match.arg(which, c("tfm", "bs"))

  k2 <- as.matrix(mtrx[, -1])
  k3 <- matrix(0, nrow = nrow(k2), ncol = ncol(k2))
  colnames(k3) <- colnames(k2)

  ids <- which(purrr::map_lgl(k2, ~stringr::str_length(.x) != 0))

  l1 <- 2:nrow(m)


  if (which == "tfm") {
    for (.i in l1) {
      for (.j in ids) {
        k3[[.j]] <- eval(str2expression(k2[[.j]]))
      }

      r1 <- rowSums(k3, na.rm = TRUE)
      c1 <- colSums(k3, na.rm = TRUE)

      # Absolute discrepancy
      if (isFALSE(rtol)) {

        # Rows
        if (isFALSE(.all_equal(r1, tol))) {

          r2 <- which(abs(r1) > tol)

          r2names <- mtrx[r2, ]$name

          .abort_water_leakr(r2names, "tfm")

        }


        # Columns
        if (isFALSE(.all_equal(c1, tol))) {

          c2 <- which(abs(c1) > tol)

          c2names <- colnames(k3[, c2])

          .abort_water_leakc(c2names, "tfm")

        }

      }

      # Relative discrepancy
      else {
          # Rows
          if (isFALSE(.all_equal(r1, 1e-3))) {

            r2 <- which(abs(r1) > 1e-3)

            # If the rows don't sum to zero, it might be the case that we are dealing with a growth model
            # in which numerical discrepancies accumulate.

            # Thus, we must check if the discrepancy is smaller than a certain threshold ratio of the positive
            # entries in that row. That's what I do below within the relative option:

            w1 <- purrr::map_dbl(r2, ~sum(k3[.x, ][which(k3[.x, ] > 0)]))

            if (all(r1[r2]/w1 > tol)) {

              r2names <- mtrx[r2, ]$name

              .abort_water_leakr(r2names, "tfm")

            }
          }

          # Columns
          if (isFALSE(.all_equal(c1, 1e-3))) {
            c2 <- which(abs(c1) > tol)

            w1 <- purrr::map_dbl(c2, ~sum(k3[, .x][which(k3[, .x] > 0)]))

            if (all(c1[c2]/w1 > tol)) {

              c2names <- colnames(k3[, c2])

              .abort_water_leakc(c2names, "tfm")

            }

          }

        }

    }
  }

  if (which == "bs") {

    for (.i in l1) {
      for (.j in ids) {
        k3[[.j]] <- eval(str2expression(k2[[.j]]))
      }

     if (!(colnames(k3)[ncol(k3)] %in% c("Sum", "sum", "SUM"))) {
        Sum <- 0
        k3 <- cbind(k3, Sum)
      }

      r1 <- rowSums(k3[, -ncol(k3)], na.rm = TRUE)

      rs <- k3[, ncol(k3)]
      c1 <- colSums(k3, na.rm = TRUE)

      # Absolute
      if (isFALSE(rtol)) {
        # Rows
        if (isFALSE(all(.is_equal(r1, rs, tol)))) {
          r2 <- which(!.is_equal(r1, rs, tol))

          r2names <- mtrx[r2, "name"]$name

          .abort_water_leakr(r2names, "bs")

        }

        # Columns
        if (isFALSE(all(.is_equal(c1, 0, tol)))) {

          c2 <- which(!.is_equal(c1, 0, tol))

          c2names <- colnames(k3[, c2])

          .abort_water_leakc(c2names, "bs")

        }
      }

      # Relative
      else {
        # Rows
        if (isFALSE(all(.is_equal(r1, rs, 1e-3)))) {

          r2 <- which(!.is_equal(r1, rs, 1e-3))

          if (all(abs((r1[r2] - rs[r2])/rs[r2] > tol))) {

            r2names <- mtrx[r2, ]$name

            .abort_water_leakr(r2names, "bs")

          }

        }

        # Columns
        if (isFALSE(all(.is_equal(c1, 0, tol)))) {

          c2 <- which(!.is_equal(c1, 0, tol))

          w1 <- purrr::map_dbl(c2, ~sum(k3[, .x][which(k3[, .x] > 0)]))

          if (all(c1[c2]/w1 > tol)) {

            c2names <- colnames(k3[, c2])

            .abort_water_leakc(c2names, "bs")

          }
        }

      }
    }
  }

}


#' Validate a transactions-flow or balance-sheet matrix
#'
#' This function validates a transactions-flow or balance-sheet
#' matrix with the simulated data obtained with \code{sfcr_baseline()}
#' function
#'
#' @param matrix A transactions-flow or balance sheet matrix
#' @param baseline A baseline model.
#' @param tol A numerical value indicating the absolute accepted discrepancy accepted
#' to validate whether the rows and columns are equal to their expected values.
#' @param rtol A logical value indicating whether relative discrepancies should be
#' evaluated. It defaults to \code{FALSE}. Stationary models should pass the test using
#' a absolute level while growth models might need a relative validation since computational
#' discrepancies tend to get larger with the model. See details for further information.
#' @param which Either "bs" (balance-sheet matrix) or "tfm" (transactions-flow matrix).
#'
#' @details The relative discrepancy is calculated differently if we are dealing with a
#' transactions-flow matrix or with a balance-sheet matrix. If \code{which} is set to \code{tfm},
#' the sum of the row/column is evaluated against the sum of the positive entries of that row/column.
#'
#' For example, in a transactions-flow matrix with three entries in the "change in the stock of bills"
#' row (-Delta (Bhd), + Delta (Bs), and + Delta (Bbd)), the discrepancy d = Delta Bs - Delta Bhd - Delta Bbd
#' is evaluated against Delta Bs, i.e., the row is validated if d/Delta Bs < tol.
#'
#' In a balance-sheet matrix, all the rows/columns that sum to zero are validated exactly as
#' in a transactions-flow matrix. The exception to this rule is when there is a expected value. In this case,
#' the discrepancy is evaluated as a proportion of the expected. value
#'
#' To prevent unnecessary calculations, a absolute check with tolerance defined as 1e-3 is executed
#' prior to this evaluation.
#'
#'
#' @details The absolute discrepancy set with \code{tol} should be enough to validate
#' a stationary SFC Model.
#'
#' @author João Macalós
#'
#' @export
sfcr_validate <- function(matrix, baseline, which, tol = 1, rtol = FALSE) {

  match.arg(which, c("tfm", "bs"))

  bl1 <- attr(baseline, "calls")
  bl2 <- attr(baseline, "external")$lhs
  m <- attr(baseline, "matrix")

  # If TFM

  if (which == "tfm") {
    tfm <- .get_matrix(matrix, bl1, bl2)

    .validate_matrix(tfm, m, "tfm", tol = tol, rtol = rtol)

    cat("Water tight! The transactions-flow matrix is consistent with the simulated model.")
  }

  else {
    bs <- .get_matrix(matrix, bl1, bl2)

    .validate_matrix(bs, m, "bs", tol = tol, rtol = rtol)

    cat("Water tight! The balance-sheet matrix is consistent with the simulated model.")
  }


}
