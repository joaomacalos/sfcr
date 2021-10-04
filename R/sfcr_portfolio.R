#' Check that symmetry condition is valid and fulfill missing entries
#'
#' @param m A square matrix
#'
#' @keywords internal
#'
#' @author João Macalós
#'
.check_symmetry <- function(m) {

  for (.i in 1:nrow(m)) {
    for (.j in 1:ncol(m)) {
      if (!is.na(m[.i, .j])) {
        # Check if subscript is out of bounds
        if (.j > nrow(m)) {
          next
        }
        # Check if symmetry is already fulfilled
        else if (isTRUE(m[.i, .j] == m[.j, .i])) {
          next
        }
        # Since they are not equal, check if they are all NA. If not, break because symmetry condition is invalid.
        else if (all(!is.na(c(m[.i, .j], m[.j, .i])))) {
          message = paste0("Symmetry condition is invalid. Check row `", .i, "` and column `", .j, "` and try again.")
          rlang::abort(message)
        }
        # Impose symmetry condition
        else {
          m[.j, .i] = m[.i, .j]
        }
      }
    }
  }

  return(m)
}


#' Scan rows to fill whenever there's only one value missing.
#'
#' @param m A square matrix
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.scan_rows <- function(m) {
  which_row <- which(apply(m, 1, function(x) length(which(is.na(x)))) == 1)
  if (vctrs::vec_size(which_row) != 0) {
    rsums <- rowSums(m, na.rm = TRUE)
    for (.r in which_row) {
      na_is <- which(is.na(m[.r, ]))
      m[.r, na_is] <- -1 * rsums[.r]
    }
  }
  return(m)
}

#' Scan columns to fill whenever there's only one value missing.
#'
#' @param m A square matrix
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.scan_cols <- function(m) {
  which_col <- which(apply(m, 2, function(x) length(which(is.na(x)))) == 1)
  if (vctrs::vec_size(which_col) != 0) {
    csums <- colSums(m, na.rm = TRUE)
    for (.c in which_col) {
      na_is <- which(is.na(m[, .c]))
      m[na_is, .c] <- -1 * csums[.c]
    }
  }
  return(m)
}


#' Check that the portfolio matrix respect the horizontal and vertical
#' adding-up constraints
#'
#' @param m A square matrix
#' @param which Rows or columns?
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.validate_scan <- function(m, which) {
  match.arg(which, c("col", "row"))

  if (which == "col") {
    # Check cols without NAs
    which_col <- purrr::map_lgl(seq_len(ncol(m)), ~all(!is.na(m[, .x])))
    if (abs(sum(m[, which(which_col)])) > 1e-6) {
      #print(abs(sum(m[, which(which_col)])))
      rlang::abort("Invalid inputs. The adding-up requirements are not fulfilled. Please rewrite your portfolio matrix.")
    }
  } else {
    # Check rols without NAs
    which_row <- purrr::map_lgl(seq_len(nrow(m)), ~all(!is.na(m[.x, ])))
    if (abs(sum(m[which(which_row), ])) > 1e-6) {
      rlang::abort("Invalid inputs. The adding-up requirements are not fulfilled. Please rewrite your portfolio matrix.")
    }
  }

}

#' Fill all possible rows and columns and validate
#'
#' @param m A square matrix
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.fill_rows_and_cols <- function(m) {
  # Scan rows
  m <- .scan_rows(m)
  # Validate with cols
  .validate_scan(m, "col")

  # Scan cols
  m <- .scan_cols(m)
  # Validate with rows
  .validate_scan(m, "row")

  return(m)
}

#' Find a valid matrix of portfolio parameters
#'
#' The \code{sfcr_portfolio()} function calculates a valid matrix of portfolio
#' parameters by applying the symmetry condition and then filling the missing
#' rows accordingly to the vertical and horizontal adding-up constraints.
#'
#' This function calculates only the values of the rates of return matrix, i.e.,
#' the internal matrix. The adding-up constraint number 1, that calculates the
#' share of assets in the net wealth and the impact of regular income to wealth
#' ratio must be calculated separately.
#'
#'
#'
#' @param m A matrix of parameter names
#' @param known A named vector of known parameters. One entry for each symmetry
#' condition is enough to find a valid matrix.
#'
#' @details If supplied with insufficient parameters, the function will return a
#' matrix with NA values.
#'
#' This function requires at least (n^2 - n)/2 known parameters to find a valid portfolio
#' matrix, where n is the number of rows/columns. This is achieved by setting known parameters
#' outside the diagonal and not on symmetrical entries, i.e., not lambda12 and lambda21, for
#' example.
#'
#' @author João Macalós
#'
#' @examples
#' j1 <- matrix(paste0("lambda", c(11:14, 21:24, 31:34, 41:44)), ncol = 4, nrow = 4, byrow = TRUE)
#' j2 <- c(lambda12 = 0, lambda13 = 0, lambda14 = 0, lambda23 = -15, lambda24 = -15, lambda34 = -15)
#'
#' sfcr_portfolio(j1, j2)
#'
#' @export
sfcr_portfolio <- function(m, known) {
  m <- apply(m, 1, function(x) stringr::str_replace_all(x, purrr::map_chr(known, as.character)))
  kx <- m[stringr::str_detect(m, "[:alpha:]")]
  slots_l <- which(c(m) %in% kx)

  m <- suppressWarnings(apply(m, 1, as.numeric))

  for (.ite in 1:350) {
    m <- .check_symmetry(m)

    newm <- .fill_rows_and_cols(m)

    #if (max(abs(newm[!is.na(newm)] - m[!is.na(m)])) < 1e-6) {
    if (length(newm[!is.na(newm)]) == length(m[!is.na(m)])) {
      break
    } else {
      m <- newm
    }
  }

  which_l <- newm[slots_l]
  names(which_l) <- kx

  return(list(lambdas = which_l, matrix = newm))
}
