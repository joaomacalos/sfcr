.args_to_row <- function(arg, tb) {
  purrr::imap_dfr(arg, ~dplyr::mutate(tb, !!.y := .x)) %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), ~dplyr::first(.x[!is.na(.x)]))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, "")))
}


#' Create balance-sheet or transactions-flow matrices
#'
#' @param columns Vector containing the name of the columns in the matrix.
#' @param codes A vector containing the abbreviation of the
#' column names that is going to be used as a reference to
#' build the rows. They must be provided in the same order
#' as the \code{columns}.
#'
#' @param ... Vectors that fill the rows of the matrix.
#' The first element of each vector **must** be the name of the
#' row in the respective matrix. The remaining elements of the vector
#' must be name-value pairs that exactly matches the \code{codes} argument.
#' See the examples for further details.
#'
#' @details This function can be used to generate a transactions-
#' flow matrix as well as a balance-sheet matrix. If the user
#' wishes to validate these matrices with the simulated data,
#' please pay attention to the following details:
#'
#' * Transactions-flow Matrix:
#'   In the transactions-flow matrix, the `sum` column is
#'   going to be generated automatically by the display and validation
#'   functions. Please do not add them by hand.
#'
#' * Balance-sheet Matrix:
#'   In the balance-sheet matrix, it might be the case that some
#'   rows do not sum to zero. Therefore, the user must supply
#'   by hand the non-zero values of the \code{sum} column.
#'   This column should always be the last column of the matrix.
#'
#' @example inst/examples/example_sfcr_matrix.R
#'
#' @export
#'
sfcr_matrix <- function(columns, codes, ...) {

  abortifnot(length(columns) == length(codes), "The length of the `codes` argument must be equal to the length of `columns`.")

  tb <- suppressMessages(
    tibble::as_tibble(matrix(NA_character_, ncol = length(codes) + 1), .name_repair = "unique") %>%
      rlang::set_names(c("name", codes))
    )

  ar <- list(...)
  ar <- purrr::map(ar, ~c(.x[-1], name = .x[1]))
  #ar <- purrr::imap(ar, ~c(.x, name = .y))

  tb2 <- purrr::map_dfr(ar, ~.args_to_row(.x, tb))

  colnames(tb2) <- c("name", columns)

  return(tb2)
}


.find_names <- function(matrix) {
  p1 <- matrix %>%
    dplyr::mutate(dplyr::across(-1, ~gsub("\\+|\\-|\\*|\\/|\\(|\\)|\\[-1\\]", " ", .x))) %>%
    dplyr::mutate(dplyr::across(-1, ~stringr::str_squish(.x))) %>%
    dplyr::mutate(dplyr::across(-1, ~stringr::str_split(.x, " "))) %>%
    dplyr::mutate(dplyr::across(-1, ~purrr::map(.x, function(y) unique(y))))

  nms <- unlist(p1[, -1])

  nms[purrr::map_lgl(nms, ~!(.x %in% c("", "0")))]
}

.to_latex_style <- function(matrix, nms) {

  # Dictionary to remove numbers from names
  words <- c("_zero_", "_one_", "_two_", "_three_", "_four_", "_five_", "_six_", "_seven_", "_eight_", "_nine_")
  n_words <- as.character(0:9)

  names(n_words) <- words
  names(words) <- n_words

  # Load existing names in the matrix
  v <- nms

  # English numbers
  v <- stringr::str_replace_all(v, purrr::imap_chr(words, ~{.y = .x}))

  v2 <- as.character(seq_along(v))
  names(v2) <- v
  v2 <- rev(v2[order(nchar(names(v2)), v2)])

  v3 <- v
  names(v3) <- as.character(seq_along(v))

  #pat <- "\\(([[:digit:]]{1,})-[[:digit:]]{1,}\\[-1\\]\\)"
  pat <- "\\(([[:digit:]]{1,})-[[:digit:]]{1,}\\[-_one_\\]\\)"

  matrix <- matrix %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(" ", "", .x))) %>%
    dplyr::mutate(dplyr::across(-1, ~stringr::str_replace_all(.x, purrr::imap_chr(words, function(x, y) y = x)))) %>%
    dplyr::mutate(dplyr::across(-1, ~stringr::str_replace_all(.x, purrr::imap_chr(v2, function(x, y) y = x)))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(pat, "\\\\Delta \\1", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~stringr::str_replace_all(.x, purrr::imap_chr(rev(v3), function(x, y) y = x)))) %>%
    dplyr::mutate(dplyr::across(-1, ~stringr::str_replace_all(.x, purrr::imap_chr(n_words, function(x, y) y = x)))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub("(.*)", "$\\1$", .x))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub("\\$\\$", "", .x))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub("\\*", "\\\\cdot ", .x))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub("\\[-1\\]", "_{-1}", .x)))

  zero_row <- matrix[1, ]
  zero_row <- purrr::modify(zero_row, ~ "$0$")
  zero_row[[1]] <- "$\\sum$"

  matrix <- matrix %>%
    rbind(zero_row)

  return(matrix)
}

#' Print matrix to screen
#'
#' @param matrix A balance sheet or transactions-flow matrix
#' @param which Is it a balance-sheet or a transactions-flow matrix?
#'
#' @details This function takes a matrix as input and generate a \code{kableExtra}
#' table with math symbols displayed in latex style.
#'
#' @example inst/examples/example_sfcr_matrix_display.R
#'
#'
#' @export
sfcr_matrix_display <- function(matrix, which = "tfm") {
  match.arg(which, c("tfm", "bs"))

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Packages \"kableExtra\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Make sure that first column is named "name"
  nms_matrix <- colnames(matrix)
  colnames(matrix) <- c("name", nms_matrix[2:length(nms_matrix)])

  nms <- .find_names(matrix)

  latex <- .to_latex_style(matrix, nms)

  nms_latex <- colnames(latex)

  if (which == "tfm") {
    latex <- latex %>%
      dplyr::mutate(sum = "$0$") %>%
      purrr::set_names(c("", nms_latex[2:length(nms_latex)], "$\\sum$"))
  } else {
    latex <- latex %>%
      dplyr::mutate(dplyr::across(ncol(latex), ~dplyr::if_else(str_length(.x) == 0, "$0$", .x))) %>%
      purrr::set_names(c("", nms_latex[2:(length(nms_latex) - 1)], "$\\sum$"))
  }


  kableExtra::kbl(latex,
                  format = "html",
                  table.attr = "style = \"color: black;\""
  ) %>%
    kableExtra::kable_styling(c("striped", "hover"))

}


.all_equal <- function(x) {diff(range(x)) < 1e-3}
.is_equal <- function(x, y) {abs(x - y) < 1e-3}


.get_matrix <- function(mtrx, baseline, bl1, bl2) {

  nms <- colnames(mtrx)
  colnames(mtrx) <- c("name", nms[2:length(nms)])

  mtrx <- mtrx %>%
    dplyr::mutate(dplyr::across(-1, ~.add_time2(.x))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(.pvar(bl1$lhs), "m\\[i,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(.pvar(bl2), "m\\[i,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(.pvarlag(bl1$lhs), "m\\[i-1,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub(.pvarlag(bl2), "m\\[i-1,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~gsub("___", "", .x)))

}


.validate_tfm <- function(tfm, m) {
  k2 <- as.matrix(tfm[, -1])
  k3 <- matrix(NA_real_, nrow = nrow(k2), ncol = ncol(k2))
  colnames(k3) <- colnames(k2)

  ids <- which(purrr::map_lgl(k2, ~stringr::str_length(.x) != 0))

  l1 <- 2:nrow(m)

  for (i in l1) {
    for (j in ids) {
      k3[[j]] <- eval(str2expression(k2[[j]]))
    }

    r1 <- rowSums(as.matrix(k3), na.rm = T)
    c1 <- colSums(as.matrix(k3), na.rm = T)

    if (isFALSE(.all_equal(r1))) {
      r2 <- which(abs(r1) > 1e-3)
      message <- paste0("Ooops, water is leaking!\n`", tfm[r2, ]$name, "` row does not sum to zero. Please make sure that the transactions-flow matrix is written consistently with the model equations.")
      rlang::abort(message = message)
    }

    if (isFALSE(.all_equal(c1))) {
      c2 <- which(abs(c1) > 1e-3)
      message <- paste0("Ooops, water is leaking!\n`", tfm[, c2]$name, "` column does not sum to zero. Please make sure that the transactions-flow matrix is written consistently with the model equations.")
      rlang::abort(message = message)
    }
  }
}


.validate_matrix <- function(mtrx, m, which = "tfm") {
  match.arg(which, c("tfm", "bs"))

  k2 <- as.matrix(mtrx[, -1])
  k3 <- matrix(0, nrow = nrow(k2), ncol = ncol(k2))
  colnames(k3) <- colnames(k2)

  ids <- which(purrr::map_lgl(k2, ~stringr::str_length(.x) != 0))

  l1 <- 2:nrow(m)

  if (which == "tfm") {
    for (i in l1) {
      for (j in ids) {
        k3[[j]] <- eval(str2expression(k2[[j]]))
      }

      r1 <- rowSums(as.matrix(k3), na.rm = T)
      c1 <- colSums(as.matrix(k3), na.rm = T)

      if (isFALSE(.all_equal(r1))) {
        r2 <- which(abs(r1) > 1e-3)
        message <- paste0("Ooops, water is leaking!\n`", mtrx[r2, ]$name, "` row does not sum to zero. Please make sure that the transactions-flow matrix is written consistently with the model equations.")
        rlang::abort(message = message)
      }

      if (isFALSE(.all_equal(c1))) {
        c2 <- which(abs(c1) > 1e-3)
        message <- paste0("Ooops, water is leaking!\n`", mtrx[, c2]$name, "` column does not sum to zero. Please make sure that the transactions-flow matrix is written consistently with the model equations.")
        rlang::abort(message = message)
      }
    }

  }

  if (which == "bs") {
    for (i in l1) {
      for (j in ids) {
        k3[[j]] <- eval(str2expression(k2[[j]]))
      }

      r1 <- rowSums(k3[, -ncol(k3)], na.rm = T)
      rs <- k3[, ncol(k3)]
      c1 <- colSums(k3, na.rm = T)

      if (isFALSE(all(.is_equal(r1, rs)))) {
        r2 <- which(isFALSE(.is_equal(r1, rs)))
        message <- paste0("Ooops, water is leaking!\n`", mtrx[, r2]$name, "` row is not equal to expected sum. Please make sure that the balance-sheet matrix is written consistently with the model equations.")
        rlang::abort(message = message)
      }

      if (isFALSE(.all_equal(c1))) {
        c2 <- which(abs(c1) > 1e-3)
        message <- paste0("Oooops, water is leaking!\n`", mtrx[, c2]$name, "` column does not sum to zero. Please make sure that the balance-sheet matrix is written consistently with the model equations.")
        rlang::abort(message = message)
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
#' @param which Is it a transactions-flow or a balance-sheet matrix?
#'
#' @export
sfcr_validate <- function(matrix, baseline, which = "tfm") {

  match.arg(which, c("tfm", "bs"))

  bl1 <- attr(baseline, "calls")
  bl2 <- attr(baseline, "external")
  m <- attr(baseline, "matrix")

  # If TFM

  if (which == "tfm") {
    tfm <- .get_matrix(matrix, baseline, bl1, bl2)

    .validate_matrix(tfm, m, "tfm")

    cat("Water tight! The transactions-flow matrix is consistent with the simulated model.")
  }

  else {
    bs <- .get_matrix(matrix, baseline, bl1, bl2)

    .validate_matrix(bs, m, "bs")

    cat("Water tight! The balance-sheet matrix is consistent with the simulated model.")
  }


}
