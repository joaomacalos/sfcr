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
#' @note This function can be used to generate a transactions-
#' flow matrix as well as a balance-sheet matrix. If the user
#' wishes to validate these matrices with the simulated data,
#' please pay attention to the following details:
#'
#' * Transactions-flow Matrix:
#'   In the transactions-flow matrix, the \code{sum} column is
#'   going to be generated automatically by the validation
#'   function. Please do not add it by hand.
#'
#' * Balance-sheet Matrix:
#'   In the balance-sheet matrix, it might be the case that some
#'   rows do not sum to zero. Therefore, the user must supply
#'   by hand the non-zero values of the \code{sum} column.
#'   This column should always be the last column of the matrix
#'   and should always be named as "Sum". If there's no column
#'   named as "Sum", it will be generated automatically by the
#'   validation function with all entries equal to zero.
#'
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

  ar <- rlang::list2(...)
  ar <- purrr::map(ar, ~c(.x[-1], name = .x[1]))

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
