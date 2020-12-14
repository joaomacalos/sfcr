#' Take arguments and make them a row of a tibble
#'
#' @param arg The arguments to transform
#' @param tb The Tibble that will receive the rows
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.args_to_row <- function(arg, tb) {
  purrr::imap_dfr(arg, ~dplyr::mutate(tb, !!.y := .x)) %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), ~dplyr::first(.x[!is.na(.x)]))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, "")))
}


#' Abort if typo on the codes of columns
#'
#' @param nms Incorrect codes detected
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.abort_typo_code <- function(nms) {

  if (length(nms) == 1) {
    message = paste0("There's a mispelled code in row `", nms, "`. Please check the matrix and try again.")
  } else {
    message = paste0("There are mispelled codes in rows `", paste0(nms, collapse = ", "), "`. Please check the matrix and try again.")
  }
  rlang::abort(message)
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
#' @author João Macalós, \email{joaomacalos@@gmail.com}
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

  # Check if there's a mispelled code
  is_a_problem <- purrr::map_lgl(colnames(tb2), ~is.na(.x))

  if (any(is_a_problem)) {
    prob_column <- which(is_a_problem)

    prob_row <- purrr::map_dbl(purrr::map(tb2[, prob_column], ~!is.na(.x)), which)

    nm_prob_row <- tb2[prob_row, ]$name

    .abort_typo_code(nm_prob_row)
  }


  return(tb2)
}


