#' Find names for display matrix
#'
#' Clean cells to display in latex format
#'
#' @param matrix A balance-sheet or transactions-flow matrix
#'
#' @author João Macalós
#'
#' @keyword Internal
#'
.find_names <- function(matrix) {
  p1 <- matrix %>%
    dplyr::mutate(dplyr::across(-1, ~gsub("\\+|\\-|\\*|\\/|\\(|\\)|\\[-1\\]", " ", .x))) %>%
    dplyr::mutate(dplyr::across(-1, ~stringr::str_squish(.x))) %>%
    dplyr::mutate(dplyr::across(-1, ~stringr::str_split(.x, " "))) %>%
    dplyr::mutate(dplyr::across(-1, ~purrr::map(.x, function(y) unique(y))))

  nms <- unlist(p1[, -1])

  nms[purrr::map_lgl(nms, ~!(.x %in% c("", "0")))]
}


#' Transform entries into latex style
#'
#' @param matrix a balance-sheet or transactions-flow matrix
#' @param nms Cleaned names with \code{.find_names()}
#'
#' @author João Macalós
#'
#' @keyword Internal
#'
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
    dplyr::mutate(dplyr::across(-1, ~stringr::str_replace_all(.x, "d\\((.*?)\\)", "\\(\\1 - \\1\\[-1\\]\\)"))) %>%
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
#' @param which A character string for the matrix. Is it a balance-sheet or
#' a transactions-flow matrix? here are two options:
#' \code{"bs"} for balance-sheet matrices, and \code{"tfm"} for transactions-
#' flow matrices. The default is \code{"tfm"}.
#'
#' @details This function takes a matrix as input and generate a \code{kableExtra}
#' table with math symbols displayed in latex style.
#'
#' @note This function converts the math expressions used to build the \code{sfcr_matrix}
#' into a latex format, but cannot add modifications to it. The user is
#' invited to explore the source code and the \code{kableExtra} package in order to
#' personalize his/her own matrices.
#'
#' @example inst/examples/example_sfcr_matrix_display.R
#'
#' @author João Macalós
#'
#'
#' @export
sfcr_matrix_display <- function(matrix, which = "tfm") {
  match.arg(which, c("tfm", "bs"))

  # if (!requireNamespace("kableExtra", quietly = TRUE)) {
  #   stop("Packages \"kableExtra\" needed for this function to work. Please install it.",
  #        call. = FALSE)
  # }

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
      dplyr::mutate(dplyr::across(ncol(latex), ~dplyr::if_else(stringr::str_length(.x) == 0, "$0$", .x))) %>%
      purrr::set_names(c("", nms_latex[2:(length(nms_latex) - 1)], "$\\sum$"))
  }


  kableExtra::kbl(latex,
                  format = "html",
                  table.attr = "style = \"color: black;\""
  ) %>%
    kableExtra::kable_styling(c("striped", "hover"), full_width = FALSE)

}
