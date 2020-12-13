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
      dplyr::mutate(dplyr::across(ncol(latex), ~dplyr::if_else(stringr::str_length(.x) == 0, "$0$", .x))) %>%
      purrr::set_names(c("", nms_latex[2:(length(nms_latex) - 1)], "$\\sum$"))
  }


  kableExtra::kbl(latex,
                  format = "html",
                  table.attr = "style = \"color: black;\""
  ) %>%
    kableExtra::kable_styling(c("striped", "hover"), full_width = FALSE)

}
