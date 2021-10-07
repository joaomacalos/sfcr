#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Abort if not
#'
#' @param cnd Condition to be evaluated.
#' @param message Message to display if condition is false.
#'
#' @author João Macalós
#'
#' @keywords internal
#'
abortifnot <- function(cnd, message = NULL) {

  if (isFALSE(cnd)) rlang::abort(message)

}

#'
#' @keywords internal
#'
release_questions <- function() {
  c(
    "Have you check that all models are running without errors? The best way to check it is to \n
    wait for Travis to build and check the package's webpage. If there's an error in any model,\n
    the page will return a 404 error."
  )
}
