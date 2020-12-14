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
