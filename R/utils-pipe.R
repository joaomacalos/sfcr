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



#' Not in operator
#'
#' @name %nin%
#'
#' @param x,y vectors for comparison
#'
`%nin%` <- function(x,y) !(`%in%`(x,y))

#' Abort if not
#'
#' @param cnd Condition to be evaluated.
#' @param message Message to display if condition is false.
abortifnot <- function(cnd, message = NULL) {

  if (isFALSE(cnd)) rlang::abort(message)

}

.round <- function(tbl) {
  tbl %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~round(.x, digits = 2)))
}
