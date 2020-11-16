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
#' @export
#'
`%nin%` <- function(x,y) !(`%in%`(x,y))
