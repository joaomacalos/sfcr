#' sfcr_set constructor
#'
#' @param list A list
new_sfcr_set <- function(list) {
  stopifnot(inherits(list, "list"))

  if (mean(vapply(list, rlang::is_formula, logical(1))) < 1) rlang::abort("Invalid arguments. Please use the R equations syntax to define the formulas.")

  structure(list,
            class = c("sfcr_set", "list"))

}



#' Define the formulas of the model
#'
#' @param ... The formulas used to define the equations and external
#' values of the system
#'
#' @author João Macalós
#'
#' @examples
#' sfcr_set(alpha0 ~ 15, alpha1 ~ 0.8, alpha2 ~ 0.15)
#'
#' @export
#'
sfcr_set <- function(...) {

  tryCatch(
    error = function(cnd) rlang::abort("Please use the R equations syntax to define the formulas."),
    formulas <- list(...)
  )

  formulas <- new_sfcr_set(formulas)

  return(formulas)
}
