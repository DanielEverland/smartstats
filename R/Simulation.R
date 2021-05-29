#' @title Approximative error propagation rule
#' @description Calculates the non-linear approximative error propagation rule (Method 4.3)
#' @param dVal Values of the partial derived functions with arguments inserted
#' @param var Variances for the given values
#' @return The error propagation
#' @export
approxErrorPropagation <- function(dVal, var) {
  sum(dVal^2 * var^2)
}