#' Calculates the coefficient of variation (CV)
#' 
#' The CV is the standard deviation seen relative to the mean of a given value
#' 
#' @param val The value to calculate
#' @param digits The amount of digits to print
#' @return A percentage representing the CV of val
#' @export
cv <- function(val, digits = 2) {
  cat(round(sd(val) / mean(val) * 100, digits), '%', sep="")
}