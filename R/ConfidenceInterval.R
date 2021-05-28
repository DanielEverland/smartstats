#' Calculates the sample error of the mean of a set
#' 
#' Definition 3.7
#' 
#' @param x The set to calculate
#' @return The standard error of the mean
#' @export
SEM <- function(x) {
  sd(x)/sqrt(length(x))
}