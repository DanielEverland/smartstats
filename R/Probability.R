#' Calculates the mean of a hypergeometric distributed variable
#' 
#' @param a number of successes
#' @param N population size
#' @param n amount of draws
#' @export
hyperMean <- function(a, N, n) {
  n * a/N
}

#' Calculates the variance of a hypergeometric distributed variable
#' 
#' @param a number of successes
#' @param N population size
#' @param n amount of draws
#' @export
hyperVar <- function(a, N, n) {
  n * ((a(N - a)/N^2)) * ((N - n)/(N - 1))
}