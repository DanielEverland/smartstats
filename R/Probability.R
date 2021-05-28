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
  n * ((a*(N - a)/N^2)) * ((N - n)/(N - 1))
}

#' Calculates the mean of a binomial distributed random variable
#' 
#' @param n number of independent draws
#' @param p probability of success
#' @export
binomMean <- function(n, p) {
  n * p
}

#' Calculates the variance of a binomial distributed random variable
#' 
#' @param n number of independent draws
#' @param p probability of success
#' @export
binomVar <- function(n, p) {
  n * p * (1 - p)
}