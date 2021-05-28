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

#' Calculates the variance confidence interval without a vector
#' 
#' @param var Given variance
#' @param len Length of the vector
#' @param alpha Significance level
#' @return The variance confidence interval
#' @export
rawVarInterval <- function(var, len, alpha = 0.05) {
  c((len - 1)*var/qchisq(1-(alpha/2), (len - 1)), (len - 1)*var/qchisq(alpha/2, (len - 1)))
}

#' Calculates the standard deviation confidence interval without a vector
#' 
#' @param sd Given standard deviation
#' @param len Length of the vector
#' @param alpha Significance level
#' @return The standard deviation confidence interval
#' @export
rawSdInterval <- function(sd, len, alpha = 0.05) {
  sqrt(rawVarInterval(sd^2, len, alpha))
}

#' Calculates the mean confidence interval without a vector
#' 
#' @param mu Given mean
#' @param sd Standard deviation
#' @param len Length of the vector
#' @param alpha Significance level
#' @return The mean confidence interval
#' @export
rawMeanInterval <- function(mu, sd, len, alpha = 0.05) {
  mu + c(-1, 1) * qt(1-alpha, len - 1) * (sd / sqrt(len))
}