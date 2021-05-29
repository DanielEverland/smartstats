#' @title Margin of error
#' @description The margin of error, also called half the width of the confidence interval
#' @param x Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return Half the width of the confidence interval
#' @export
ME = function(x, alpha = 0.05) {
  s = sd(x)
  n = length(x)
  qt(1-alpha/2, df = n - 1)*s/sqrt(n)
}

#' @title Margin of error
#' @description The margin of error, also called half the width of the confidence interval \cr
#' Calculated from raw summary statistics
#' @param s Standard deviation of a vector
#' @param n Sample size
#' @param alpha Significance level (0.05 by default)
#' @return Half the width of the confidence interval
#' @export
rawME = function(s, n, alpha = 0.05) {
  qt(1-alpha/2, df = n - 1)*s/sqrt(n)
}


#' @title Confidence interval sample size
#' @description Calculate the sample size needed for a given margin of error \cr
#' Method 3.63.
#' @param ME Margin of error
#' @param s Standard deviation of data
#' @param alpha Significance level (0.05 by default)
#' @return The minimum sample size needed for a given margin of error
#' @export
sampleSizeME = function(ME, s, alpha = 0.05) {
  ((qnorm(1-alpha/2)*s)/ME)^2
}

