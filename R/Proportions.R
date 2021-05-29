
#' @title Proportion confidence interval
#' @description Method 7.3 
#' @param x Number of observations in a category
#' @param n Number of observations in total
#' @param alpha Significance level (0.05 by default)
#' @return The proportion confidence interval
#' @export
propConfInt = function(x, n, alpha) {
  p = x/n
  z = qnorm(1-alpha/2)
  p+c(-1,1)*z*sqrt((p*(1-p))/n)
}
  
#' @title The Z-distribution value
#' @description The Z-distribution is a normal distribution with mean zero and standard deviation 1
#' @param alpha Significance level (0.05 by default)
#' @return Z-distribution value
#' @export
zval = function(alpha) {
  qnorm(1-alpha/2)
}

#' @title The test statistic Z_obs
#' @description Method 7.11 \cr
#' Used for hypothesis test \cr
#' Use prop.test(x = x, n = n, p = p0, correct = FALSE, conf.level = 1-alpha) to get p-value
#' @param x Number of observations in a category
#' @param p0 The null hypothesis
#' @param n Number of observations in total
#' @return Z_obs test statistic
#' @export
zobs = function(x, p0, n) {
  top = x-n*p0
  bottom = sqrt(n*p0*(1-p0))
  top/bottom
}

#' @title The test statistic Z_obs for two sample proportions
#' @description Method 7.18 \cr
#' Used for two sample proportions hypothesis test \cr
#' Use prop.test(x = c(x1, x2), n = c(n1,n2), p = c(p1,p2), correct = FALSE, conf.level = 1-alpha) to get p-value
#' @param x Vector of two numbers of observations in a category
#' @param p0 Vector of two number of the null hypothesis
#' @param n Vector of two number of observations in total
#' @return Z_obs test statistic for two sample proportions hypothesis test
#' @export
zobs2 = function(x, p, n) {
  phat = (x[1] + x[2])/(n[1]+n[2])
  top = p[1]-p[2]
  bottom = sqrt(phat*(1-phat)*(1/n[1]+1/n[2]))
  top/bottom
}



