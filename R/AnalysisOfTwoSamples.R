#' @title Raw tobs of two samples
#' @description Calculates the tobs for two samples using raw data
#' @param m Vector of 2 means
#' @param v Vector of 2 variances
#' @param n Vector of 2 sample sizes
#' @param mu0 The null hypothesis mean value
#' @return The t-obs value
#' @export
rawTobs2 = function(m, v, n, mu0 = 0) {
  ((m[1]-m[2])-mu0)/sqrt(v[1]/n[1]+v[2]/n[2])
}

#' @title Raw two sample p-value
#' @description Calculates the p-val for two samples using raw data
#' @param m Vector of 2 means
#' @param v Vector of 2 variances
#' @param n Vector of 2 sample sizes
#' @return The t-test p-value
#' @export
rawPval2 = function(m, v, n) {
  2 * (1- pt(tobs2Raw(m, v, n), dof2Raw(v, n)))
}

#' @title Two sample confidence interval for mean1 - mean2
#' @description Calculates the confidence interval for two samples using raw data
#' @param m Vector of 2 means
#' @param v Vector of 2 variances
#' @param n Vector of 2 sample sizes
#' @param alpha Significance level (0.05 by default)
#' @return Confidence interval for the difference in mean of x and y with significance level alpha
#' @export
rawConfInterval2 = function(m, v, n, alpha = 0.05) {
  xbar = m[1]
  ybar = m[2]
  s1 = v[1]
  s2 = v[2]
  n1 = n[1]
  n2 = n[2]
  xbar - ybar+c(-1,1)*qt(1-alpha/2, df = rawdf2(v, n))*sqrt((s1/n1)+(s2/n2))
}
