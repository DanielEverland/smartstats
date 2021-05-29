#' @title  Residual sum of squares
#' @description Input makes a simple linear model lm(y~x) \cr
#' Equation 5-8
#' @param y Vector of data
#' @param x Vector of data
#' @return Sum of the residuals of the linear model lm(y~x) squared
#' @export
RSS = function(y, x) {
  D = data.frame()
  fit = lm(y~x)
  sum(resid(fit)^2)
}

#' @title Variance of estimators
#' @description Simple linear regression variance of estimators beta0 and beta1
#' @param RSS Residual sum of squares
#' @param n length of vector
#' @return The variance of estimators
#' @export
sigmaSquare = function(RSS, n) {
  RSS/(n-2)
}

#' @title Variation of estimator beta0
#' @description Theorem 5.8
#' @param x Vector of data
#' @param RSS Residual sum of squares
#' @return The variation of estimator beta0
#' @export
sigmaBeta0 = function(x, RSS) {
  n = length(x)
  sigma = sigmaSquare(RSS, n)
  sxx = Sxx(x)
  sqrt((sigma/n)+(mean(x)^2*sigma)/sxx)
}

#' @title Variation of estimator beta1
#' @description Theorem 5.8
#' @param x Vector of data
#' @param RSS Residual sum of squares
#' @return The variation of estimator beta1
#' @export
sigmaBeta1 = function(x, RSS) {
  sigma = sigmaSquare(RSS, n)
  sxx = Sxx(x)
  sqrt(sigma/sxx)
}

#' @title Test statistic tobs for estimator beta
#' @description Null hypothesis test (betaI = beta0I) \cr
#' Theorem 5.12
#' @param betaI Estimator betaI
#' @param beta0I Null hypothesis betaI = beta0I
#' @param sigmabetaI Variation of betaI
#' @return The test statistic tobs for beta_i
#' @export
tobsBeta = function(betaI, beta0I, sigmabetaI) {
  (betaI-beta0I)/sigmabetaI
}


#' @title p-value for simple linear regression
#' @description Method 5.14 \cr
#' or just use confint(fit, level=0.95)
#' @param tobs The test statistic tobs for estimator beta
#' @param n length of vector
#' @return p-value 
#' @export
pvalBeta = function(tobs, n) {
  1-(2*pt(abs(tobs), df = n - 2))
}

#' @title Parameter estimator confidence interval
#' @description Method 5.15
#' @param beta Estimator parameter beta
#' @param sigmabeta Variance of estimator beta 
#' @param n length of vector
#' @param alpha Significance level (0.05 by default)
#' @return Confidence interval for estimator parameter
#' @export
paraConfInt = function(beta, sigmabeta, n, alpha = 0.05) {
  beta + c(-1,1)*qt(1-alpha/2, df = n - 2)*sigmabeta
}

#' @title Confidence interval for line
#' @description The line beta0 + beta1*xnew  \cr
#' Method 5.18
#' @param fit linear model 
#' @param xnew new data
#' @param alpha Significance level (0.05 by default)
#' @return Confidence interval for new data 
#' @export
confIntLine = function(fit, xnew, alpha = 0.05) {
  predict(fit, newdata=data.frame(x=xnew), interval="confidence", level=1-alpha)  
}

#' @title Prediction interval for line
#' @description The line beta0 + beta1*xnew  \cr
#' Method 5.18
#' @param fit linear model 
#' @param xnew new data
#' @param alpha Significance level (0.05 by default)
#' @return Prediction interval for new data 
#' @export
predIntLine = function(fit, xnew, alpha = 0.05) {
  predict(fit, newdata=data.frame(x=xnew), interval="prediction", level=1-alpha)  
}




