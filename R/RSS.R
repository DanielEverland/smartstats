#' @title  Residual sum of squares
#' @description Input makes a simple linear model lm(y~x)
#' @param y Vector of data
#' @param x Vector of data
#' @return Sum of the residuals of the linear model lm(y~x) squared
#' @export
RSS = function(y, x) {
  D = data.frame()
  fit = lm(y~x)
  sum(resid(fit)^2)
}