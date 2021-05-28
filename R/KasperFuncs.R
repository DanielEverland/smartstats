#' @title The one-sample t-test statistic
#' @description Method 3.23 \cr
#' or just do t.test(x, conf.level=0.95) :)
#' @param x Vector of data
#' @param mu mean for the null hypothesis
#' @return The test statistic t_obs
#' @export
tobs1 = function(x, mu = 0) {
  n = length(x)
  val = (mean(x) - mu) / (sd(x) / sqrt(n))
  return(val)
}
#' @title The one-sample t-test p-value
#' @description Method 3.23 \cr
#' or just do t.test(x, conf.level=0.95) :)
#' @param x Vector of data
#' @param mu mean for the null hypothesis
#' @return The t-test p-value
#' @export
pval1 = function(x, mu = 0) {
  n = length(x)
  tval = tobs1(x, mu)
  pval = 2 * (1-pt(abs(tval), df = n-1))
  return(pval)
}


#' @title The critical value
#' @description Method 3.32 \cr
#' reject null hypothesis if |tobs1(x, mu)| > criticalval(x, alpha)
#' @param x Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return The critical value for comparing to the test stastic t_obs
#' @export
criticalval = function(x, alpha = 0.05) {
  n = length(x)
  qt(1-alpha/2, df = n-1)
}

#' @title Two sample confidence interval for mean1 - mean2
#' @description or just use t.test(x,y, conf.level=0.95) :)
#' @param x Vector of data
#' @param y Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return Confidence interval for the difference in mean of x and y with significance level alpha
#' @export
confInterval2 = function(x, y, alpha = 0.05) {
  xbar = mean(x)
  ybar = mean(y)
  s1 = var(x)
  s2 = var(y)
  n1 = length(x)
  n2 = length(y)
  xbar - ybar+c(-1,1)*qt(1-alpha/2, df = df2(x, y))*sqrt((s1/n1)+(s2/n2))
}

#' @title Two sample t_obs value
#' @description Method 3.51 \cr
#' or just use t.test(x,y, conf.level=0.95) :)
#' @param x Vector of data
#' @param y Vector of data
#' @param mu0 The null hypothesis mean value
#' @return The test statistic t_obs for two sample t-test
#' @export
tobs2 = function(x, y, mu0 = 0) {
  ms = c(mean(x), mean(y))
  vs = c(var(x), var(y))
  ns = c(length(x), length(y))
  tobs = ((ms[1]-ms[2]) - mu0)/sqrt(vs[1]/ns[1]+vs[2]/ns[2])
  return(tobs)
}

#' @title Two sample degrees of freedom
#' @description  Method 3.51
#' @param x Vector of data
#' @param y Vector of data
#' @return The v degrees of freedom for two sample
df2 = function(x, y) {
  ms = c(mean(x), mean(y))
  vs = c(var(x), var(y))
  ns = c(length(x), length(y))
  v = ((vs[1]/ns[1]+vs[2]/ns[2])^2)/((vs[1]/ns[1])^2/(ns[1]-1)+(vs[2]/ns[2])^2/(ns[2]-1))
  return(v)
}

#' @title Two sample p-value
#' @description or just do t.test(x,y, conf.level = 0.95) :)
#' @param x Vector of data
#' @param y Vector of data
#' @param mu0 The null hypothesis mean value
#' @return The t-test p-value
#' @export
pval2 = function(x, y, mu0) {
  1 - pt(tobs2(x,y, mu0), df2(x,y))
}


#' @title Standard deviation interval
#' @description Method 3.19
#' @param x Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return The confidence interval for the standard deviation
#' @export
sdInterval = function(x, alpha = 0.05) {
  n = length(x)
  sdInt = c(sqrt((n-1)*var(x)/qchisq(1-alpha/2,n-1)),sqrt((n-1)*var(x)/qchisq(alpha/2,n-1)))
  return(sdInt)
}


#' @title Variance interval
#' @description Method 3.19
#' @param x Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return The confidence interval for the variation deviation
#' @export
varInterval = function(x, alpha = 0.05) {
  n = length(x)
  varInt = c((n-1)*var(x)/qchisq(1-alpha/2,n-1),(n-1)*var(x)/qchisq(alpha/2,n-1))
  #sdInterval(x, alpha)^2
  return(varInt)
}

#' @title Confidence interval
#' @description Method 3.9 \cr
#' or do t.test(x, conf.level = 0.95)
#' @param x Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return The one sample confidence interval for mean
#' @export
confInterval = function(x, alpha = 0.05){
  n = length(x)
  xbar = mean(x)
  s=sd(x)
  xbar+c(-1,1)*(qt(1-alpha/2,n-1)*(s/sqrt(n)))
}

#' Q0, or the minimum value
#' @param x Vector of data
#' @return The minimum
#' @export
Q0 = function(x) {
  quantile(x,0, type=2)
}

#' Q1, or the lower quartile
#' @param x Vector of data
#' @return The lower quartile Q1
#' @export
Q1 = function(x) {
  quantile(x,0.25, type=2)
}

#' Q2, or the median
#' @param x Vector of data
#' @return The middle quartile Q2 (or median)
#' @export
Q2 = function(x) {
  quantile(x,0.5, type=2)
}

#' Q3, or the upper quartile
#' @param x Vector of data
#' @return The upper quartile Q3
#' @export
Q3 = function(x) {
  quantile(x,0.75, type=2)
}

#' Q4, or the maximum value
#' @param x Vector of data
#' @return The maximum
#' @export
Q4 = function(x) {
  quantile(x,1, type=2)
}

#' @title The sample correct sum of squares
#' @description Theorem 5.4
#' @param x Vector of data
#' @return The sum of squares of the difference between x and its mean
#' @export
Sxx = function(x){
  sum((x-mean(x))^2)
}

#' @title Simple linear regression estimator beta1
#' @description Theorem 5.4
#' @param x Vector of data
#' @param y Vector of data
#' @return The estimator beta1 hat
#' @export
beta1hat = function(x, y) {
  sum((x - mean(x))*(y - mean(y))) / Sxx(x)
}

#' @title Simple linear regression estimator beta0
#' @description Theorem 5.4
#' @param x Vector of data
#' @param y Vector of data
#' @return The estimator beta0 hat
#' @export
beta0hat = function(x, y) {
  mean(y) - b1hat(x,y) * mean(x)
}

#' @title Simple linear regression analysis
#' @description Linear model based on y ~ x
#' @param y Vector of data
#' @param x Vector of data
#' @return Summary of the linear model given by y ~ x
#' @export
linearAnal = function(y, x) {
  #teehee
  D = data.frame(x=x, y=y)
  fit = lm(y ~ x, data=D)
  summary(fit)
}

#' @title Total sum of squares
#' @description Theorem 8.2
#' @param x Vector of data
#' @return The total sum of squares
#' @export
SST = function(x) {
  sum((x-mean(x))^2)
}

#' @title Residual sum of squares
#' @description Theorem 8.2 and 8.20 \cr
#' One- and Two-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to (Null if One-way ANOVA)
#' @return The sum of squared errors
#' @export
SSE = function(x, treatments, blocks = NULL) {
  factor(treatments)
  numOfGroups = length(unique(treatments))
  D = data.frame()
  
  if (is.null(blocks)) {
    muis = tapply(x, treatments, mean)
    for (i in 1:numOfGroups) {
      D = rbind(D, sum((x[treatments==i]-muis[i])^2))
    }
    return(sum(D))
  }
  else {
    factor(blocks)
    sst = SST(x)
    sstr = SSTr(x, treatments)
    ssbl = SSBl(x, treatments, blocks)
    return(sst-sstr-ssbl)
  }
}

#' @title Block sum of squares
#' @description Theorem 8.20 \cr
#' Two-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to
#' @return The block sum of squared
#' @export
SSBl = function(x, treatments, blocks) {
  mu = mean(x)
  factor(treatments)
  factor(blocks)
  numOfGroups = length(unique(treatments))
  beta = tapply(x, blocks, mean) - mu
  numOfGroups*sum(beta^2)
}

#' @title Treatment sum of squares
#' @description Theorem 8.2 \cr
#' ANOVA 
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @return The treatment sum of squares
#' @export
SSTr = function(x, treatments) {
  factor(treatments)
  mu = mean(x)
  numOfGroups = length(unique(treatments))
  muis = tapply(x, treatments, mean)
  alpha = muis - mu
  numOfObsGroup = as.data.frame(table(treatments))$Freq
  sum(numOfObsGroup*alpha^2)
}

#' @title Mean treatment sum of squares
#' @description ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @return The mean treatment sum of squares
#' @export
MSTr = function(x, treatments) {
  numOfGroups = length(unique(treatments))
  SSTr(x, treatments)/(numOfGroups-1)
}

#' @title Mean residual sum of squares (errors)
#' @description One- and Two-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to (Null if One-way ANOVA)
#' @return The mean sum of square errors
#' @export
MSE = function(x, treatments, blocks = NULL) {
  numOfGroups = length(unique(treatments))
  n = length(x)
  if (is.null(blocks)) {
    SSE(x, treatments)/(n-numOfGroups)
  }
  else {
    factor(blocks)
    numOfBlocks = length(unique(blocks))
    sse = SSE(x, treatments, blocks)
    sse/((numOfGroups-1)*(numOfBlocks-1))
  }
}

#' @title Mean block sum of squares
#' @description Two-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to (Null if One-way ANOVA)
#' @return The mean block sum of squares
#' @export
MSBl = function(x, treatments, blocks) {
  numOfBlocks = length(unique(blocks))
  SSBl(x, treatments, blocks)/(numOfBlocks-1)
}

#' @title Calculate the test statistic F_obs
#' @description Theorem 8.6 \cr
#' One-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @return The test statistic F_obs
#' @export
Fobs = function(x, treatments) {
  MSTr(x, treatments)/MSE(x, treatments)
}

#' @title Calculate the p-value
#' @description For One-way ANOVA 
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @return The test p-value
#' @export
pvalANOVA = function(x, treatments) {
  numOfGroups = length(unique(treatments))
  n = length(x)
  1- pf(Fobs(x, treatments), df1 = numOfGroups - 1, df2 = n - numOfGroups)
}

#' @title Treatment test statistic Ftr
#' @description Theorem 8.22 \cr
#' Two-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to
#' @return The test statistic F for treatments
#' @export
FTr = function(x, treatments, blocks) {
  MSTr(x, treatments)/MSE(x, treatments, blocks)
}

#' @title Block test statistic Ftr
#' @description Theorem 8.22 \cr
#' Two-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to
#' @return The test statistic F for blocks
#' @export
FBl = function(x, treatments, blocks) {
  MSBl(x, treatments, blocks)/MSE(x, treatments,blocks)
}

#' @title Calculating p-value for treatments
#' @description For Two-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to
#' @return The p-value for treatments
#' @export
pvalTr = function(x, treatments, blocks) {
  numOfGroups = length(unique(treatments))
  numOfBlocks = length(unique(blocks))
  1 - pf(FTr(x,treatments, blocks), df1 = numOfGroups - 1, df2 = (numOfGroups - 1)*(numOfBlocks - 1))
}

#' @title Calculating p-value for blocks
#' @description For Two-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to
#' @return The p-value for blocks
#' @export
pvalBl = function(x, treatments, blocks) {
  numOfGroups = length(unique(treatments))
  numOfBlocks = length(unique(blocks))
  1 - pf(FBl(x,treatments, blocks), df1 = numOfBlocks - 1, df2 = (numOfGroups - 1)*(numOfBlocks - 1))
}


#' @title Post Hoc pairwise confidence interval
#' @description Method 8.9 \cr
#' For One- and Two-way ANOVA
#' @param x Vector of data
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param groupIndex1 Index of group to compare
#' @param groupIndex2 Index of group to compare 
#' @param blocks Vector with pointers for which block each entry in x belong to (NULL if One-way ANOVA)
#' @param alpha Significance level (0.05 by default)
#' @return Confidence interval for Post Hoc pairwise comparison
#' @export
postHocConfInt = function(x, treatments, groupIndex1, groupIndex2, blocks = NULL, alpha = 0.05){
  numOfGroups = length(unique(treatments))
  muis = tapply(x, treatments, mean)
  numOfObsGroup1 = as.data.frame(table(treatments))$Freq[groupIndex1]
  numOfObsGroup2 = as.data.frame(table(treatments))$Freq[groupIndex2]
  if (is.null(blocks)) {
    n = length(x)
    muis[groupIndex1] - muis[groupIndex2] + c(-1,1)*qt(1-alpha/2, df=n-numOfGroups)*sqrt(MSE(x, treatments)*(1/numOfObsGroup1 + 1/numOfObsGroup2))
  }
  else {
    numOfBlocks = length(unique(blocks))
    muis[groupIndex1] - muis[groupIndex2] + c(-1,1)*qt(1-alpha/2, df=(numOfGroups-1)*(numOfBlocks-1))*sqrt(MSE(x, treatments, blocks)*(1/numOfObsGroup1 + 1/numOfObsGroup2))
  }
}


#' @title Post hoc pairwise hypothesis test t-value
#' @description Method 8.10 \cr
#' For One- and Two-way ANOVA
#' @param x Vector of data
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param groupIndex1 Index of group to compare
#' @param groupIndex2 Index of group to compare 
#' @param alpha Significance level (0.05 by default)
#' @return The test statistic t_obs for Post Hoc pairwise hypothesis test
#' @export
tobsPostHoc = function(x, treatments, groupIndex1, groupIndex2, blocks = NULL, alpha = 0.05){
  numOfGroups = length(unique(treatments))
  muis = tapply(x, treatments, mean)
  numOfObsGroup1 = as.data.frame(table(treatments))$Freq[groupIndex1]
  numOfObsGroup2 = as.data.frame(table(treatments))$Freq[groupIndex2]
  if (is.null(blocks)) {
    n = length(x)
    (muis[groupIndex1] - muis[groupIndex2])/sqrt(MSE(x, treatments)*(1/numOfObsGroup1 + 1/numOfObsGroup2))
  }
  else {
    (muis[groupIndex1] - muis[groupIndex2])/sqrt(MSE(x, treatments, blocks)*(1/numOfObsGroup1 + 1/numOfObsGroup2))
  }
}

#' @title Post hoc pairwise hypothesis test p-value for One- and Two-way ANOVA
#' @description Method 8.10
#' @param x Vector of data
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param groupIndex1 Index of group to compare
#' @param groupIndex2 Index of group to compare 
#' @param alpha Significance level (0.05 by default)
#' @return The p-value for post hoc pairwise hypothesis test
#' @export
pvalPostHoc = function(x, treatments, groupIndex1, groupIndex2, blocks = NULL, alpha = 0.05) {
  if(is.null(blocks)) {
    n = length(x)
    tobs = tobsPostHoc(x, treatments, groupIndex1, groupIndex2, alpha)
    2*(1-pt(abs(tobs), n - numOfGroups))
  }
  else {
    numOfBlocks = length(unique(blocks))
    numOfGroups = length(unique(treatments))
    tobs = tobsPostHoc(x, treatments, groupIndex1, groupIndex2, blocks, alpha)
    2 * (1-pt(abs(tobs), df = (numOfGroups - 1)*(numOfBlocks - 1)))
  }
}

#' @title The bonferroni corrected alpha
#' @param numOfGroups number of groups
#' @param alpha Significance level (0.05 by default)
#' @return The bonferroni corrected significance level
#' @export
bonferroni = function(numOfGroups, alpha = 0.05) {
  M = numOfGroups*(numOfGroups-1)/2
  alpha/M
}

#' @title Create a wallyplot
#' @description 
#' Wally plot is a guessing game where your data is compared with 8 normally distributed data. \cr
#' Pick the data that deviates most from the line, if it is not yours then your data is assumed to be normally distributed \cr
#' You cannot conclude anything from wallyplot, it is merely a tool 
#' @param x Vector of data to plot
#' @param range1 y-axis plot range lower value
#' @param range2 y-axis plot range upper value 
#' @return Wallyplot of the data compared with 8 normally distributed data
#' @export
wally = function(x, range1, range2) {
  library(MESS)
  qqwrap <- function(x, y, ...){
    stdy <- (y-mean(y))/sd(y)
    qqnorm(stdy, main="", ...)
    qqline(stdy)
  }
  wallyplot(x, FUN=qqwrap, ylim=c(range1,range2))
  
}

