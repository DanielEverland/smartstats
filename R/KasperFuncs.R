#' Method 3.23
#' Calculate the one-sample t-test statistic
#' @param x Vector of data
#' @param mu mean for the null hypothesis
#' @return The test statistic t_obs
#' @export
#t_obs for 1 sample
tobs1 = function(x, mu = 0) {
  n = length(x)
  val = (mean(x) - mu) / (sd(x) / sqrt(n))
  return(val)
}

#' Method 3.23
#' Calculate the one-sample t-test p-value
#' or just do t.test(x) :)
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


#' Method 3.32
#' reject if |tobs1(x, mu)| > criticalval(x, alpha)
#' @param x Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return The critical value for comparing to the test stastic t_obs
#' @export
criticalval = function(x, alpha = 0.05) {
  n = length(x)
  qt(1-alpha/2, df = n-1)
}

#' Two sample confidence interval for mean1 - mean2
#' or just use t.test(x,y, conf.level=0.95)
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


#' Method 3.51
#' Two sample t_obs value
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

#' Method 3.51
#' Two sample degrees of freedom
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

#' Two sample p-value
#' or just do t.test(x,y)
#' @param x Vector of data
#' @param y Vector of data
#' @param mu0 The null hypothesis mean value
#' @return The t-test p-value
#' @export
pval2 = function(x, y, mu0) {
  1 - pt(tobs2(x,y, mu0), df2(x,y))
}


#' Method 3.19
#' Standard deviation interval
#' @param x Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return The confidence interval for the standard deviation
#' @export
sdInterval = function(x, alpha = 0.05) {
  n = length(x)
  sdInt = c(sqrt((n-1)*var(x)/qchisq(1-alpha/2,n-1)),sqrt((n-1)*var(x)/qchisq(alpha/2,n-1)))
  return(sdInt)
}


#' Method 3.19
#' Variance interval
#' @param x Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return The confidence interval for the variation deviation
#' @export
sdInterval 
varInterval = function(x, alpha = 0.05) {
  n = length(x)
  varInt = c((n-1)*var(x)/qchisq(1-alpha/2,n-1),(n-1)*var(x)/qchisq(alpha/2,n-1))
  #sdInterval(x, alpha)^2
  return(varInt)
}

#' Method 3.9 
#' or do t.test(x, conf.level = 0.95)
#' @param x Vector of data
#' @param alpha Significance level (0.05 by default)
#' @return The one sample confidence interval for mean
#' @export
confInterval = function(x, alpha = 0.05){
  n = length(x)
  xbar = mean(x)
  s=sd(x)
  c(-1,1)*xbar+(qt(1-alpha/2,n-1)*(s/sqrt(n)))
}

#' 0th qauntile
#' @param x Vector of data
#' @return The minimum
#' @export
Q0 = function(x) {
  quantile(x,0, type=2)
}

#' 25th qauntile
#' @param x Vector of data
#' @return The lower quartile Q1
#' @export
Q1 = function(x) {
  quantile(x,0.25, type=2)
}

#' 50th qauntile
#' @param x Vector of data
#' @return The middle quartile Q2 (or median)
#' @export
Q2 = function(x) {
  quantile(x,0.5, type=2)
}

#' 75th qauntile
#' @param x Vector of data
#' @return The upper quartile Q3
#' @export
Q3 = function(x) {
  quantile(x,0.75, type=2)
}

#' 100th qauntile
#' @param x Vector of data
#' @return The maximum
#' @export
Q4 = function(x) {
  quantile(x,1, type=2)
}

#' Theorem 5.4
#' The sample correct sum of squares
#' @param x Vector of data
#' @return The sum of squares of the difference between x and its mean
#' @export
Sxx = function(x){
  sum((x-mean(x))^2)
  #sxx = (length(x)-1)*var(x)
}

#' Theorem 5.4
#' Linear regression estimator beta1
#' @param x Vector of data
#' @param y Vector of data
#' @return The estimator beta1 hat
#' @export
beta1hat = function(x, y) {
  sum((x - mean(x))*(y - mean(y))) / Sxx(x)
}

#' Theorum 5.4
#' Linear regression estimator beta0
#' @param x Vector of data
#' @param y Vector of data
#' @return The estimator beta0 hat
#' @export
beta0hat = function(x, y) {
  mean(y) - b1hat(x,y) * mean(x)
}

#' Making a simple linear regression analysis
#' y ~ x
#' @param x Vector of data
#' @param y Vector of data
#' @return Summary of the linear model given by y ~ x
#' @export
linearAnal = function(y, x) {
#teehee
  D = data.frame(x=x, y=y)
  fit = lm(y ~ x, data=D)
  summary(fit)
}

#Following functions calculate sum of squares for One-way ANOVA

#' Theorem 8.2
#' One-way ANOVA
#' Can also be calculated as SSTr + SSE
#' @param x Vector of data
#' @return The total sum of squares
#' @export
SST = function(x) {
  sum((x-mean(x))^2)
}

#' Theorem 8.2 and 8.20
#' One-way ANOVA
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

#' Theorem 8.20
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

#' Theorem 8.2
#' One-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to
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


#' One-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to.
#' @return The mean treatment sum of squares
#' @export
MSTr = function(x, treatments) {
  numOfGroups = length(unique(treatments))
  SSTr(x, treatments)/(numOfGroups-1)
}

#' One- and Two-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to (Null if One-way ANOVA)
#' @return The mean sum of squares
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

MSBl = function(x, treatments, blocks) {
  numOfBlocks = length(unique(blocks))
  SSBl(x, treatments, blocks)/(numOfBlocks-1)
}

#' Theorem 8.6
#' One-way ANOVA
#' Calculate the test statistic F_obs
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @return The test statistic F_obs
#' @export
Fobs = function(x, treatments) {
  MSTr(x, treatments)/MSE(x, treatments)
}

#' Calculate the p-value for One-way ANOVA
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to.
#' @return The test p-value
#' @export
pvalANOVA = function(x, treatments) {
  numOfGroups = length(unique(treatments))
  n = length(x)
  1- pf(Fobs(x, treatments), df1 = numOfGroups - 1, df2 = n - numOfGroups)
}

#' Theorem 8.22
#' Two-way ANOVA
#' Treatment test statistic Ftr
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to
#' @return The test statistic F for treatments
#' @export
FTr = function(x, treatments, blocks) {
  MSTr(x, treatments)/MSE(x, treatments, blocks)
}

#' Theorem 8.22
#' Two-way ANOVA
#' Block test statistic Ftr
#' @param x Vector of all data from all groups
#' @param treatments Vector with pointers for which group each entry in x belong to. 
#' @param blocks Vector with pointers for which block each entry in x belong to
#' @return The test statistic F for blocks
#' @export
FBl = function(x, treatments, blocks) {
  MSBl(x, treatments, blocks)/MSE(x, treatments,blocks)
}

#' Calculating p-value for treatments in Two-way ANOVA
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

#' Calculating p-value for blocks in Two-way ANOVA
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


#' Method 8.9
#' Post Hoc pairwise confidence interval for One- and Two-way ANOVA
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


#' Method 8.10
#' Post hoc pairwise hypothesis test t-value for One-way ANOVA
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

#' Method 8.10
#' Post hoc pairwise hypothesis test p-value for One-way ANOVA
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

#' The bonferroni corrected alpha
#' @param numOfGroups number of groups
#' @param alpha Significance level (0.05 by default)
#' @return The bonferroni corrected significance level
#' @export
bonferroni = function(numOfGroups, alpha = 0.05) {
  M = numOfGroups*(numOfGroups-1)/2
  alpha/M
}

#' Wally plot is a guessing game where your data is compared with 8 normally distributed data.
#' Pick the data that deviates most from the line, if it is not yours then your data is assumed to be normally distributed
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
