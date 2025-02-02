% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KasperFuncs.R
\name{confInterval}
\alias{confInterval}
\title{One sample confidence interval for the mean}
\usage{
confInterval(x, alpha = 0.05)
}
\arguments{
\item{x}{Vector of data}

\item{alpha}{Significance level (0.05 by default)}
}
\value{
The one sample confidence interval for mean
}
\description{
Method 3.9 \cr
or do t.test(x, conf.level = 0.95)
}
