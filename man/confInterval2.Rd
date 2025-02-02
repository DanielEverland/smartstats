% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KasperFuncs.R
\name{confInterval2}
\alias{confInterval2}
\title{Two sample confidence interval for mean1 - mean2}
\usage{
confInterval2(x, y, alpha = 0.05)
}
\arguments{
\item{x}{Vector of data}

\item{y}{Vector of data}

\item{alpha}{Significance level (0.05 by default)}
}
\value{
Confidence interval for the difference in mean of x and y with significance level alpha
}
\description{
or just use t.test(x,y, conf.level=0.95) :)
}
