% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnalysisOfTwoSamples.R
\name{rawConfInterval2}
\alias{rawConfInterval2}
\title{Two sample confidence interval for mean1 - mean2}
\usage{
rawConfInterval2(m, v, n, alpha = 0.05)
}
\arguments{
\item{m}{Vector of 2 means}

\item{v}{Vector of 2 variances}

\item{n}{Vector of 2 sample sizes}

\item{alpha}{Significance level (0.05 by default)}
}
\value{
Confidence interval for the difference in mean of x and y with significance level alpha
}
\description{
Calculates the confidence interval for two samples using raw data
}
