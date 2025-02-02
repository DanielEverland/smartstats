% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KasperFuncs.R
\name{criticalval}
\alias{criticalval}
\title{The critical value}
\usage{
criticalval(x, alpha = 0.05)
}
\arguments{
\item{x}{Vector of data}

\item{alpha}{Significance level (0.05 by default)}
}
\value{
The critical value for comparing to the test stastic t_obs
}
\description{
Method 3.32 \cr
reject null hypothesis if |tobs1(x, mu)| > criticalval(x, alpha)
}
