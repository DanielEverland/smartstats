% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KasperFuncs.R
\name{pvalTr}
\alias{pvalTr}
\title{Calculating p-value for treatments}
\usage{
pvalTr(x, treatments, blocks)
}
\arguments{
\item{x}{Vector of all data from all groups}

\item{treatments}{Vector with pointers for which group each entry in x belong to.}

\item{blocks}{Vector with pointers for which block each entry in x belong to}
}
\value{
The p-value for treatments
}
\description{
For Two-way ANOVA
}
