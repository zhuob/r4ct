% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayes-pp-fun.R
\name{predp}
\alias{predp}
\title{Calculate predicative probability density}
\usage{
predp(x, n, nmax, p0, a, b)
}
\arguments{
\item{x}{number of respond}

\item{n}{number of subjects already enrolled}

\item{nmax}{max number of subjects to be enrolled}

\item{p0}{hypothesized response rate}

\item{a, b}{beta priors}
}
\value{
a tibble
}
\description{
Calculate predicative probability density
}
\examples{
predp(x = 3, n = 10, nmax = 30, p0 = 0.3, a = 1, b = 1)
}
