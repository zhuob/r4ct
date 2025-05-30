% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiplicity.R
\name{test_truncated_hochberg}
\alias{test_truncated_hochberg}
\title{Perform truncated Hochberg procedure}
\usage{
test_truncated_hochberg(pvals, truncation_gamma, overall_alpha)
}
\arguments{
\item{pvals}{a vector of p-values to be adjusted for multiplicity}

\item{truncation_gamma}{the truncation parameter between 0 and 1, with 0
being Bonferroni procedure and 1 corresponding to Hochberg procedure. A
larger value yields a larger alpha reserved if one or more test fails}

\item{overall_alpha}{the overall alpha level.}
}
\value{
a tibble including the outcome of each test as well as alpha reserved
}
\description{
Perform truncated Hochberg procedure
}
\examples{
test_truncated_hochberg(c(0.01, 0.026), 0.8, overall_alpha = 0.025)
}
