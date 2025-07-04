% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-loading.R
\name{create_weighted_prob_matrix}
\alias{create_weighted_prob_matrix}
\title{Create Weighted Probability Matrix}
\usage{
create_weighted_prob_matrix(
  rasterprob_usgs,
  rasterprob_epa,
  datmatx_epa,
  epa_pwell_col
)
}
\arguments{
\item{rasterprob_usgs}{USGS probability matrix}

\item{rasterprob_epa}{EPA probability matrix}

\item{datmatx_epa}{EPA data frame}

\item{epa_pwell_col}{Column name for private well percentage}
}
\value{
Combined weighted probability matrix
}
\description{
Create Weighted Probability Matrix
}
\keyword{internal}
