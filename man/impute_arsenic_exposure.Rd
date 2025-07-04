% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputation.R
\name{impute_arsenic_exposure}
\alias{impute_arsenic_exposure}
\title{Impute Arsenic Exposure Levels}
\usage{
impute_arsenic_exposure(
  datmatx_usgs,
  rasterprob_combined,
  cellpop,
  births,
  ndraws,
  as_cat_label,
  geoid_col,
  birth_county_col,
  as_level_col,
  apply_imputation_fallback
)
}
\arguments{
\item{datmatx_usgs}{Data frame containing USGS data with geographic
identifiers}

\item{rasterprob_combined}{Matrix of combined probability distributions}

\item{cellpop}{Vector of cell population weights}

\item{births}{Data frame containing birth records}

\item{ndraws}{Number of imputed datasets to create}

\item{as_cat_label}{Character vector of arsenic category labels}

\item{geoid_col}{Column name for geographic identifier in USGS data}

\item{birth_county_col}{Column name for county identifier in birth data}

\item{as_level_col}{Column name for arsenic level variable}

\item{apply_imputation_fallback}{Logical indicating whether to apply fallback
imputation for missing arsenic levels. The fallback fills missing
values with the most common category in the respective dataset. If
enabled, this fallback will be applied to any missing arsenic levels
after the initial imputation step. Otherwise, it will remove any rows
with missing arsenic levels.}
}
\value{
List of imputed datasets
}
\description{
Internal function to create multiple imputed datasets with arsenic
exposure levels based on county-level probability distributions.
}
\keyword{internal}
