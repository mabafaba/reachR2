% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_cleaning_check.R
\name{select_other_columns}
\alias{select_other_columns}
\title{Subset columns that end in "other" or "autre"}
\usage{
select_other_columns(data)
}
\arguments{
\item{data}{data}
}
\value{
data frame subset
`NULL` if `return.stratum.populations` is `FALSE` (default)
A named vector with the population counts per stratum, if `return.stratum.populations` is set to `TRUE`
}
\description{
Subset columns that end in "other" or "autre"
}
