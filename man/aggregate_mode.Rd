% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_aggregation.R
\name{aggregate_mode}
\alias{aggregate_mode}
\title{Aggregating to mode}
\usage{
aggregate_mode(df, aggregate.by, when.tie = NA, ignore.missing = T,
  write.to.file)
}
\arguments{
\item{df}{the data frame to aggregate}

\item{aggregate.by}{the name of the column to aggregate by}

\item{when.tie}{what value to return when there is a tie (defaults to NA)}

\item{ignore.missing}{when TRUE, first removes NA, empty strings, -Inf, Inf  (default TRUE)}
}
\value{
data frame of Modes for all variables in df, with one row per unique value in the column named in aggregate.by
}
\description{
Aggregating to mode
}
\examples{

}
\seealso{
\code{\link{function_name}}
}
