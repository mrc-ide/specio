% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dp_accessors.R
\name{get_array_data}
\alias{get_array_data}
\title{Get array data for a particular property from DP data.}
\usage{
get_array_data(tag, tag_data, metadata, proj_years)
}
\arguments{
\item{tag}{The name of the tag getting data for.}

\item{tag_data}{Tag data to be parsed.}

\item{metadata}{Metadata params made available by tag configuration. Must
contain rows, and a func to define array dimensions and names.}

\item{proj_years}{Years the projection is for.}
}
\description{
Get array data for a particular property from DP data.
}
\keyword{internal}
