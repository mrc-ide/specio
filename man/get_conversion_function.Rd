% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xml_utils.R
\name{get_conversion_function}
\alias{get_conversion_function}
\title{Get the relevant conversion function for an xml node.}
\usage{
get_conversion_function(node, search = 1)
}
\arguments{
\item{node}{The nodeset to get the property from.}

\item{search}{Passed to xml_child, either the child number to convert, or
name of the child node to convert. If multiple with the same name exist, the
first will be returned. Defaults to first.}
}
\value{
Conversion function for the node type.
}
\description{
Looks at the children of the node to determine the type of data contained in
the node. If the node represents an array then this will return all of the
array data. If the children contain single property data then it will return
the function to convert the data at the specified search location.
}
\keyword{internal}
