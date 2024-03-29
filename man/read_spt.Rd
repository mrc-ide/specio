% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_epp_results.R
\name{read_spt}
\alias{read_spt}
\title{Read best fitting HIV incidence and prevalence data from SPT file.}
\usage{
read_spt(pjnz_path)
}
\arguments{
\item{pjnz_path}{Path to the PJNZ file.}
}
\value{
List of data frames containing the best fit prevalence and incidence
data.
}
\description{
Locates SPT file within the PJNZ file and reads out the best fitting
incidence and prevalence data into memory.
}
\details{
Reads best fitting prevalence and incidence and population data for all
regions. The outputs one data frame for each region in the SPT file.
}
\examples{
pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package = "specio")
read_spt(pjnz_path)
}
