% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swc_correct.R
\name{swc_correct}
\alias{swc_correct}
\title{Internal function to correct depths for VSWC NEON data.}
\usage{
swc_correct(input_swc, curr_site, reference_time)
}
\arguments{
\item{input_swc}{Required. input soil water content data from acquire_neon_data (as a list)}

\item{curr_site}{Current site we are working with}

\item{reference_time}{Current month we are working with}
}
\value{
A revised list of corrected soil water content and depths.
}
\description{
Given the expanded SWC data, return a corrected version based on the values below
}
\examples{
\donttest{
# Download the soil water content data:
site_swc <- neonUtilities::loadByProduct(
dpID="DP1.00094.001",
site="SJER",
startdate="2020-05",
enddate="2020-05",
timeIndex = "30",
package="expanded",
check.size = FALSE,
include.provisional = TRUE
)

# Then correct the swc:
site_swc <- swc_correct(site_swc,"SJER","2020-05")
}
}
\author{
John Zobitz \email{zobitz@augsburg.edu}
}
