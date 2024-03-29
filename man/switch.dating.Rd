% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datplot_utility.R
\name{switch.dating}
\alias{switch.dating}
\title{Switch values where dating is in wrong order (internal)}
\usage{
switch.dating(DAT_df)
}
\arguments{
\item{DAT_df}{a data.frame with 4 variables in this order: ID, group,
minimum date (int/num), maximum date (int/num)}
}
\value{
The same data.frame with the dating values which were in wrong order
switched.
}
\description{
Requires a data.frame with 2 numeric variables in the
3rd and 4th column: minimum date (int/numeric) and
maximum date (int/numeric) as used in [datsteps()].
}
\seealso{
[datsteps()]
}
\keyword{internal}
