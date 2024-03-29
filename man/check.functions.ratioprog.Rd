% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rpsg_riskratioprog.R
\name{check.functions.ratioprog}
\alias{check.functions.ratioprog}
\title{Checks if PSG function may be used in rpsg_riskratioprog as risk1 or risk2.}
\usage{
check.functions.ratioprog(function.text)
}
\arguments{
\item{character}{with name of PSG function}
}
\value{
list with results:
\tabular{ll}{
resultf$adm \tab TRUE if PSG function may be used and FALSE otherwise;\cr
resultf$param \tab type of the parameter of PSG function: 0 = no parameter, 1 = threshold, 2 = user defined parameter;\cr
resultf$num \tab TRUE if PSG function may be used and FALSE otherwise;\cr
}
}
\description{
Checks if PSG function may be used in rpsg_riskratioprog as risk1 or risk2.
}
\keyword{internal}

