\name{rpsg_convert_ps_params}
\alias{rpsg_convert_ps_params}
\title{Replace values of parameters with variables in problem statement}
\usage{
rpsg_convert_ps_params(problem_statement_init)
}
\arguments{
\item{character}{with PSG Problem Statement}
}
\value{
results   list with results:
\tabular{ll}{
results$problem_statement_new \tab character with PSG Problem Statement where parameters are
 changed with corresponding variables;\cr
results$parameter.data.list \tab list with variables used in the new PSG Problem Statement.\cr
}
}
\description{
Replace values of parameters with variables in problem statement
}
\keyword{internal}

