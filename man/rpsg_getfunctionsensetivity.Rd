\name{rpsg_getfunctionsensetivity}
\alias{rpsg_getfunctionsensetivity}
\title{PSG Functions sensetivity calculation}
\usage{
rpsg_getfunctionsensetivity(function_description, point_argument,
  rho = parent.frame(), allowExt = TRUE, rpsg_suppress.Messages = FALSE,
  allowFunVal = TRUE)
}
\arguments{
\item{function_description}{character with PSG function description:
\emph{"risk_function(<parameter>,matrix_<name>)"}. This input depends on type
of PSG function (parameter is present or absent, one, two or more matrices, ect.).
\emph{matrix_<name>} should be created as variable before calling function \emph{rpsg_getfunctionvalue}.}

\item{point_argument}{vector with names of members that correspond to
names of optimization variables.}

\item{rho}{optional parameter for setting frame. Default is rho = parent.frame().}

\item{allowExt}{optional parameter to specify if a solver can use variables from
data frame defined in rho (by the default allowExt = TRUE) or not (allowExt = FALSE).}

\item{rpsg_suppress.Messages}{optional parameter specifying if messages, that may appear when
you run this function, should be suppressed (rpsg_suppress.Messages = TRUE) or not (by the default: rpsg_suppress.Messages = FALSE).}

\item{allowFunVal}{optional parameter specifying if output.list will include value of the PSG function on the given point
(by the default allowFunVal = TRUE) or not (allowFunVal = FALSE)}
}
\value{
\verb{output.list} list with the following components:
\describe{
\item{\verb{results$Fun_Value}}{numeric value of PSG function on the given point;}
\item{\verb{results$Fun_Sensetivity}}{vector of a function sensetivity (gradient) calculated on the given point.}
}
}
\description{
Calculates sensetivity (gradient) of a PSG function on some point.
}
\examples{

#Create Matrix
matrix_scenarios <- matrix(c(1,4,8,3, 7,-5,4,-6, -2,8,-1,0,0,-3,-4,9),nrow=4, byrow=TRUE)
colnames(matrix_scenarios) <- colnames(matrix_scenarios,do.NULL = FALSE, prefix = "x")
scenario_benchmark <- c(-2, 11, 6, 10)
scenario_probability <- c(0.2, 0.2, 0.3, 0.3)
matrix_scenarios <- cbind(matrix_scenarios,scenario_benchmark,scenario_probability)

#Create Point
point_a <- c(1,1,1,1)
names(point_a) <-c("x1","x2","x3","x4")

val1<- rpsg_getfunctionsensetivity("cvar_risk(0.65,matrix_scenarios)",point_a)

print(val1)



}
\author{
\packageAuthor{PSGExpress}

Maintainer: \packageMaintainer{PSGExpress}
}
\references{
\href{http://www.aorda.com}{American Optimal Decisions} \cr
\href{http://www.aorda.com/html/PSG_Help_HTML}{Portfolio Safeguard Help}
}
\seealso{
\code{\link{rpsg_getfunctionincrement}} \code{\link{rpsg_getfunctionvalue}}
}

