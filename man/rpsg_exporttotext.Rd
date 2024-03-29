\name{rpsg_exporttotext}
\alias{rpsg_exporttotext}
\title{Exports PSG Problem to text}
\usage{
rpsg_exporttotext(path_tofiles, problem_list, rho = parent.frame(),
  allowExt = TRUE, rpsg_suppress.Messages = FALSE)
}
\arguments{
\item{path_tofiles}{character with path to *.txt file for storing PSG problem statement.
All data needed for solving this problem will be stored in the same folder in separate .txt files.
Name of the *.txt file for PSG problem statement should start with \emph{problem_}.}

\item{problem_list}{list with data for optimization problem. List members:
\describe{
\item{\verb{problem_list$problem_statement}}{character with PSG Problem Statement;}
\item{\verb{problem_list$matrix_<name>}}{matrix with names of columns that correspond to
names of optimization variables. Name of this list member (matrix_<name>) must coincide with the name
of PSG Matrix in Problem Statement. Matrix may include two optional collumns:
scenario_benchmark and scenario_probability;}
\item{\verb{problem_list$pmatrix_<name>}}{sparse matrix with names of columns that correspond to
names of optimization variables. Name of this list member (pmatrix_<name>) must coincide with the name
of PSG PMatrix in Problem Statement. PMatrix may include two optional collumns:
scenario_benchmark and scenario_probability;}
\item{\verb{problem_list$point_<name>}}{vector with members' names that correspond to
names of optimization variables. Name of this list member (point_<name>) must coincide with the name
of PSG Point in Problem Statement;}
\item{\verb{problem_list$vector_<name>}}{vector with data. Name of this list member (vector_<name>) must coincide with the name
of PSG Vector in Problem Statement.}
}}

\item{rho}{optional parameter for setting frame. Default is rho = parent.frame().}

\item{allowExt}{optional parameter defining may solver use variables from
data frame defined in rho (by the default allowExt = TRUE) or not (allowExt = FALSE).}

\item{rpsg_suppress.Messages}{optional parameter specifying if messages that may appear when
you run this function should be suppressed (rpsg_suppress.Messages = TRUE) or not (by the default: rpsg_suppress.Messages = FALSE).}
}
\value{
text files with problem in
 \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?general_format.htm}{General (Text) format.}
}
\description{
Exports the PSG problem from R to text files with problem in
\href{http://www.aorda.com/html/PSG_Help_HTML/index.html?general_format.htm}{General (Text) format}.
}
\note{
\itemize{
\item \emph{rpsg_exporttotext} supports two cases of input data: \cr
1. all PSG data objects are saved in input list, \cr
2. all PSG data objects are saved in Global Environment. \cr
In both cases input list must include PSG Problem Statement (problem_list$problem_statement)
and names of PSG data objects (names of list members or variables) should be used in Problem Statement.
}
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
\code{\link{rpsg_importfromtext}} 
}
