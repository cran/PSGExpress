\name{rpsg_importfromtext}
\alias{rpsg_importfromtext}
\title{Imports PSG Problem from text}
\usage{
rpsg_importfromtext(path_tofiles, rpsg_suppress.Messages = FALSE)
}
\arguments{
\item{path_tofiles}{character with path to *.txt file with PSG problem statement
in \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?general_format.htm}{General (Text) format}.
All data needed for solving this problem should be stored in the same folder in separate .txt files.
Name of the *.txt file for PSG problem statement should start with \emph{problem_}.}

\item{rpsg_suppress.Messages}{optional parameter specifying if messages, that may appear when
you run this function, should be suppressed (rpsg_suppress.Messages = TRUE) or not (by the default: rpsg_suppress.Messages = FALSE).}
}
\value{
problem_list  list with data for optimization problem. List members:
\describe{
\item{\verb{problem_list$problem_statement}}{character with PSG Problem Statement;}
\item{\verb{problem_list$matrix_<name>}}{matrix with names of columns that correspond to
names of optimization variables. Name of this list member (matrix_<name>) must correspond to the name
of PSG Matrix in Problem Statement. Matrix may include two optional collumns:
scenario_benchmark and scenario_probability;}
\item{\verb{problem_list$pmatrix_<name>}}{sparse matrix with names of columns that correspond to
names of optimization variables. Name of this list member (pmatrix_<name>) must correspond to the name
of PSG PMatrix in Problem Statement. PMatrix may include two optional collumns:
scenario_benchmark and scenario_probability;}
\item{\verb{problem_list$point_<name>}}{vector with names of members that correspond to
names of optimization variables. Name of this list member (point_<name>) must correspond to the name
of PSG Point in Problem Statement;}
\item{\verb{problem_list$vector_<name>}}{vector with data. Name of this list member (vector_<name>) must correspond to the name
of PSG Vector in Problem Statement.}
}
}
\description{
Imports problem statement and its data stored in txt files
in \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?general_format.htm}{General (Text) format}.
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
\code{\link{rpsg_exporttotext}}
}

