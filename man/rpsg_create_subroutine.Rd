\name{rpsg_create_subroutine}
\alias{rpsg_create_subroutine}
\title{Creates user-defined R-function for solving PSG optimization problem.}
\usage{
rpsg_create_subroutine(problem.list, description)
}
\arguments{
\item{problem.list}{list with data for optimization problem. List members:
\describe{
\item{\verb{problem.list$problem_statement}}{character with PSG Problem Statement;}
\item{\verb{problem.list$matrix_<name>}}{matrix with names of columns that correspond to
names of optimization variables. Name of this list member (matrix_<name>) must coincide with the name
of PSG Matrix in Problem Statement. Matrix may include two optional collumns:
scenario_benchmark and scenario_probability;}
\item{\verb{problem.list$pmatrix_<name>}}{sparse matrix with names of column that correspond to
names of optimization variables. Name of this list member (pmatrix_<name>) must coincide with the name
of PSG PMatrix in Problem Statement. PMatrix may include two optional collumns:
scenario_benchmark and scenario_probability;}
\item{\verb{problem.list$point_<name>}}{vector with names of members that correspond to
names of optimization variables. Name of this list member (point_<name>) must coincide with the name
of PSG Point in Problem Statement;}
\item{\verb{problem.list$vector_<name>}}{vector with data. Name of this list member (vector_<name>) must coincide with the name
of PSG Vector in Problem Statement.}
}}

\item{description}{list with additional information about user-defined R-function. Names of list members:
\describe{
\item{\verb{description$path_to_save}}{character with path to the folder for saving function;}
\item{\verb{description$function_name}}{character with the name user-defined R-function;}
\item{\verb{description$function_description}}{character with description of functions functionality;}
}}
}
\value{
Three files stored in folder typed by user:
\item{users_f_name.R}{R-file with user-defined R-function;}
\item{users_f_name_data.RData}{RData-file with data for PSG optimization problem;}
\item{users_f_name_example.R}{R-file with example code to run user-defined R-function;}
}
\description{
Creates user-defined R-function for solving PSG optimization problem.
}
\references{
\href{http://www.aorda.com}{American Optimal Decisions} \cr
\href{http://www.aorda.com/html/PSG_Help_HTML}{Portfolio Safeguard Help}
}
\author{
\packageAuthor{PSGExpress}

Maintainer: \packageMaintainer{PSGExpress}
}
