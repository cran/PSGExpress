\name{rpsg_solver}
\alias{rpsg_solver}
\title{solves optimization problem using General (Text) Format of PSG}
\usage{
rpsg_solver(problem_list, rho = parent.frame(), allowExt = TRUE,
  rpsg_suppress.Messages = FALSE)
}
\arguments{
\item{problem_list}{list with data for optimization problem. List members:
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
}}

\item{rho}{optional parameter for setting frame. Default is rho = parent.frame().}

\item{allowExt}{optional parameter to specify if a solver can use variables from
data frame defined in rho (by the default allowExt = TRUE) or not (allowExt = FALSE).}

\item{rpsg_suppress.Messages}{optional parameter specifying if messages, that may appear when
you run this function, should be suppressed (rpsg_suppress.Messages = TRUE) or not (by the default: rpsg_suppress.Messages = FALSE).}
}
\value{
list \verb{output.list} with solution results:
\describe{
\item{\verb{output.list$problem_name }}{ name (problem_<name>) of the optimization problem. By default: problem_1;}
\item{\verb{output.list$solution_status }}{ status of solution of the optimization problem: optimal | feasible | infeasible | unbounded | calculated (for calculation problems);}
\item{\verb{output.list$problem_statement }}{ Problem Statement of the solved problem;}
\item{\verb{output.list$output }}{ solution report = character with main information about optimization results;}
\item{\verb{output.list$point_constraints_problem_<name> }}{ optimal values of left hand sides of constraints in optimization problem;}
\item{\verb{output.list$point_slack_constraints_problem_<name> }}{ slacks value for constraints of the optimization problem;}
\item{\verb{output.list$point_dual_constraints_problem_<name> }}{ dual constraints values;}
\item{\verb{output.list$point_problem_<name> }}{ optimal point for solved optimization problem;}
\item{\verb{output.list$log }}{information about solving process.}
}
}
\description{
To solve optimization problem using PSG solvers it is necessary to prepare
problem in \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?general_format.htm}{General (Text) format}.
There are three main objects of PSG:
\itemize{
\item \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?problem_statement_description.htm}{Problem Statement}.
It is a character with description of optimization problem.
This object must be created according to the rules of PSG General format.\cr
Problem statement for optimization problems with one constraint: \cr
\cr
\emph{(minimize|maximize)}\cr
\emph{[<coef1>*]<PSG function obj 1>}\cr
\emph{.........}\cr
\emph{[<coefK>*]<PSG function obj K>}\cr
\emph{Constraint:(<=|>=|==) <bound>}\cr
\emph{[<coef1>*]<PSG function constr 1>}\cr
\emph{.........}\cr
\emph{[<coefK>*]<PSG function constr K>}\cr
\emph{Box: (<=|>=|==) <bound>}
\item \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?description_data_in_psg_format.htm}{PSG Data Objects}.
Data for optimization problem in a specific format:
PSG Matrix, PSG PMatrix, PSG Point, and PSG Vector.
\item \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?output_in_run-filh.htm}{PSG Solution}. The results of optimization problem are stored in solution report (character) and PSG Data Objects (PSG Matrix, PSG Point, ect).
}
}
\note{
\itemize{
\item rpsg_solver supports two cases of input data: \cr
1. all PSG data objects are saved in input list, \cr
2. all PSG data objects are saved in Global Environment. \cr
In both cases input list must include PSG Problem Statement (problem_list$problem_statement)
and names of PSG data objects (names of list members or variables) should be used in Problem Statement.
\item Use \code{\link{rpsg_getsolution}} to convert character solution report into list witn numeric data.
\item Use \code{\link{rpsg_verify}} to verify problem before optimization.
}

\itemize{
\item List with solution results \emph{output.list} may include output
matrices, vectors and points not mentioned above. It is due to the specific
of some PSG functions used in optimization problem.
}
}
\examples{
#Problem of CVaR minimization with constraint on the mean profit:
#Find x = (x1,x2,x3,x4) minimizing
#risk(x) = CVaR(0.95,x)
#subject to
#Average Gain(x)>4.5
#x1+x2+x3+x4 = 1
#x1>=0, x2>=0, x3>=0, x4>=0

matrix_scenarios <- matrix(c(1,4,8,3, 7,5,4,6, 2,8,1,0,0,3,4,9),nrow=4, byrow=TRUE)
colnames(matrix_scenarios) <- colnames(matrix_scenarios,do.NULL = FALSE, prefix = "x")
scenario_benchmark <- c(0.2, 0.11, 0.6, 0.1)
matrix_scenarios <- cbind(matrix_scenarios,scenario_benchmark)
matrix_budget <- matrix(c(1, 1, 1, 1),nrow=1)
colnames(matrix_budget) <- colnames(matrix_budget,do.NULL = FALSE, prefix = "x")
point_lowerbounds <- c(0, 0, 0, 0)
names(point_lowerbounds) <- rownames(point_lowerbounds,do.NULL = FALSE, prefix = "x")

# Case 1. PSG data is saved in list
problem_list <- list()

#Problem Statement
problem_list$problem_statement <- sprintf(
 "minimize
 cvar_risk(0.95,matrix_scenarios)
Constraint: >= 4.5
 avg_g(matrix_scenarios)
Constraint: == 1
 linear(matrix_budget)
Box: >= point_lowerbounds
Solver:CAR")

# PSG Matrix:
problem_list$matrix_scenarios <- matrix_scenarios

# PSG Matrix:
problem_list$matrix_budget <- matrix_budget

# PSG Point:
problem_list$point_lowerbounds <- point_lowerbounds

# Solve optimization problem
output.list.1 <- rpsg_solver(problem_list)

print(output.list.1)


# Case 2. PSG data is saved in Global Environment
problem_list <- list()

#Problem Statement
problem_list$problem_statement <- sprintf(
 "minimize
 cvar_risk(0.95,matrix_scenarios)
Constraint: >= 4.5
 avg_g(matrix_scenarios)
Constraint: == 1
 linear(matrix_budget)
Box: >= point_lowerbounds")

output.list.2 <- rpsg_solver(problem_list)

print(output.list.2)


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
\code{\link{rpsg_verify}}  \code{\link{rpsg_riskprog}}  \code{\link{rpsg_getsolution}}
}

