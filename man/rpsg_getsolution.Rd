\name{rpsg_getsolution}
\alias{rpsg_getsolution}
\title{Convert character PSG solution report into list witn numeric data}
\usage{
rpsg_getsolution(problem.res)
}
\arguments{
\item{problem.res}{list with solution results of the optimization problem obtained by \code{\link{rpsg_solver}}
\describe{
\item{\verb{problem.res$problem_name}}{name (problem_<name>) of the optimization problem. By default: problem_1;}
\item{\verb{problem.res$solution_status}}{status of the solution of the optimization problem: optimal | feasible | infeasible | unbounded | calculated (for calculation problems);}
\item{\verb{problem.res$problem_statement}}{Problem Statement of the solved problem;}
\item{\verb{problem.res$output}}{solution report = character with main information about optimization results;}
\item{\verb{problem.res$point_constraints_problem_<name>}}{optimal values of left hand sides of constraints in the optimization problem;}
\item{\verb{problem.res$point_slack_constraints_problem_<name>}}{slacks value for constraints of the optimization problem;}
\item{\verb{problem.res$point_dual_constraints_problem_<name>}}{dual constraints values;}
\item{\verb{problem.res$point_problem_<name>}}{optimal point for solved optimization problem;}
\item{\verb{problem.res$log}}{information about solving process.}
}}
}
\value{
\verb{output.list} = list with solution results. List members:
\describe{
\item{\verb{output.list$status}}{status of the solution of the optimization problem: optimal | feasible | infeasible | unbounded | calculated (for calculation problems);}
\item{\verb{output.list$loading.time}}{data loading time;}
\item{\verb{output.list$preprocessing.time}}{preprocessing time;}
\item{\verb{output.list$solving.time}}{solving time;}
\item{\verb{output.list$objective}}{optimal value of objective function;}
\item{\verb{output.list$gap}}{difference between objective value in obtained point and Lower estimate of optimal value;}
\item{\verb{output.list$function.value}}{optimal values of PSG functions defined in the problem;}
\item{\verb{output.list$point_constraints_problem_<name>}}{optimal values of left hand sides of constraints in the optimization problem;}
\item{\verb{output.list$point_slack_constraints_problem_<name>}}{slacks value for constraints of the optimization problem;}
\item{\verb{output.list$point_dual_constraints_problem_<name>}}{dual constraints values;}
\item{\verb{output.list$point_problem_<name>}}{optimal point for solved optimization problem;}
}
}
\description{
Standard output of \code{\link{rpsg_solver}} contains information in character string \emph{problem.res$output}.
\emph{rpsg_getsolution} function exctracts the main information from character string and creates list with numeric data.
}
\note{
\itemize{
\item Lists with solution results, \emph{problem.res} and \emph{output.list}, may include output
matrices, vectors and points not mentioned above. It is due to the specific characteristics
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
Solver: CAR")

# PSG Matrix:
problem_list$matrix_scenarios <- matrix_scenarios

# PSG Matrix:
problem_list$matrix_budget <- matrix_budget

# PSG Point:
problem_list$point_lowerbounds <- point_lowerbounds

# Solve optimization problem
output1 <- rpsg_solver(problem_list)

output2 <- rpsg_getsolution(output1)

print(output2)

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
\code{\link{rpsg_solver}}
}

