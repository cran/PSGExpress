\name{rpsg_restore_ps}
\alias{rpsg_restore_ps}
\title{Replace parameters with their values in PSG Problem Statement}
\usage{
rpsg_restore_ps(input)
}
\arguments{
\item{input}{list with data for optimization problem. List members:
\describe{
\item{\verb{input$problem_statement}}{character with PSG \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?problem_statement_description.htm}{Problem Statement};}
\item{\verb{input$parameter_<name>}}{value of PSG functions parameter;}
\item{\verb{input$coefficient_<name>}}{value of the coefficient before PSG function in objective or constraint;}
\item{\verb{input$bound_<name>}}{value of the bound in Constraint or Box of Variables sections of PSG Problem Statement;}
\item{\verb{input$solver_<name>}}{character with name of optimization solver: VAN, CAR, BULDOZER, TANK;}
\item{\verb{input$precision_<name>}}{number of digits that solver tries to obtain in objective;}
\item{\verb{input$stages_<name>}}{number of stages of the optimization process;}
\item{\verb{input$timelimit_<name>}}{time in seconds restricting the duration of the optimization process;}
\item{\verb{input$linearize_<name>}}{number 0 or 1, controls internal representation of risk function;}
\item{\verb{input$mip_<name>}}{number that specifies the linearization of PSG functions and using MIP capabilities of Gurobi. Active only in VANGRB, CARGRB or HELI solvers
(based on Gurobi optimization solvers and are available only if Gurobi is installed);}
}}
}
\value{
output list with results:
\describe{
\item{\verb{output$problem_statement}}{character with PSG Problem Statement with values of all input parameters;}
\item{\verb{output$...}}{all inputs without parameters;}
}
}
\description{
Replace parameters with their values in PSG Problem Statement
}
\examples{

#Define PSG Problem Statement:
problem_list<-list()
problem_list$problem_statement <- sprintf(
 "minimize
 cvar_risk(parameter_alpha,matrix_scenarios)
Constraint: >= bound_avg
 avg_g(matrix_scenarios)
Constraint: == bound_lin
 linear(matrix_budget)
Box: >= point_lowerbounds
Solver: solver_van")

#Define parameters:
problem_list$parameter_alpha <-0.95
problem_list$bound_avg <-4.5
problem_list$bound_lin <-1
problem_list$solver_van <-"VAN"

#Convert PSG Problem Statement:

problem_list_new <- rpsg_restore_ps(problem_list)

cat(problem_list_new$problem_statement)

}
\author{
\packageAuthor{PSGExpress}

Maintainer: \packageMaintainer{PSGExpress}
}
\references{
\href{http://www.aorda.com}{American Optimal Decisions} \cr
\href{http://www.aorda.com/html/PSG_Help_HTML}{Portfolio Safeguard Help}
}

