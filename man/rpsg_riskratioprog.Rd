\name{rpsg_riskratioprog}
\alias{rpsg_riskratioprog}
\title{Maximizes ratio of risk functions with linear constraints}
\usage{
rpsg_riskratioprog(model, stroptions = NULL)
}
\arguments{
\item{model}{list with data for optimization problem. Some components are optional. Names of list members:
\describe{
\item{\verb{model$risk1 }}{  character with name of PSG function in denominator. \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?riskratioprog.htm}{List of PSG functions for riskratioprog}}
\item{\verb{model$risk2 }}{  character with name of PSG function in numerator. \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?riskratioprog.htm}{List of PSG functions for riskratioprog}}
\item{\verb{model$w1}}{  parameter (numeric) of the PSG function model$risk1;}
\item{\verb{model$w2}}{  parameter (numeric) of the PSG function model$risk2;}
\item{\verb{model$H1}}{  matrix for PSG function model$risk1;}
\item{\verb{model$H2}}{  matrix for PSG function model$risk2;}
\item{\verb{model$c1}}{  vector of benchmark for PSG function model$risk1;}
\item{\verb{model$c2}}{  vector of benchmark for PSG function model$risk2;}
\item{\verb{model$p1}}{  vector of probabilities for PSG function model$risk1;}
\item{\verb{model$p2}}{  vector of probabilities for PSG function model$risk2;}
\item{\verb{model$d}}{  vector for linear component in numerator;}
\item{\verb{model$Aineq}}{  matrix for linear inequality constraint;}
\item{\verb{model$bineq}}{  vector or scalars for linear inequality constraint;}
\item{\verb{model$Aeq}}{  matrix for linear equality constraint;}
\item{\verb{model$beq}}{  vector for linear equality constraint;}
\item{\verb{model$lb}}{  vector of lower bounds for x;}
\item{\verb{model$ub}}{  vector of upper bounds for x;}
\item{\verb{model$x0}}{  initial point for x.}
}}

\item{stroptions}{list with additional optimization options:
\describe{
\item{\verb{stroptions$solver}}{  character with name of optimization solver: VAN (default), CAR, BULDOZER, TANK;}
\item{\verb{stroptions$precision}}{  number of digits that solver tries to obtain in objective (default = 7);}
\item{\verb{stroptions$time.limit}}{  time in seconds restricting the duration of the optimization process;}
\item{\verb{stroptions$linearization1}}{  number 0 or 1, controls internal representation of risk function model$risk1, which can speed up the optimization process (used  with CAR and TANK solvers);}
\item{\verb{stroptions$linearization2}}{  number 0 or 1, controls internal representation of risk function model$risk2, which can speed up the optimization process (used  with CAR and TANK solvers);}
\item{\verb{stroptions$stages}}{  number of stages of the optimization process. This parameter should be specified for VaR,  Probability, and Cardinality groups of functions, default = 9;}
\item{\verb{stroptions$types}}{  number that specifies the variables types of a problem. If Types is defined as column-vector, it should include as many components as number of variables Problem includes. The components of column-vector can possess the values 0 - for variables of type real, or 1- for variables of type boolean, or 2 - for variables of type integer. If Types is defined as one number (0, or 1, or 2) than all variables types real, or   boolean, or integer respectively;}
\item{\verb{stroptions$mip1}}{  number that specifies the linearization of functions model$risk1 and using MIP capabilities of Gurobi. Active only in VANGRB, CARGRB or HELI solvers (which are based on Gurobi optimization solvers and are available only if Gurobi is installed).}
\item{\verb{stroptions$mip2}}{  number that specifies the linearization of functions model$risk2 and using MIP capabilities of Gurobi. Active only in VANGRB, CARGRB or HELI solvers (which are based on Gurobi optimization solvers and are available only if Gurobi is installed).}
\item{\verb{stroptions$save.to.text}}{ character with path to the folder for storing problem in General (Text) Format of PSG.}
}}
}
\value{
list \verb{results} with solution results:
\describe{
\item{\verb{results$status }}{ status of solved problem;}
\item{\verb{results$objective }}{ optimal value of objective function;}
\item{\verb{results$gap }}{ difference between objective value in obtained point and Lower estimate of optimal value;}
\item{\verb{results$optimal.point }}{ optimal point;}
\item{\verb{results$ineq.constraint.value }}{ optimal values of left hand sides of linear inequality constraint;}
\item{\verb{results$ineq.constraint.residual }}{  residual of linear inequality constraint;}
\item{\verb{results$eq.constraint.value }}{ optimal values of left hand sides of linear equality constraint;}
\item{\verb{results$eq.constraint.residual }}{ residual of linear equality constraint;}
\item{\verb{results$function.value }}{ optimal values of PSG functions defined in problem;}
\item{\verb{results$loading.time }}{  data loading time;}
\item{\verb{results$preprocessing.time }}{ preprocessing time;}
\item{\verb{results$solving.time }}{ solving time.}
}
}
\description{
Finds a maximum for a problem specified by
\deqn{max_x[\frac{d^{T}x-risk2(x)}{risk1(x)}]}
subject to
\deqn{A \cdot x= b}
\deqn{Aeq \cdot x= beq}
\deqn{lb \leq x \leq ub}
where\cr
A, Aeq are matrices;\cr
d, x are vectors;\cr
b, beq, lb, ub are vectors or scalars;\cr
risk1(x), risk2(x)  functions are linear combinations of PSG risk functions, PSG deterministic functions, or PSG utility functions. \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?riskratioprog.htm}{List of PSG functions for riskratioprog}
}
\examples{

#Problem:
#Find x = (x1,x2,x3,x4) maximizing
#f(x) = Average Gain(x)/Partial Moment Penalty for Loss(x)
#subject to
#x1+x2+x3+x4+x5 = 1
#x1+x3 <= 0.6
#x2+x4+x5 <= 0.5
#0 <= x1 <= 0.5, 0 <= x2 <= 0.5, 0.3 <= x3 <= 0.5, 0 <= x4 <= 0.5, 0 <= x5 <= 0.5

input.model <- list()
input.model$risk1 <- "pm_pen"
input.model$w1 <- -1
input.model$risk2 <- "avg"
H1 <- c(0.11, 0.04, -0.12, 0.01, -0.05,
       -0.01, -0.1, 0.03, 0.21, 0.01,
       0.05, 0.2, 0.07, -0.03, -0.12,
       -0.1, 0.14, 0.04, -0.1, 0.09,
       0.08, -0.02, -0.2, 0.07, -0.01,
       0.17, 0.3, -0.12, -0.02, 0.08)
input.model$H1 <- matrix(H1,ncol = 5,byrow = TRUE)
input.model$c1 <- c(0.06, -0.1, 0.12, 0.01, -0.06, 0.11)
input.model$H2 <- input.model$H1
input.model$c2 <- input.model$c1
input.model$Aineq <- matrix(c(1,0, 1, 0, 0, 0, 1, 0, 1, 1),ncol = 5,byrow = TRUE)
input.model$bineq <- c(0.6, 0.5)
input.model$Aeq <- matrix(c(1.0, 1.0, 1.0, 1.0, 1.0),nrow = 1)
input.model$beq <- 1.0
input.model$lb <- c(0.0, 0.0, 0.3, 0.0, 0.0)
input.model$ub <- c(0.5, 0.5, 0.5, 0.5, 0.5)

options<-list()
options$solver <- "van"
options$precision <- 7

results <- rpsg_riskratioprog(input.model,options)

print(results)

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
\code{\link{rpsg_riskprog}}    \code{\link{rpsg_riskconstrprog}}
}

