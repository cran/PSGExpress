\name{rpsg_riskconstrprog}
\alias{rpsg_riskconstrprog}
\title{Solves risk minimization problems with constraint on a risk function and linear constraints}
\usage{
rpsg_riskconstrprog(model, stroptions = NULL)
}
\arguments{
\item{model}{list with data for the optimization problem. Some components are optional. Names of list members:
\describe{
\item{\verb{model$risk1}}{character with description of the PSG function (objective includes only one PSG function) or character vector with coefficients and PSG functions in one of the following forms: {coefficient 1, function 1, ... , coefficient K, function K}; "na" denotes the absent parameter. \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?table_list_of_functions.htm}{List of PSG functions for riskconstrprog}}
\item{\verb{model$risk2}}{character with description of the PSG function (constraint includes only one PSG function) or character vector with coefficients and PSG functions in one of the following forms: {coefficient 1, function 1, ... , coefficient K, function K}; "na" denotes the absent parameter. \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?table_list_of_functions.htm}{List of PSG functions for riskconstrprog}}
\item{\verb{model$w1}}{parameter (numeric) of the PSG function (for one PSG function in objective) or vector of parameters (for linear combination of PSG functions; order of parameters are the same as the order of functions in model$risk1, "na" denotes the absent parameter);}
\item{\verb{model$w2}}{parameter (numeric) of the PSG function (for one PSG function in constraint) or vector of parameters (for linear combination of PSG functions; order of parameters are the same as the order of functions in model$risk2, "na" denotes the absent parameter);}
\item{\verb{model$H1}}{matrix for one PSG function in objective or vector of matrices for linear combination of PSG functions model$risk1 (order of matrix are the same as the order of functions in model$risk1);}
\item{\verb{model$H2}}{matrix for one PSG function in constraint or vector of matrices for linear combination of PSG functions model$risk2 (order of matrix are the same as the order of functions in model$risk2);}
\item{\verb{model$c1}}{vector of benchmark for one PSG function in objective or vector of vectors of benchmarks for linear combination of PSG functions (order of vectors are the same as the order of functions in model$risk1);}
\item{\verb{model$c2}}{vector of benchmark for one PSG function in constraint or vector of vectors of benchmarks for linear combination of PSG functions (order of vectors are the same as the order of functions in model$risk2);}
\item{\verb{model$p1}}{vector of probabilities for one PSG function in objective or vector of vectors of probabilities for linear combination of PSG functions (order of vectors are the same as the order of functions in model$risk1);}
\item{\verb{model$p2}}{vector of probabilities for one PSG function in constraint or vector of vectors of probabilities for linear combination of PSG functions (order of vectors are the same as the order of functions in model$risk2);}
\item{\verb{model$d}}{vector for linear component of objective;}
\item{\verb{model$rineq}}{value of upper bound for risk constraint;}
\item{\verb{model$Aineq}}{matrix for linear inequality constraint;}
\item{\verb{model$bineq}}{vector or scalars for linear inequality constraint;}
\item{\verb{model$Aeq}}{matrix for linear equality constraint;}
\item{\verb{model$beq}}{vector for linear equality constraint;}
\item{\verb{model$lb}}{vector of lower bounds for x;}
\item{\verb{model$ub}}{vector of upper bounds for x;}
\item{\verb{model$x0}}{initial point for x.}
}}

\item{stroptions}{list with additional optimization options:
\describe{
\item{\verb{stroptions$solver}}{character with name of optimization solver: VAN (default), CAR, BULDOZER, TANK;}
\item{\verb{stroptions$precision}}{number of digits that solver tries to obtain in objective and constraints (default = 7);}
\item{\verb{stroptions$time.limit}}{time in seconds restricting the duration of the optimization process;}
\item{\verb{stroptions$linearization1}}{number 0 or 1, controls internal representation of risk function in objective (model$risk1), which can speed up the optimization process (used  with CAR and TANK solvers);}
\item{\verb{stroptions$linearization2}}{number 0 or 1, controls internal representation of risk function in constraint (model$risk2), which can speed up the optimization process (used  with CAR and TANK solvers);}
\item{\verb{stroptions$stages}}{number of stages of the optimization process. This parameter should be specified for VaR,  Probability, and Cardinality groups of functions, default = 9;}
\item{\verb{stroptions$types}}{number that specifies the variable types of a problem. If Types is defined as column-vector, it should include as many components as number of variables Problem includes. The components of column-vector can possess the values 0 - for variables of type real, or 1- for variables of type boolean, or 2 - for variables of type integer. If Types is defined as one number (0, or 1, or 2) than all variable types are real, or   boolean, or integer respectively;}
\item{\verb{stroptions$mip1}}{number that specifies the linearization of functions in objective (model$risk1)  using MIP capabilities of Gurobi. Active only in VANGRB, CARGRB or HELI solvers (which are based on Gurobi optimization solvers and are available only if Gurobi is installed).}
\item{\verb{stroptions$mip2}}{number that specifies the linearization of functions in constraint (model$risk2) using MIP capabilities of Gurobi. Active only in VANGRB, CARGRB or HELI solvers (which are based on Gurobi optimization solvers and are available only if Gurobi is installed).}
\item{\verb{stroptions$save.to.text}}{character with path to the folder for storing problem in General (Text) Format of PSG.}
}}
}
\value{
list \verb{results} with solution results:
\describe{
\item{\verb{results$status}}{status of solved problem;}
\item{\verb{results$objective}}{optimal value of objective function;}
\item{\verb{results$gap}}{difference between objective value in obtained point and Lower estimate of optimal value;}
\item{\verb{results$optimal.point}}{optimal point;}
\item{\verb{results$risk.constraint.value}}{optimal values of left hand sides of risk constraint;}
\item{\verb{results$risk.constraint.residual}}{residual of risk constraint;}
\item{\verb{results$ineq.constraint.value}}{optimal values of left hand sides of linear inequality constraint;}
\item{\verb{results$ineq.constraint.residual}}{residual of linear inequality constraint;}
\item{\verb{results$eq.constraint.value}}{optimal values of left hand sides of linear equality constraint;}
\item{\verb{results$eq.constraint.residual}}{residual of linear equality constraint;}
\item{\verb{results$function.value}}{optimal values of PSG functions defined in problem;}
\item{\verb{results$loading.time}}{data loading time;}
\item{\verb{results$preprocessing.time}}{preprocessing time;}
\item{\verb{results$solving.time}}{solving time.}
}
}
\description{
Finds a minimum for the problem
\deqn{min_x [risk1(x) + d\cdot x]}
subject to
\deqn{risk2 \leq r}
\deqn{A \cdot x= b}
\deqn{Aeq \cdot x= beq}
\deqn{lb \leq x \leq ub}
where\cr
A, Aeq are matrices;\cr
d, x are vectors;\cr
b, beq, lb, ub are vectors or scalars;\cr
risk1(x), risk2(x)  functions are linear combinations of PSG risk functions, PSG deterministic functions, or PSG utility functions. \href{http://www.aorda.com/html/PSG_Help_HTML/index.html?table_list_of_functions.htm}{List of PSG functions for riskprog}
}
\note{
\itemize{
\item Exponential Utility, Logarithmic Utility, Power Utility, Logarithms Sum, and Logarithms Exponents Sum functions are included in the objective with the negative coefficient -1 by default.  However, you should not set any coefficient for these functions, riskprog sets it automatically.
\item linear, pr_dev,  pr_dev_g, pr_pen, and pr_pen_g PSG functions should NOT be combined  with the vector d.
\item Relative Entropy function (entropyr) can have up to 100,000,000 decision variables if stroptions$linearization=1 is specified. This option may dramatically speedup calculations. In this case,a BULDOZER solver is recommended (options$solver <- "buldozer").
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

input.model <- list()
input.model$risk1 <- "cvar_risk"
input.model$w1 <- 0.95
input.model$H1<-matrix(c(1,4,8,3, 7,5,4,6, 2,8,1,0,0,3,4,9),nrow=4, byrow=TRUE)
input.model$c1 <- c(0.2, 0.11, 0.6, 0.1)
input.model$risk2 <- "avg"
input.model$H2<-matrix(c(1,4,8,3, 7,5,4,6, 2,8,1,0,0,3,4,9),nrow=4, byrow=TRUE)
input.model$c2 <- c(0.2, 0.11, 0.6, 0.1)
input.model$rineq <- -4.5
input.model$Aeq <- matrix(c(1, 1, 1, 1),nrow=1)
input.model$beq <- 1
input.model$lb<-c(0, 0, 0, 0)

options<-list()
options$solver <- "van"
options$precision <- 7
options$stages <- 30

results <- rpsg_riskconstrprog(input.model,options)

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
\code{\link{rpsg_riskprog}}    \code{\link{rpsg_riskratioprog}}
}

