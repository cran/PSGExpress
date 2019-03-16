rpsg_riskconstrprog <- function(model,stroptions = NULL)
{
  problem.list<-list()
  #Risk function in objective
  if (!is.null(model$risk1) || !is.null(model$d)) {stopifnot(is.character(model$risk1)|| is.vector(model$d))}
  else {stop("Objective is not specified")}

  #Parameter of Risk function in objective
  if (!is.null(model$w1)) {
    if (is.numeric(model$w1)){as.character(model$w1)}
    if (length(model$w1)==1){
      key_param_1<-TRUE
    }
    else if (length(model$w1)!=length(model$risk1)/2){
      stop("Number of parameters w does not equal the number of risk functions in objective")
    }
    else if (length(model$w1)>1){
      key_param_1<-NULL
      for (i in 1:length(model$w1)){
        if (model$w1[i]=="na") key_param_1<-c(key_param_1,FALSE)
        else key_param_1<-c(key_param_1,TRUE)
      }
    }
  }
  else if (length(model$risk1)==1) key_param_1<-FALSE
  else if (length(model$risk1)>1) key_param_1<-rep(FALSE,length(model$risk1)/2)


  #Risk function in constraint
  if (!is.null(model$risk2)) {stopifnot(is.character(model$risk2))}
  else {stop("Risk function in constraint is not specified")}

  #Parameter of Risk function in constraint
  if (!is.null(model$w2)) {
    if (is.numeric(model$w2)){as.character(model$w2)}
    if (length(model$w2)==1){
      key_param_2<-TRUE
    }
    else if (length(model$w2)!=length(model$risk2)/2){
      stop("Number of parameters w does not equal the number of risk functions in constraint")
    }
    else if (length(model$w2)>1){
      key_param_2<-NULL
      for (i in 1:length(model$w2)){
        if (model$w2[i]=="na") key_param_2<-c(key_param_2,FALSE)
        else key_param_2<-c(key_param_2,TRUE)
      }
    }
  }
  else if (length(model$risk2)==1) key_param_2<-FALSE
  else if (length(model$risk2)>1) key_param_2<-rep(FALSE,length(model$risk2)/2)

  # Matrix for objective
  if (!is.null(model$H1)) {
    if (is.matrix(model$H1)||is(model$H1,"sparseMatrix")){
        matrix_scenarios_h1<-model$H1
        colnames(matrix_scenarios_h1)<-colnames(matrix_scenarios_h1,do.NULL = FALSE, prefix = "x")
        problem.list$matrix_scenarios_h1<-matrix_scenarios_h1
      }
    else if (is.vector(model$H1) && length(model$H1)==length(model$risk1)/2){
      for (j in 1:length(model$H1)){
        stopifnot(is.matrix(model$H1[[j]])||is(model$H1[[j]],"sparseMatrix"))
        eval(parse(text=sprintf("matrix_scenarios_h1_%g<-model$H1[[j]]",j)))
        eval(parse(text=sprintf("colnames(matrix_scenarios_h1_%g)<-colnames(matrix_scenarios_h1_%g,do.NULL = FALSE, prefix = \"x\")",j,j)))
        #colnames(matrix_scenarios_h1)<-colnames(matrix_scenarios_h1,do.NULL = FALSE, prefix = "x")
        eval(parse(text=sprintf("problem.list$matrix_scenarios_h1_%g<-matrix_scenarios_h1_%g",j,j)))
        #problem.list$matrix_scenarios_h1<-matrix_scenarios_h1
      }
    }
    else if (is.vector(model$H1) && length(model$H1)!=length(model$risk1)/2) stop("Number of matrices H does not equal the number of risk functions")
  }
  else if (is.null(model$H1) && !is.null(model$risk1)) {stop("Matrix for objective is not specified")}

  if (!is.null(model$c1) && is.vector(model$c1)) {
    if (is.matrix(model$H1)) {
      stopifnot(nrow(model$H1)==length(model$c1))
      matrix_scenarios_h1<-cbind(matrix_scenarios_h1,model$c1)
      colnames(matrix_scenarios_h1)[ncol(matrix_scenarios_h1)]<-"scenario_benchmark"
      problem.list$matrix_scenarios_h1<-matrix_scenarios_h1
    }
    else if (is.vector(model$H1)){
      stopifnot(length(model$c1)<=length(model$H1))
      for (j in 1:length(model$c1)){
        if (!is.null(model$c1[[j]])){
          stopifnot(nrow(model$H1[[j]])==length(model$c1[[j]]))
          eval(parse(text=sprintf("matrix_scenarios_h1_%g<-cbind(matrix_scenarios_h1_%g,model$c1[[%g]])",j,j,j)))
          eval(parse(text=sprintf("colnames(matrix_scenarios_h1_%g)[ncol(matrix_scenarios_h1_%g)]<-\"scenario_benchmark\"",j,j)))
          eval(parse(text=sprintf("problem.list$matrix_scenarios_h1_%g<-matrix_scenarios_h1_%g",j,j)))

        }
      }
    }
  }

  if (!is.null(model$p1) && is.vector(model$p1)) {
    if (is.matrix(model$H1)){
      stopifnot(nrow(model$H1)==length(model$p1))
      #if (nrow(model$H1)==length(model$p1)) matrix_scenarios_h1<-cbind(matrix_scenarios_h1,model$p1)
      #else if (nrow(model$H1)==length(model$p1)) matrix_scenarios_h1<-rbind(matrix_scenarios_h1,model$p1)
      matrix_scenarios_h1<-cbind(matrix_scenarios_h1,model$p1)
      colnames(matrix_scenarios_h1)[ncol(matrix_scenarios_h1)]<-"scenario_probability"
      problem.list$matrix_scenarios_h1<-matrix_scenarios_h1
    }
    else if (is.vector(model$H1)){
      stopifnot(length(model$p1)<=length(model$H1))
      for (j in 1:length(model$p1)){
        if (!is.null(model$p1[[j]])){
          stopifnot(nrow(model$H1[[j]])==length(model$p1[[j]]))
          #if (nrow(model$H1[[j]])==length(model$p1[[j]])) eval(parse(text=sprintf("matrix_scenarios_h1_%g<-cbind(matrix_scenarios_h1_%g,model$p1[[%g]])",j,j,j)))
          #else if (nrow(model$H1[[j]])==length(model$p1[[j]])) eval(parse(text=sprintf("matrix_scenarios_h1_%g<-rbind(matrix_scenarios_h1_%g,model$p1[[%g]])",j,j,j)))
          eval(parse(text=sprintf("matrix_scenarios_h1_%g<-cbind(matrix_scenarios_h1_%g,model$p1[[%g]])",j,j,j)))
          eval(parse(text=sprintf("colnames(matrix_scenarios_h1_%g)[ncol(matrix_scenarios_h1_%g)]<-\"scenario_probability\"",j,j)))
          eval(parse(text=sprintf("problem.list$matrix_scenarios_h1_%g<-matrix_scenarios_h1_%g",j,j)))

        }
      }
    }
  }


  ###########################################################
  # Matrix for risk constraint
  if (!is.null(model$H2)) {
    if (is.matrix(model$H2)||is(model$H2,"sparseMatrix")){
        matrix_scenarios_h2<-model$H2
        colnames(matrix_scenarios_h2)<-colnames(matrix_scenarios_h2,do.NULL = FALSE, prefix = "x")
        problem.list$matrix_scenarios_h2<-matrix_scenarios_h2
    }
    else if (is.vector(model$H2) && length(model$H2)==length(model$risk1)/2){
      for (j in 1:length(model$H2)){
        stopifnot(is.matrix(model$H2[[j]])||is(model$H2[[j]],"sparseMatrix"))
        eval(parse(text=sprintf("matrix_scenarios_h2_%g<-model$H2[[j]]",j)))
        eval(parse(text=sprintf("colnames(matrix_scenarios_h2_%g)<-colnames(matrix_scenarios_h2_%g,do.NULL = FALSE, prefix = \"x\")",j,j)))
        #colnames(matrix_scenarios_h2)<-colnames(matrix_scenarios_h2,do.NULL = FALSE, prefix = "x")
        eval(parse(text=sprintf("problem.list$matrix_scenarios_h2_%g<-matrix_scenarios_h2_%g",j,j)))
        #problem.list$matrix_scenarios_h2<-matrix_scenarios_h2
      }
    }
    else if (is.vector(model$H2) && length(model$H2)!=length(model$risk2)/2) stop("Number of matrices H2 does not equal the number of risk functions in constraint")
  }
  else if (is.null(model$H2) && !is.null(model$risk2)) {stop("Matrix for risk in constraint is not specified")}

  if (!is.null(model$c2) && is.vector(model$c2)) {
    if (is.matrix(model$H2)){
      stopifnot(nrow(model$H2)==length(model$c2))
      #if (nrow(model$H2)==length(model$c2)) matrix_scenarios_h2<-cbind(matrix_scenarios_h2,model$c2)
      #else if (nrow(model$H2)==length(model$c2)) matrix_scenarios_h2<-rbind(matrix_scenarios_h2,model$c2)
      matrix_scenarios_h2<-cbind(matrix_scenarios_h2,model$c2)
      colnames(matrix_scenarios_h2)[ncol(matrix_scenarios_h2)]<-"scenario_benchmark"
      problem.list$matrix_scenarios_h2<-matrix_scenarios_h2
    }
    else if (is.vector(model$H2)){
      stopifnot(length(model$c2)<=length(model$H2))
      for (j in 1:length(model$c2)){
        if (!is.null(model$c2[[j]])){
          stopifnot(nrow(model$H2[[j]])==length(model$c2[[j]]))
          #if (nrow(model$H2[[j]])==length(model$c2[[j]])) eval(parse(text=sprintf("matrix_scenarios_h2_%g<-cbind(matrix_scenarios_h2_%g,model$c2[[%g]])",j,j,j)))
          #else if (nrow(model$H2[[j]])==length(model$c2[[j]])) eval(parse(text=sprintf("matrix_scenarios_h2_%g<-rbind(matrix_scenarios_h2_%g,model$c2[[%g]])",j,j,j)))
          eval(parse(text=sprintf("matrix_scenarios_h2_%g<-cbind(matrix_scenarios_h2_%g,model$c2[[%g]])",j,j,j)))
          eval(parse(text=sprintf("colnames(matrix_scenarios_h2_%g)[ncol(matrix_scenarios_h2_%g)]<-\"scenario_benchmark\"",j,j)))
          eval(parse(text=sprintf("problem.list$matrix_scenarios_h2_%g<-matrix_scenarios_h2_%g",j,j)))

        }
      }
    }
  }

  if (!is.null(model$p2) && is.vector(model$p2)) {
    if (is.matrix(model$H2)){
      stopifnot(nrow(model$H2)==length(model$p2))
      #if (nrow(model$H2)==length(model$p2)) matrix_scenarios_h2<-cbind(matrix_scenarios_h2,model$p2)
      #else if (nrow(model$H2)==length(model$p2)) matrix_scenarios_h2<-rbind(matrix_scenarios_h2,model$p2)
      matrix_scenarios_h2<-cbind(matrix_scenarios_h2,model$p2)
      colnames(matrix_scenarios_h2)[ncol(matrix_scenarios_h2)]<-"scenario_probability"
      problem.list$matrix_scenarios_h2<-matrix_scenarios_h2
    }
    else if (is.vector(model$H2)){
      stopifnot(length(model$p2)<=length(model$H2))
      for (j in 1:length(model$p2)){
        if (!is.null(model$p2[[j]])){
          stopifnot(nrow(model$H2[[j]])==length(model$p2[[j]]))
          #if (nrow(model$H2[[j]])==length(model$p2[[j]])) eval(parse(text=sprintf("matrix_scenarios_h2_%g<-cbind(matrix_scenarios_h2_%g,model$p2[[%g]])",j,j,j)))
          #else if (nrow(model$H2[[j]])==length(model$p2[[j]])) eval(parse(text=sprintf("matrix_scenarios_h2_%g<-rbind(matrix_scenarios_h2_%g,model$p2[[%g]])",j,j,j)))
          eval(parse(text=sprintf("matrix_scenarios_h2_%g<-cbind(matrix_scenarios_h2_%g,model$p2[[%g]])",j,j,j)))
          eval(parse(text=sprintf("colnames(matrix_scenarios_h2_%g)[ncol(matrix_scenarios_h2_%g)]<-\"scenario_probability\"",j,j)))
          eval(parse(text=sprintf("problem.list$matrix_scenarios_h2_%g<-matrix_scenarios_h2_%g",j,j)))

        }
      }
    }
  }

  # Define max number of elements
  loc_size_1<-0
  loc_size_2<-0
  if (!is.null(model$risk1)){
    if (is.vector(model$H1)) {
      for (i in 1:length(model$H1)){
        loc_size_1 <- c(loc_size_1,ncol(model$H1[[i]]))
      }
    }
    else if (is.matrix(model$H1)) {loc_size_1<-ncol(model$H1)}
  }

  if (is.vector(model$H2)) {
    for (i in 1:length(model$H2)){
      loc_size_2 <- c(loc_size_2,ncol(model$H2[[i]]))
    }
  }
  else if (is.matrix(model$H2)) {loc_size_2<-ncol(model$H2)}

  max_size <- max(c(loc_size_1,loc_size_2))

  ###########################################################
  # Risk Constraint
  if (is.null(model$rineq)) stop("Value for rineq is not specified")
  else {stopifnot(is.vector(model$rineq),length(model$rineq)==1, is.numeric(model$rineq))
    problem.list$rineq<-model$rineq
  }


  #Linear component
  if (!is.null(model$d)) {stopifnot(is.vector(model$d))
    matrix_d<-matrix(model$d,nrow=1,byrow = TRUE)
    colnames(matrix_d)<-colnames(matrix_d,do.NULL = FALSE, prefix = "x")
    key_lin = TRUE
    problem.list$matrix_d<-matrix_d
    max_size <- max(max_size,length(model$d))
  }
  else key_lin = FALSE

  #Constraint inequality
  key_constr_ineq = FALSE
  if (!is.null(model$Aineq)) {stopifnot(is.matrix(model$Aineq)||is(model$Aineq,"sparseMatrix"))
    matrix_Aineq<-model$Aineq
    colnames(matrix_Aineq)<-colnames(matrix_Aineq,do.NULL = FALSE, prefix = "x")
    problem.list$matrix_Aineq<-matrix_Aineq
    if (is.null(model$bineq)) stop("Vector bineq is not specified for existing matrix Aineq")
    else {stopifnot(is.vector(model$bineq),length(model$bineq)==nrow(matrix_Aineq))
      #vector_b<-vector(model$b)
      problem.list$vector_bineq<-model$bineq
      key_constr_ineq = TRUE
    }
  }
  else if (!is.null(model$bineq)) stop("Matrix Aineq is not specified for existing vector bineq")

  #Constraint equality
  key_constr_eq = FALSE
  if (!is.null(model$Aeq)) {stopifnot(is.matrix(model$Aeq)||is(model$Aeq,"sparseMatrix"))
    matrix_Aeq<-model$Aeq
    colnames(matrix_Aeq)<-colnames(matrix_Aeq,do.NULL = FALSE, prefix = "x")
    problem.list$matrix_Aeq<-matrix_Aeq
    if (is.null(model$beq)) stop("Vector beq is not specified for existing matrix Aeq")
    else {stopifnot(is.vector(model$beq),length(model$beq)==nrow(matrix_Aeq))
      #vector_beq<-model$beq
      problem.list$vector_beq<-model$beq
      key_constr_eq = TRUE
    }
  }
  else if (!is.null(model$beq)) stop("Matrix Aeq is not specified for existing vector beq")

  # Box of variables
  key_box_lb = FALSE
  if (!is.null(model$lb))
  {stopifnot(is.vector(model$lb))
    if (length(model$lb)==1){
      point_lb<-rep(model$lb,max_size)}
    else point_lb<-model$lb

    names(point_lb)<-rownames(point_lb,do.NULL = FALSE, prefix = "x")
    problem.list$point_lb<-point_lb
    key_box_lb = TRUE
  }

  key_box_ub = FALSE
  if (!is.null(model$ub))
  {stopifnot(is.vector(model$ub))
    if (length(model$ub)==1){
      point_ub<-rep(model$ub,max_size)}
    else point_ub<-model$ub

    names(point_ub)<-rownames(point_ub,do.NULL = FALSE, prefix = "x")
    problem.list$point_ub<-point_ub
    key_box_ub = TRUE
  }

  #Solver
  key_initpoint = FALSE
  if (!is.null(model$x0))
  {stopifnot(is.vector(model$x0))
    if (length(model$x0)==1){
      point_x0<-rep(model$x0,max_size)}
    else point_x0<-model$x0

    names(point_x0)<-rownames(point_x0,do.NULL = FALSE, prefix = "x")
    problem.list$point_x0<-point_x0
    key_initpoint = TRUE
  }

  #######################################################
  # Options
  #######################################################

  key_solver <- FALSE
  key_precision<-FALSE
  key_time<-FALSE
  key_linearization_1<-FALSE
  key_linearization_2<-FALSE
  key_stages<-FALSE
  key_types<-FALSE
  key_mip_1<-FALSE
  key_mip_2<-FALSE

  if (!is.null(stroptions))
  {
    if (!is.null(stroptions$solver))
    {
      if (is.character(stroptions$solver))
      {key_solver<-TRUE
      solver<- stroptions$solver}
      else stop("Wrong solver. Solver: VAN CAR BULDOZER TANK")
    }
    if (!is.null(stroptions$precision))
    {
      if (is.numeric(stroptions$precision))
      {key_precision<-TRUE
      precision<- toString(stroptions$precision)}
      else stop("Wrong precision. Precision: INT(1..9) ")
    }
    if (!is.null(stroptions$time.limit))
    {
      if (is.numeric(stroptions$time.limit))
      {key_time<-TRUE
      timelimit<- toString(stroptions$time.limit)}
      else stop("Wrong time limit.")
    }
    if (!is.null(stroptions$linearization1))
    {
      if (is.numeric(stroptions$linearization1))
      {key_linearization_1<-TRUE
      linearization_1<- toString(stroptions$linearization1)}
      else stop("Wrong linearization. Linearization: 0|1")
    }
    if (!is.null(stroptions$linearization2))
    {
      if (is.numeric(stroptions$linearization2))
      {key_linearization_2<-TRUE
      linearization_2<- toString(stroptions$linearization2)}
      else stop("Wrong linearization. Linearization: 0|1")
    }
    if (!is.null(stroptions$mip1))
    {
      if (is.numeric(stroptions$mip1)&&length(stroptions$mip1)==1)
      {key_mip_1<-TRUE
      mip_1<- toString(stroptions$mip1)}
      else stop("Wrong mip option. mip: 0|1")
    }
    if (!is.null(stroptions$mip2))
    {
      if (is.numeric(stroptions$mip2)&&length(stroptions$mip2)==1)
      {key_mip_2<-TRUE
      mip_2<- toString(stroptions$mip2)}
      else stop("Wrong mip option. mip: 0|1")
    }
    if (!is.null(stroptions$stages))
    {
      if (is.numeric(stroptions$stages))
      {key_stages<-TRUE
      stages<- toString(stroptions$stages)}
      else stop("Wrong stages. Stages: INT(1..30)")
    }
    if (!is.null(stroptions$types))
    {
      if (is.vector(stroptions$types)&&is.numeric(stroptions$types))
      {
        key_types<-TRUE
        if (length(stroptions$types)==1){
          point_types<-rep(stroptions$types,max_size)}
        else point_types<-stroptions$types

        names(point_types)<-rownames(point_types,do.NULL = FALSE, prefix = "x")
        problem.list$point_types<-point_types
      }
      else stop("Wrong types. Stages: vector|0|1|2")
    }
  }

  #######################################################
  # Create problem_statement
  #######################################################

  problem_statement<-"Problem: problem_risk, type = minimize"

  # Objective
  if (key_linearization_1 && key_mip_1){
    stop("Only one option with value 0 or 1 may be used for objective: linearization or mip")
  }
  else if (key_linearization_1 && !key_mip_1){
    problem_statement<-sprintf("%s\nObjective: objective_H1, linearize = %s",problem_statement,linearization_1)
  }
  else if (!key_linearization_1 && key_mip_1){
    problem_statement<-sprintf("%s\nObjective: objective_H1, mip = %s",problem_statement,mip_1)
  }
  else if (!key_linearization_1 && !key_mip_1){
    problem_statement<-sprintf("%s\nObjective: objective_H1",problem_statement)
  }

  if (!is.null(model$risk1)){

  if (length(model$risk1)==1) {
    if (key_param_1)
    {
      problem_statement<-sprintf("%s\n  %s(%s,matrix_scenarios_h1)",problem_statement,model$risk1,model$w1)
    }
    else if (!key_param_1) problem_statement<-sprintf("%s\n  %s(matrix_scenarios_h1)",problem_statement,model$risk1)
  }
  else if(length(model$risk1)%%2 != 0) stop("Wrong number of risk1 functions and their coefficients in objective")
  else if (length(model$risk1) == 2) {
      if (key_param_1) {
        problem_statement<-sprintf("%s\n  %s*%s(%s,matrix_scenarios_h1)",problem_statement,model$risk1[1],model$risk1[2],model$w1)
      }
      else if (!key_param_1) problem_statement<-sprintf("%s\n  %s*%s(matrix_scenarios_h1)",problem_statement,model$risk1[1],model$risk1[2])
  }
    else if (length(model$risk1)%%2 == 0 && length(model$risk1)>2) {
    for (j in 1:(length(model$risk1)/2)){
      if (key_param_1[j]) {
        problem_statement<-sprintf("%s\n  %s*%s(%s,matrix_scenarios_h1_%g)",problem_statement,model$risk1[2*j-1],model$risk1[2*j],model$w1[j],j)
      }
      else if (!key_param_1[j]) problem_statement<-sprintf("%s\n  %s*%s(matrix_scenarios_h1_%g)",problem_statement,model$risk1[2*j-1],model$risk1[2*j],j)
    }
  }
  }
  if (key_lin)
    problem_statement<-sprintf("%s\n  linear(matrix_d)",problem_statement)

  # Add risk constraint
  #Constraint: constraint_risk, upper_bound =
  if (key_linearization_2 && key_mip_2){
    stop("Only one option with value 0 or 1 may be used for constraint: linearization or mip")
  }
  else if (key_linearization_2 && !key_mip_2){
    problem_statement<-sprintf("%s\nConstraint: constraint_risk, <= %g, linearize = %s",problem_statement,problem.list$rineq,linearization_2)
  }
  else if (!key_linearization_2 && key_mip_2){
    problem_statement<-sprintf("%s\nConstraint: constraint_risk, <= %g, mip = %s",problem_statement,problem.list$rineq,mip_2)
  }
  else if (!key_linearization_2 && !key_mip_2){
    problem_statement<-sprintf("%s\nConstraint: constraint_risk, <= %g",problem_statement,problem.list$rineq)
  }

  if (length(model$risk2)==1) {
    if (key_param_2)
    {
      problem_statement<-sprintf("%s\n  %s(%s,matrix_scenarios_h2)",problem_statement,model$risk2,model$w2)
    }
    else if (!key_param_2) problem_statement<-sprintf("%s\n  %s(matrix_scenarios_h2)",problem_statement,model$risk2)
  }
  else if(length(model$risk2)%%2 != 0) stop("Wrong number of risk2 functions and their coefficients in constraint")
  else if (length(model$risk2) == 2) {
      if (key_param_2) {
        problem_statement<-sprintf("%s\n  %s*%s(%s,matrix_scenarios_h2)",problem_statement,model$risk2[1],model$risk2[2],model$w2)
      }
      else if (!key_param_2) problem_statement<-sprintf("%s\n  %s*%s(matrix_scenarios_h2)",problem_statement,model$risk2[1],model$risk2[2])
  }
  else if (length(model$risk2)%%2 == 0 && length(model$risk2)>2) {
    for (j in 1:(length(model$risk2)/2)){
      if (key_param_2[j]) {
        problem_statement<-sprintf("%s\n  %s*%s(%s,matrix_scenarios_h2_%g)",problem_statement,model$risk2[2*j-1],model$risk2[2*j],model$w2[j],j)
      }
      else if (!key_param_2[j]) problem_statement<-sprintf("%s\n  %s*%s(matrix_scenarios_h2_%g)",problem_statement,model$risk2[2*j-1],model$risk2[2*j],j)
    }
  }


  if (key_constr_ineq) {
    problem_statement<-sprintf("%s\nConstraint:inequality, <= vector_bineq",problem_statement)
    problem_statement<-sprintf("%s\n  linearmulti(matrix_Aineq)",problem_statement)
  }
  if (key_constr_eq) {
    problem_statement<-sprintf("%s\nConstraint:equality, == vector_beq",problem_statement)
    problem_statement<-sprintf("%s\n  linearmulti(matrix_Aeq)",problem_statement)
  }
  key_box<-FALSE
  if (key_box_lb && key_box_ub) {
    problem_statement<-sprintf("%s\nBox: lowerbounds = point_lb, upperbounds = point_ub",problem_statement)
    key_box<-TRUE
  }
  else if (key_box_lb && !key_box_ub) {
    problem_statement<-sprintf("%s\nBox: lowerbounds = point_lb",problem_statement)
    key_box<-TRUE
  }
  else if (!key_box_lb && key_box_ub) {
    problem_statement<-sprintf("%s\nBox: upperbounds = point_ub",problem_statement)
    key_box<-TRUE
  }
  if (key_types && key_box)
  {
    problem_statement<-sprintf("%s, types = point_types",problem_statement)
  }

  # Solver
  if (key_solver||key_precision||key_stages||key_time||key_initpoint) {
    problem_statement<-sprintf("%s\nSolver: ",problem_statement)
    lock_key = 1
    if (key_solver)
    {
      problem_statement<-sprintf("%s %s",problem_statement,solver)
      lock_key=lock_key+1
    }
    if (key_precision && lock_key==1)
    {
      problem_statement<-sprintf("%s precision = %s",problem_statement,precision)
      llock_key=lock_key+1
    }
    else if (key_precision && lock_key>1) problem_statement<-sprintf("%s, precision = %s",problem_statement,precision)
    if (key_stages && lock_key==1)
    {
      problem_statement<-sprintf("%s stages = %s",problem_statement,stages)
      lock_key=lock_key+1
    }
    else if (key_stages && lock_key>1) problem_statement<-sprintf("%s, stages = %s",problem_statement,stages)
    if (key_time && lock_key==1)
    {
      problem_statement<-sprintf("%s  timelimit = %s",problem_statement,timelimit)
      lock_key=lock_key+1
    }
    else if (key_time && lock_key>1) problem_statement<-sprintf("%s, timelimit = %s",problem_statement,timelimit)
    if (key_initpoint && lock_key==1)
    {
      problem_statement<-sprintf("%s init_point = point_x0",problem_statement)
      lock_key=lock_key+1
    }
    else if (key_initpoint && lock_key>1) problem_statement<-sprintf("%s, init_point = point_x0",problem_statement)
  }

  problem.list$problem_statement<-problem_statement

  #######################################################
  # Export problem
  #######################################################

  #save.to.text
  if (!is.null(stroptions$save.to.text))
  {
    if (is.character(stroptions$save.to.text))
    {
      rpsg_exporttotext(stroptions$save.to.text, problem.list)
    }
  }

  #save.to.list
  #if (!is.null(stroptions$save.to.list))
  #{
  #  if (is.character(stroptions$save.to.list))
  #  {
  #    new.variable.name<-make.names(stroptions$save.to.list, unique = TRUE);
  #    assign(new.variable.name,problem.list,envir = .GlobalEnv)
  #  }
  #  else stop("Wrong save.to.list option")
  #}

   problem.res <- rpsg_solver(problem.list)

  resulted.list<-rpsg_getsolution(problem.res)

  # Create output list
  resulted.list.new <-list()

  resulted.list.new <- list()
  resulted.list.new$status <- resulted.list$status
  resulted.list.new$objective <- resulted.list$objective
  resulted.list.new$gap <- resulted.list$gap
  resulted.list.new$optimal.point <- resulted.list$point_problem_risk

  resulted.list.new$risk.constraint.value <- resulted.list$constraint.value[["constraint_risk"]]
  resulted.list.new$risk.constraint.residual <- resulted.list$point_slack_constraints_problem_risk[["constraint_risk"]]

  if (key_constr_ineq) {
    resulted.list.new$ineq.constraint.value <- resulted.list$vector_constraint_inequality
    resulted.list.new$ineq.constraint.residual <- resulted.list$vector_slack_constraint_inequality
  }

  if (key_constr_eq) {
    resulted.list.new$eq.constraint.value <- resulted.list$vector_constraint_equality
    resulted.list.new$eq.constraint.residual <- resulted.list$vector_slack_constraint_equality
  }

  resulted.list.new$function.value <- resulted.list$function.value

  resulted.list.new$loading.time <- resulted.list$loading.time
  resulted.list.new$preprocessing.time <- resulted.list$preprocessing.time
  resulted.list.new$solving.time <- resulted.list$solving.time

  return(resulted.list.new)
}
