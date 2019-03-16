rpsg_riskprog <- function(model,stroptions = NULL)
{
  problem.list<-list()
  #Risk function in objective
   if (!is.null(model$risk) || !is.null(model$d)) {
     stopifnot(is.character(model$risk) || is.vector(model$d))
     }
   else {stop("Objective is not specified")}

  #Parameter of Risk function in objective
  if (!is.null(model$w)) {
    if (is.numeric(model$w)){as.character(model$w)}
    if (length(model$w)==1){
      key_param<-TRUE
    }
    else if (length(model$w)!=length(model$risk)/2){
      stop("Number of parameters w does not equal the number of risk functions")
    }
    else if (length(model$w)>1){
      key_param<-NULL
      for (i in 1:length(model$w)){
        if (model$w[i]=="na") key_param<-c(key_param,FALSE)
        else key_param<-c(key_param,TRUE)
      }
    }
  }
  else if (length(model$risk)==1) key_param<-FALSE
  else if (length(model$risk)>1) key_param<-rep(FALSE,length(model$risk)/2)
  #length(model$risk)/2

  #Matrix for objective
  if (!is.null(model$H)) {
    if (is.matrix(model$H)||is(model$H,"sparseMatrix")){
      matrix_scenarios<-model$H
      colnames(matrix_scenarios)<-colnames(matrix_scenarios,do.NULL = FALSE, prefix = "x")
      problem.list$matrix_scenarios<-matrix_scenarios
    }
    else if (is.vector(model$H) && length(model$H)==length(model$risk)/2){
      for (j in 1:length(model$H)){
        stopifnot(is.matrix(model$H[[j]])||is(model$H[[j]],"sparseMatrix"))
        eval(parse(text=sprintf("matrix_scenarios_%g<-model$H[[j]]",j)))
        eval(parse(text=sprintf("colnames(matrix_scenarios_%g)<-colnames(matrix_scenarios_%g,do.NULL = FALSE, prefix = \"x\")",j,j)))
        eval(parse(text=sprintf("problem.list$matrix_scenarios_%g<-matrix_scenarios_%g",j,j)))
      }
    }
    else if (is.vector(model$H) && length(model$H)!=length(model$risk)/2) stop("Number of matrices H does not equal the number of risk functions")
    }
  else if (is.null(model$H) && !is.null(model$risk)) {stop("Matrix for objective is not specified")}

  if (!is.null(model$c) && is.vector(model$c)) {
    if (is.matrix(model$H)){
      stopifnot(nrow(model$H)==length(model$c))
      matrix_scenarios<-cbind(matrix_scenarios,model$c)
      colnames(matrix_scenarios)[ncol(matrix_scenarios)]<-"scenario_benchmark"
      problem.list$matrix_scenarios<-matrix_scenarios
    }
    else if (is.vector(model$H) && length(model$c)<=length(model$H)){
      for (j in 1:length(model$c)){
        if (!is.null(model$c[[j]])){
          stopifnot(nrow(model$H[[j]])==length(model$c[[j]]))
          eval(parse(text=sprintf("matrix_scenarios_%g<-cbind(matrix_scenarios_%g,model$c[[%g]])",j,j,j)))
          eval(parse(text=sprintf("colnames(matrix_scenarios_%g)[ncol(matrix_scenarios_%g)]<-\"scenario_benchmark\"",j,j)))
          eval(parse(text=sprintf("problem.list$matrix_scenarios_%g<-matrix_scenarios_%g",j,j)))

        }
      }
    }
  }

  if (!is.null(model$p) && is.vector(model$p)) {
    if (is.matrix(model$H)){
      stopifnot(nrow(model$H)==length(model$p))
      #if (nrow(model$H)==length(model$p)) matrix_scenarios<-cbind(matrix_scenarios,model$p)
      #else if (nrow(model$H)==length(model$p)) matrix_scenarios<-rbind(matrix_scenarios,model$p)
      matrix_scenarios<-cbind(matrix_scenarios,model$p)
      colnames(matrix_scenarios)[ncol(matrix_scenarios)]<-"scenario_probability"
      problem.list$matrix_scenarios<-matrix_scenarios
    }
    else if (is.vector(model$H) && length(model$p)<=length(model$H)){
      for (j in 1:length(model$H)){
        if (!is.null(model$p[[j]])){
          stopifnot(nrow(model$H[[j]])==length(model$p[[j]]))
          eval(parse(text=sprintf("matrix_scenarios_%g<-cbind(matrix_scenarios_%g,model$p[[%g]])",j,j,j)))
          eval(parse(text=sprintf("colnames(matrix_scenarios_%g)[ncol(matrix_scenarios_%g)]<-\"scenario_probability\"",j,j)))
          eval(parse(text=sprintf("problem.list$matrix_scenarios_%g<-matrix_scenarios_%g",j,j)))

        }
      }
    }
  }

  loc_size<-NULL
  max_size<-0
  if (!is.null(model$risk)){
    if (is.vector(model$H)) {
    for (i in 1:length(model$H)){
      loc_size <- c(loc_size,ncol(model$H[[i]]))
    }
    max_size <- max(loc_size)
    }
    else if (is.matrix(model$H)){
      max_size <- ncol(model$H)
    }
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
  key_linearization<-FALSE
  key_stages<-FALSE
  key_types<-FALSE
  key_mip<-FALSE

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
        time<- toString(stroptions$time.limit)}
      else stop("Wrong time limit.")
      }
    if (!is.null(stroptions$linearization))
      {
      if (is.numeric(stroptions$linearization))
        {key_linearization<-TRUE
        linearization<- toString(stroptions$linearization)}
      else stop("Wrong linearization. Linearization: 0|1")
    }
    if (!is.null(stroptions$mip))
    {
      if (is.numeric(stroptions$mip)&&length(stroptions$mip)==1)
      {key_mip<-TRUE
      mip<- toString(stroptions$mip)}
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
  if (key_linearization && key_mip){
    stop("Only one option with value 0 or 1 may be used for objective: linearization or mip")
  }
  else if (key_linearization && !key_mip){
    problem_statement<-sprintf("%s\nObjective: objective_H, linearize = %s",problem_statement,linearization)
  }
  else if (!key_linearization && key_mip){
    problem_statement<-sprintf("%s\nObjective: objective_H, mip = %s",problem_statement,mip)
  }
  else if (!key_linearization && !key_mip){
    problem_statement<-sprintf("%s\nObjective: objective_H",problem_statement)
  }

  if (!is.null(model$risk)){

  if (length(model$risk)==1) {
    if (key_param)
      {
      problem_statement<-sprintf("%s\n  %s(%s,matrix_scenarios)",problem_statement,model$risk,model$w)
      }
    else if (!key_param) problem_statement<-sprintf("%s\n  %s(matrix_scenarios)",problem_statement,model$risk)
  }
  else if(length(model$risk)%%2 != 0) stop("Wrong number of risk functions and their coefficients in objective")
    else if (length(model$risk) == 2) {
      if (key_param) {
        problem_statement<-sprintf("%s\n  %s*%s(%s,matrix_scenarios)",problem_statement,model$risk[1],model$risk[2],model$w)
      }
      else if (!key_param) problem_statement<-sprintf("%s\n  %s*%s(matrix_scenarios)",problem_statement,model$risk[1],model$risk[2])
    }
    else if (length(model$risk)%%2 == 0) {
    for (j in 1:(length(model$risk)/2)){
      if (key_param[j]) {
        problem_statement<-sprintf("%s\n  %s*%s(%s,matrix_scenarios_%g)",problem_statement,model$risk[2*j-1],model$risk[2*j],model$w[j],j)
      }
      else if (!key_param[j]) problem_statement<-sprintf("%s\n  %s*%s(matrix_scenarios_%g)",problem_statement,model$risk[2*j-1],model$risk[2*j],j)
    }
    }
  }

  if (key_lin)
    problem_statement<-sprintf("%s\n  linear(matrix_d)",problem_statement)
  if (key_constr_ineq) {
    problem_statement<-sprintf("%s\nConstraint: inequality, <= vector_bineq",problem_statement)
    problem_statement<-sprintf("%s\n  linearmulti(matrix_Aineq)",problem_statement)
  }
  if (key_constr_eq) {
    problem_statement<-sprintf("%s\nConstraint: equality, == vector_beq",problem_statement)
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
      problem_statement<-sprintf("%s  timelimit = %s",problem_statement,time)
      lock_key=lock_key+1
      }
    else if (key_time && lock_key>1) problem_statement<-sprintf("%s, timelimit = %s",problem_statement,time)
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
