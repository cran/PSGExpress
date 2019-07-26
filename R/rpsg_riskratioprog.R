rpsg_riskratioprog <- function(model,stroptions = NULL)
{
  problem.list<-list()
  #Risk function in objective
  if (!is.null(model$risk1) || !is.null(model$d)) {
    stopifnot(is.character(model$risk1)|| is.vector(model$d))
    resultf1<-check.functions.ratioprog(model$risk1)
    if (!resultf1$adm){stop("Inadmissible risk function in objective")}
    }
  else {stop("Risk function in objective is not specified")}

  #Parameter of Risk function in objective
  key_param_1<-FALSE
  if (!is.null(model$w1)) {
    if (is.numeric(model$w1)){as.character(model$w1)}
    if (resultf1$param == 0) {
      warning("Function in objective does not involve parameter")
      key_param_1<-FALSE
    }
    else {key_param_1<-TRUE}
  }

  #Risk function in constraint
  if (!is.null(model$risk2)) {
    stopifnot(is.character(model$risk2))
    resultf2<-check.functions.ratioprog(model$risk2)
    if (!resultf2$adm){stop("Inadmissible risk function in constraint")}
    }
  else {stop("Risk function in constraint is not specified")}

  #Parameter of Risk function in constraint
  key_param_2<-FALSE
  if (!is.null(model$w2)) {
    if (is.numeric(model$w2)){as.character(model$w2)}
    if (resultf2$param == 0) {
      warning("Function in constraint does not involve parameter")
      key_param_2<-FALSE
    }
    else {key_param_2<-TRUE}
  }

  # Matrix for objective
  if (!is.null(model$H1)) {
    if (is.matrix(model$H1)||is(model$H1,"sparseMatrix")){
      matrix_scenarios_h1<-model$H1
      #colnames(matrix_scenarios_h1)<-colnames(matrix_scenarios_h1,do.NULL = FALSE, prefix = "x")
      #problem.list$matrix_scenarios_h1<-matrix_scenarios_h1
    }
  }
  else if (is.null(model$H1) && !is.null(model$risk1)) {stop("Matrix for objective is not specified")}

  if (is.null(model$c1)){
    if (resultf1$param == 1){
      this.c1<-rep(model$w1,nrow(model$H1))
    }
    else {this.c1<-rep(0,nrow(model$H1))}
    }

  if (!is.null(model$c1)) {
      stopifnot(is.vector(model$c1))
      stopifnot(nrow(model$H1)==length(model$c1))
      if (resultf1$param == 1){
        this.c1<-rep(model$w1,nrow(model$H1))-model$c1
      }
      else {this.c1<- -model$c1}
  }
  colnames(matrix_scenarios_h1)<-colnames(matrix_scenarios_h1,do.NULL = FALSE, prefix = "x")
  matrix_scenarios_h1<-cbind(matrix_scenarios_h1,this.c1,deparse.level = 0)
  colnames(matrix_scenarios_h1)[ncol(matrix_scenarios_h1)]<-"y0"
  problem.list$matrix_scenarios_h1<-matrix_scenarios_h1


  if (!is.null(model$p1) && is.vector(model$p1)) {
      stopifnot(nrow(model$H1)==length(model$p1))
      matrix_scenarios_h1<-cbind(matrix_scenarios_h1,model$p1)
      colnames(matrix_scenarios_h1)[ncol(matrix_scenarios_h1)]<-"scenario_probability"
      problem.list$matrix_scenarios_h1<-matrix_scenarios_h1
  }


  ###########################################################
  # Matrix for risk constraint
  if (!is.null(model$H2)) {
    if (is.matrix(model$H2)||is(model$H2,"sparseMatrix")){
      matrix_scenarios_h2<-model$H2
      #colnames(matrix_scenarios_h2)<-colnames(matrix_scenarios_h2,do.NULL = FALSE, prefix = "x")
      #problem.list$matrix_scenarios_h2<-matrix_scenarios_h2
    }
   }
  else if (is.null(model$H2) && !is.null(model$risk2)) {stop("Matrix for risk in constraint is not specified")}

  if (is.null(model$c2)){
    if (resultf2$param == 1){
      this.c2<-rep(model$w2,nrow(model$H2))
    }
    else {this.c2<-rep(0,nrow(model$H2))}
  }

  if (!is.null(model$c2)) {
    stopifnot(is.vector(model$c2))
    stopifnot(nrow(model$H2)==length(model$c2))
    if (resultf2$param == 1){
      this.c2<-rep(model$w2,nrow(model$H2))-model$c2
    }
    else {this.c2<- -model$c2}
  }

  colnames(matrix_scenarios_h2)<-colnames(matrix_scenarios_h2,do.NULL = FALSE, prefix = "x")
  matrix_scenarios_h2<-cbind(matrix_scenarios_h2,this.c2,deparse.level = 0)
  colnames(matrix_scenarios_h2)[ncol(matrix_scenarios_h2)]<-"y0"
  problem.list$matrix_scenarios_h2<-matrix_scenarios_h2

  if (!is.null(model$p2) && is.vector(model$p2)) {
      stopifnot(nrow(model$H2)==length(model$p2))
      matrix_scenarios_h2<-cbind(matrix_scenarios_h2,model$p2)
      colnames(matrix_scenarios_h2)[ncol(matrix_scenarios_h2)]<-"scenario_probability"
      problem.list$matrix_scenarios_h2<-matrix_scenarios_h2
  }

  # Define max number of elements
  loc_size_1<-ncol(model$H1)
  loc_size_2<-ncol(model$H2)
  max_size <- max(c(loc_size_1,loc_size_2))

  ###########################################################

  #Linear component
  if (!is.null(model$d)) {stopifnot(is.vector(model$d))
    stopifnot(length(model$d)==max_size)
    this.d <- c(model$d,0)
    matrix_d<-matrix(this.d,nrow=1,byrow = TRUE)
    colnames(matrix_d)<-colnames(matrix_d,do.NULL = FALSE, prefix = "x")
    colnames(matrix_d)[ncol(matrix_d)]<-"y0"
    key_lin <- TRUE
    problem.list$matrix_d<-matrix_d
  }
  else key_lin <- FALSE

  ###########################################################

  #Constraint inequality
  key_constr_ineq <- FALSE
  if (!is.null(model$Aineq)) {stopifnot(is.matrix(model$Aineq)||is(model$Aineq,"sparseMatrix"))
    if (is.null(model$bineq)) stop("Vector bineq is not specified for existing matrix Aineq")
    stopifnot(is.vector(model$bineq),length(model$bineq)==nrow(model$Aineq))

    matrix_Aineq<-cbind(model$Aineq,-model$bineq,deparse.level = 0)
    colnames(matrix_Aineq)<-colnames(matrix_Aineq,do.NULL = FALSE, prefix = "x")
    colnames(matrix_Aineq)[ncol(matrix_Aineq)]<-"y0"
    problem.list$matrix_Aineq<-matrix_Aineq
    key_constr_ineq <- TRUE
  }
  else if (!is.null(model$bineq)) stop("Matrix Aineq is not specified for existing vector bineq")

  ###########################################################

  #Constraint equality
  key_constr_eq <- FALSE
  if (!is.null(model$Aeq)) {stopifnot(is.matrix(model$Aeq)||is(model$Aeq,"sparseMatrix"))
    if (is.null(model$beq)) stop("Vector beq is not specified for existing matrix Aeq")
    stopifnot(is.vector(model$beq),length(model$beq)==nrow(model$Aeq))

    matrix_Aeq<-cbind(model$Aeq,-model$beq,deparse.level = 0)
    colnames(matrix_Aeq)<-colnames(matrix_Aeq,do.NULL = FALSE, prefix = "x")
    colnames(matrix_Aeq)[ncol(matrix_Aeq)]<-"y0"
    problem.list$matrix_Aeq<-matrix_Aeq
    key_constr_eq <- TRUE
  }
  else if (!is.null(model$beq)) stop("Matrix Aeq is not specified for existing vector beq")

  ###########################################################

  # Box of variables
  key_matrix_lb <- FALSE
  if (!is.null(model$lb))
  {stopifnot(is.vector(model$lb))
    if (length(model$lb)==1){
      point_lb<-rep(model$lb,max_size)}
    else point_lb<-model$lb
    this.lb <- - diag(length(point_lb))
    this.lb <- cbind(this.lb,point_lb,deparse.level = 0)
    colnames(this.lb)<-colnames(this.lb,do.NULL = FALSE, prefix = "x")
    colnames(this.lb)[ncol(this.lb)]<-"y0"
    problem.list$matrix_lb<-this.lb
    key_matrix_lb <- TRUE
  }

  key_matrix_ub <- FALSE
  if (!is.null(model$ub))
  {stopifnot(is.vector(model$ub))
    if (length(model$ub)==1){
      point_ub<-rep(model$ub,max_size)}
    else point_ub<-model$ub

    this.ub <- - diag(length(point_ub))
    this.ub <- cbind(this.ub,point_ub,deparse.level = 0)
    colnames(this.ub)<-colnames(this.ub,do.NULL = FALSE, prefix = "x")
    colnames(this.ub)[ncol(this.ub)]<-"y0"
    problem.list$matrix_ub<-this.ub
    key_matrix_ub = TRUE
  }

  point_lb <- c(rep(-10^20,max_size),0)
  names(point_lb) <-rownames(point_lb,do.NULL = FALSE, prefix = "x")
  names(point_lb)[length(point_lb)]<-"y0"
  problem.list$point_lb <- point_lb

  #Solver
  key_initpoint <- FALSE
  if (!is.null(model$x0))
  {stopifnot(is.vector(model$x0))
    if (length(model$x0)==1){
      point_x0<-rep(model$x0,max_size)}
    else point_x0<-model$x0

    names(point_x0)<-rownames(point_x0,do.NULL = FALSE, prefix = "x")
    names(point_x0)[length(point_x0)]<-"y0"
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
        names(point_types)[length(point_types)]<-"y0"
        problem.list$point_types<-point_types
      }
      else stop("Wrong types. Stages: vector|0|1|2")
    }
  }

  #######################################################
  # Create problem_statement
  #######################################################

  problem_statement<-"Problem: problem_ratiorisk, type = minimize"

  # Objective
  if (key_linearization_1 && key_mip_1){
    stop("Only one option with value 0 or 1 may be used for objective: linearization or mip")
  }
  else if (key_linearization_1 && !key_mip_1){
    problem_statement<-sprintf("%s\nObjective: objective_H, linearize = %s",problem_statement,linearization_1)
  }
  else if (!key_linearization_1 && key_mip_1){
    problem_statement<-sprintf("%s\nObjective: objective_H, mip = %s",problem_statement,mip_1)
  }
  else if (!key_linearization_1 && !key_mip_1){
    problem_statement<-sprintf("%s\nObjective: objective_H",problem_statement)
  }

  if (!is.null(model$risk1)){
    if (key_param_1){
      if (resultf1$param == 1){
        problem_statement<-sprintf("%s\n  %s(0,matrix_scenarios_h1)",problem_statement,model$risk1)
        }
      else if (resultf1$param == 2){
        problem_statement<-sprintf("%s\n  %s(%s,matrix_scenarios_h1)",problem_statement,model$risk1,model$w1)
        }
      }
      else if (!key_param_1) problem_statement<-sprintf("%s\n  %s(matrix_scenarios_h1)",problem_statement,model$risk1)
    }

  # Add risk constraint
  if (key_linearization_2 && key_mip_2){
    stop("Only one option with value 0 or 1 may be used for constraint: linearization or mip")
  }
  else if (key_linearization_2 && !key_mip_2){
    problem_statement<-sprintf("%s\nConstraint: constraint_risk, >= 1, linearize = %s",problem_statement,linearization_2)
  }
  else if (!key_linearization_2 && key_mip_2){
    problem_statement<-sprintf("%s\nConstraint: constraint_risk, >= 1, mip = %s",problem_statement,mip_2)
  }
  else if (!key_linearization_2 && !key_mip_2){
    problem_statement<-sprintf("%s\nConstraint: constraint_risk, >= 1",problem_statement)
  }

  if (!is.null(model$risk2)){
    if (key_param_2){
      if (resultf2$param == 1){
        problem_statement<-sprintf("%s\n  -%s(0,matrix_scenarios_h2)",problem_statement,model$risk2)
        }
      else if (resultf2$param == 2){
        problem_statement<-sprintf("%s\n  -%s(%s,matrix_scenarios_h2)",problem_statement,model$risk2,model$w2)
        }
      }
    else if (!key_param_2) problem_statement<-sprintf("%s\n  -%s(matrix_scenarios_h2)",problem_statement,model$risk2)
  }

  if (key_lin) {
      problem_statement<-sprintf("%s\n  linear(matrix_d)",problem_statement)
  }

  # Add constraint inequality
  if (key_constr_ineq) {
    problem_statement<-sprintf("%s\nConstraint: <= 0",problem_statement)
    problem_statement<-sprintf("%s\n  linearmulti(matrix_Aineq)",problem_statement)
  }

  # Add constraint equality
  if (key_constr_eq) {
    problem_statement<-sprintf("%s\nConstraint: == 0",problem_statement)
    problem_statement<-sprintf("%s\n  linearmulti(matrix_Aeq)",problem_statement)
  }

  # Add constraint Lower bound
  if (key_matrix_lb) {
    problem_statement<-sprintf("%s\nConstraint: <= 0",problem_statement)
    problem_statement<-sprintf("%s\n  linearmulti(matrix_lb)",problem_statement)
  }

  # Add constraint Upper bound
  if (key_matrix_ub) {
    problem_statement<-sprintf("%s\nConstraint: >= 0",problem_statement)
    problem_statement<-sprintf("%s\n  linearmulti(matrix_ub)",problem_statement)
  }

  # Add Box of Variables
  key_box<-FALSE
  if (!is.null(point_lb)) {
    problem_statement<-sprintf("%s\nBox: >= point_lb",problem_statement)
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
  
  problem.res <- rpsg_solver(problem.list)
 
  resulted.list.new <- list()
  
  if (!is.null(problem.res) & is.list(problem.res)){
   
  resulted.list<-rpsg_getsolution(problem.res)

  if (resulted.list$objective == 0){stop("Riskratioprog can not find optimal solution,because the problem is unbounded.")}
  if (resulted.list$objective < 0){stop("Riskratioprog can not find optimal solution,because the denominator in the objective does not satisfy the non-negativity condition.")}


  optim.point.loc<-resulted.list$point_problem_ratiorisk
  y0<-optim.point.loc[["y0"]]
  pos.add.var<-grep("y0",names(optim.point.loc))
  optim.point<-optim.point.loc[-pos.add.var]/y0

  ##################################
  # Check optimality of the problem

  problem.list.check1 <- list()
  pos.add.var.h1<-grep("y0",colnames(matrix_scenarios_h1))
  matrix_scenarios_h1_add <- matrix_scenarios_h1[,-pos.add.var.h1]

  if (!is.null(model$c1) && is.vector(model$c1)) {
    matrix_scenarios_h1_add<-cbind(matrix_scenarios_h1_add,model$c1)
    colnames(matrix_scenarios_h1_add)[ncol(matrix_scenarios_h1_add)]<-"scenario_benchmark"
  }
  problem.list.check1$matrix_scenarios_h1 <- matrix_scenarios_h1_add

  if (key_param_1){
      function_description1<-sprintf("%s(%s,matrix_scenarios_h1)",model$risk1,model$w1)
    }
    else function_description1<-sprintf("%s(matrix_scenarios_h1)",model$risk1)

  problem.list.check1$function_description <- function_description1
  risk1value <- rpsg_getfunctionvalue(problem.list.check1, optim.point)

  denominator <- risk1value

  problem.list.check2 <- list()
  pos.add.var.h2<-grep("y0",colnames(matrix_scenarios_h2))
  matrix_scenarios_h2_add <- matrix_scenarios_h2[,-pos.add.var.h2]

  if (!is.null(model$c2) && is.vector(model$c2)) {
    matrix_scenarios_h2_add<-cbind(matrix_scenarios_h2_add,model$c2)
    colnames(matrix_scenarios_h2_add)[ncol(matrix_scenarios_h2_add)]<-"scenario_benchmark"
  }
  problem.list.check2$matrix_scenarios_h2 <- matrix_scenarios_h2_add

  if (key_param_2){
    function_description2<-sprintf("%s(%s,matrix_scenarios_h2)",model$risk2,model$w2)
  }
  else function_description2<-sprintf("%s(matrix_scenarios_h2)",model$risk2)

  problem.list.check2$function_description <- function_description2

  risk2value <- rpsg_getfunctionvalue(problem.list.check2, optim.point)

  if (key_lin) {
    problem.list.check3 <- list()
    matrix_d<-matrix(model$d,nrow=1,byrow = TRUE)
    colnames(matrix_d)<-colnames(matrix_d,do.NULL = FALSE, prefix = "x")
    problem.list.check3$matrix_d<-matrix_d
    function_description3<-"linear(matrix_d)"
    problem.list.check3$function_description <- function_description3
    d.value <- rpsg_getfunctionvalue(problem.list.check3, optim.point)
  }
  else d.value <- 0

  numerator <- d.value - risk2value
  if (denominator!=0){
    fval <- numerator/denominator
  }
  else fval <- 1e20

  # Create final report
  resulted.list.new$status <- resulted.list$status
  resulted.list.new$objective <- fval

  if(key_lin){
    function.value <- c(risk1value,risk2value,d.value)
    names(function.value)<-c(function_description1,function_description2,function_description3)
    }
  else {
    function.value <- c(risk1value,risk2value)
    names(function.value)<-c(function_description1,function_description2)
    }


  resulted.list.new$function.value <- function.value
  resulted.list.new$optimal_point <- optim.point

  if (key_constr_ineq) {
    vec <- model$Aineq %*% optim.point
    maxres <- max(vec - model$bineq)
    minres <- min(vec - model$bineq)
    resulted.list.new$ineq.constraint.value <- vec
    if (abs(maxres) > abs(minres)){
      resulted.list.new$ineq.constraint.residual <- maxres
    }
    else resulted.list.new$ineq.constraint.residual <- minres
  }

  if (key_constr_eq) {
    vec <- model$Aeq %*% optim.point
    maxres <- max(vec - model$beq)
    minres <- min(vec - model$beq)
    resulted.list.new$eq.constraint.value <- vec
    if (abs(maxres) > abs(minres)){
      resulted.list.new$eq.constraint.residual <- maxres
    }
    else resulted.list.new$eq.constraint.residual <- minres
  }

  resulted.list.new$loading.time <- resulted.list$loading.time
  resulted.list.new$preprocessing.time <- resulted.list$preprocessing.time
  resulted.list.new$solving.time <- resulted.list$solving.time
}
  return(resulted.list.new)
  
}

# Checks if PSG function may be used in rpsg_riskratioprog as risk1 or risk2.

check.functions.ratioprog <- function(function.text)
{
  list.of.functions<-c(	"cvar_dev",
                        "cvar_dev_g",
                        "cvar_risk",
                        "cvar_risk_g",
                        "var_dev",
                        "var_dev_g",
                        "var_risk",
                        "var_risk_g",
                        "cdar_dev",
                        "cdar_dev_g",
                        "drawdown_dev_avg",
                        "drawdown_dev_avg_g",
                        "drawdown_dev_max",
                        "drawdown_dev_max_g",
                        "pm_dev",
                        "pm_dev_g",
                        "pm_pen",
                        "pm_pen_g",
                        "max_dev",
                        "max_dev_g",
                        "max_risk",
                        "max_risk_g",
                        "meanabs_dev",
                        "meanabs_pen",
                        "meanabs_risk",
                        "meanabs_risk_g",
                        "st_dev",
                        "st_pen",
                        "st_risk",
                        "st_risk_g",
                        "avg",
                        "avg_g",
                        "linear",
                        "polynom_abs",
                        "pm2_pen",
                        "pm2_pen_g",
                        "pm2_dev",
                        "pm2_dev_g",
                        "cvar_comp_pos",
                        "cvar_comp_neg",
                        "var_comp_pos",
                        "var_comp_neg",
                        "max_comp_pos",
                        "max_comp_neg",
                        "quadratic",
                        "log_sum",
                        "logexp_sum",
                        "log_sum",
                        "logexp_sum"
                       )
  parameters.type <- c(    2,
                           2,
                           2,
                           2,
                           2,
                           2,
                           2,
                           2,
                           2,
                           2,
                           0,
                           0,
                           0,
                           0,
                           1,
                           1,
                           1,
                           1,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           1,
                           1,
                           1,
                           1,
                           2,
                           2,
                           2,
                           2,
                           0,
                           0,
                           0,
                           0,
                           0)

  loc <- match(function.text,list.of.functions)
  if (!is.na(loc)){
    loc1 <- TRUE
    loc2 <- parameters.type[loc]
  }
  else {
    loc1 <- FALSE
    loc2 <- -1
  }
  resultf <- list()
  resultf$adm <-loc1
  resultf$param <- loc2
  resultf$num <- loc
  return(resultf)
}



