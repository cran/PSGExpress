rpsg_create_subroutine <- function(problem.list, description) {

stopifnot(!is.null(problem.list))
stopifnot(!is.null(description$path_to_save))
stopifnot(!is.null(description$function_name))
stopifnot(!is.null(description$function_description))
stopifnot(length(description$function_description)==1)
stopifnot(!is.null(problem.list$problem_statement))
stopifnot(is.character(problem.list$problem_statement))

if (length(problem.list$problem_statement)==1) {
  loc1<-strsplit(problem.list$problem_statement,"\n")
  problem.list$problem_statement <- loc1[[1]]
}


path <- description$path_to_save
user_name <- description$function_name
header <- sprintf("#%s",description$function_description)

# Save data for user subroutine

header <- c(header,"  ")

# Create user subroutine
loc <- rpsg_convert_ps_params(problem.list$problem_statement)

input_user_subroutine <- problem.list
input_user_subroutine$problem_name <- NULL
input_user_subroutine$problem_statement <- NULL
input_user_subroutine  <- c(input_user_subroutine,loc$parameter.data.list)

problem.list <- input_user_subroutine

save(problem.list,file = sprintf("%s/%s_data.RData",path,user_name))

ps_text<- header
count <- length(header)+1
ps_text[[count]] <-sprintf("%s <-function(problem.list) {",user_name)

loc_ps <- loc$problem_statement_new
count <-count+1
ps_text[[count]] <- sprintf("problem.list$problem_statement <- c('%s',",loc_ps[[1]])

if (length(loc_ps)>2){
  for (i in 2:(length(loc_ps)-1)) {
    count <-count+1
    ps_text[[count]]<-sprintf("'%s',",loc_ps[[i]])
  }
ps_text[[length(ps_text)+1]] <- sprintf("'%s')",loc_ps[[length(loc_ps)]])
}
else if(length(loc_ps)==2){
  ps_text[[length(ps_text)+1]] <- sprintf("'%s')",loc_ps[[2]])
}

ps_text[[length(ps_text)+1]] <- " "
ps_text[[length(ps_text)+1]] <- "problem_list_new <- rpsg_restore_ps(problem.list)"
ps_text[[length(ps_text)+1]] <- " "
ps_text[[length(ps_text)+1]] <- "output.list <- rpsg_solver(problem_list_new)"
ps_text[[length(ps_text)+1]] <- "output<-rpsg_getsolution(output.list)"

ps_text[[length(ps_text)+1]] <- "}"

endtext<-vector(length = 3)
  endtext[[1]] <-"#========================================================"
  endtext[[2]] <-"#This function was created by rpsg_create_subroutine"
  endtext[[3]] <-"#========================================================"

ps_text <-c(ps_text,endtext)

fileConn<-file(sprintf("%s/%s.R",path,user_name))
writeLines(ps_text, fileConn)
close(fileConn)

# Create example for user subroutine

example_text <- " "

count1 <- 1

count1 <-count1+1
loc <- sprintf("%s/%s_data.RData",path,user_name)
example_text[[count1]] <- sprintf("load('%s')",loc)

count1 <-count1+1
loc <- sprintf("%s/%s.R",path,user_name)
example_text[[count1]] <- sprintf("source('%s')",loc)

count1 <-count1+1
example_text[[count1]] <- " "

count1 <-count1+1
example_text[[count1]] <- sprintf("output.list <- %s(problem.list)",user_name)


all.memebers<-names(problem.list)
param.descr <- NULL
problem_statement_new <- loc_ps
#user.comment <- "Solve cvar optimization problem with constraints"
#cat(param.descr,sep = "\n")
param.descr[[1]] <- "#Description:"
param.descr[[2]] <- sprintf("#%s",description$function_description)
param.descr[[3]] <- " "
param.descr[[4]] <- "#Problem Statement:"
param.descr[[5]] <- paste(sprintf("#%s",problem_statement_new),collapse="\n")
param.descr[[6]] <- " "
param.descr[[7]] <- "#Arguments:"
param.descr[[8]] <- "#model   list with data for optimization problem. All fields are mandatory:"

count <- length(param.descr)
for (i in 1:length(all.memebers)) {
  psg_line <-NULL
  text_loc <- NULL
  pos_param <- grep(all.memebers[[i]],problem_statement_new)
  if (length(pos_param)==1){
    psg_line <- problem_statement_new[[pos_param]]
  }
  else if(length(pos_param)>1){
    psg_line<-NULL
    for (ik in 1:length(pos_param)){
      psg_line <- c(psg_line,problem_statement_new[[pos_param[[ik]]]])}
  }
  #else stop("Wrong parameter")
  if (!is.null(psg_line)){
    if(length(grep("parameter",all.memebers[[i]]))>0){
      text_loc <- sprintf("parameter (numeric) of the PSG function:'%s';",psg_line)
    }
    if(length(grep("coefficient",all.memebers[[i]]))>0){
      text_loc <- sprintf("value (numeric) of the coefficient of the PSG function: '%s';",psg_line)
    }
    if(length(grep("bound",all.memebers[[i]]))>0){
      text_loc <- sprintf("value (numeric) of the bound in the constraint: '%s';",psg_line)
    }
    if(length(grep("solver",all.memebers[[i]]))>0){
      text_loc <- sprintf("character with name of optimization solver: VAN, CAR, BULDOZER, TANK;")
    }
    if(length(grep("precision",all.memebers[[i]]))>0){
      text_loc <- sprintf("number of digits (1,...,9) that solver tries to obtain in objective and constraints (default = 7);")
    }
    if(length(grep("stages",all.memebers[[i]]))>0){
      text_loc <- sprintf("number of stages (1,...,30) of the optimization process. This parameter should be specified for VaR, Probability, and Cardinality groups of functions;")
    }
    if(length(grep("timelimit",all.memebers[[i]]))>0){
      text_loc <- sprintf("time in seconds restricting the duration of the optimization process;")
    }
    if(length(grep("linearize",all.memebers[[i]]))>0){
      text_loc <- sprintf("number 0 or 1; controls internal representation of risk function, which can speed up the optimization process;used in '%s';",psg_line)
    }
    if(length(grep("mip",all.memebers[[i]]))>0){
      text_loc <- sprintf("number 0 or 1; specifies the linearization of functions and using MIP capabilities of Gurobi. active only in VANGRB, CARGRB or HELI solvers; used in '%s';",psg_line)
    }
    if(length(grep("matrix",all.memebers[[i]]))>0){
      text_loc <- sprintf("PSG Matrix with data; used in '%s';",psg_line[[1]])
    }
    if(length(grep("pmatrix",all.memebers[[i]]))>0){
      text_loc <- sprintf("PSG Sparse Matrix with data; used in '%s';",psg_line[[1]])
    }
    if(length(grep("point",all.memebers[[i]]))>0){
      text_loc <- sprintf("PSG Point with data; used in '%s';",psg_line[[1]])
    }
    if(length(grep("vector",all.memebers[[i]]))>0){
      text_loc <- sprintf("PSG Vector with data; used in '%s';",psg_line[[1]])
    }
    if (!is.null(text_loc)){
      if (length(psg_line)==1){
        count <- count+1
        param.descr[[count]] <- sprintf("#model$%s   %s",all.memebers[[i]],text_loc)
      }
      else if (length(psg_line)>1){
        count <- count+1
        loc1 <- substr(text_loc[[1]], 1, nchar(text_loc[[1]])-1)
        param.descr[[count]] <- sprintf("#model$%s   %s",all.memebers[[i]],loc1)
        for (lmn in 2:length(psg_line)){
          #count <- count+1
          param.descr[[count]] <- sprintf("%s,'%s'",param.descr[[count]],psg_line[[lmn]])
        }
        param.descr[[count]] <- sprintf("%s;",param.descr[[count]])
      }
    }
  }
}

full_example_text <- c(param.descr,example_text," ",endtext)

fileConn<-file(sprintf("%s/%s_example.R",path,user_name))
writeLines(full_example_text, fileConn)
close(fileConn)

loc_out_text<-sprintf("User subroutine was saved to %s/%s.R.\nExample R-file with subroutine call was saved to %s/%s_example.R.\nData for example call was saved to %s/%s_data.RData",path,user_name,path,user_name,path,user_name)
print(cat(loc_out_text))

}

rpsg_convert_ps_params <- function(problem_statement_init)
{

  problem_statement <- tolower(problem_statement_init)

  #############################
  # Parameters of functions
  #############################

  loc_str <- strsplit(problem_statement,"[(,)]")

  count<-0
  param.list<-list()
  for (i in 1:length(loc_str)){
    loc_val <- suppressWarnings(as.numeric(loc_str[[i]]))
    loc_pos <- which(!is.na(loc_val))
    if(length(loc_pos)==1){
      count<-count+1
      param.list$num.line[[count]]<-i
      param.list$param.text[[count]]<-loc_str[[i]][[loc_pos]]
      param.list$param.value[[count]]<-loc_val[[loc_pos]]
    }
    else if(length(loc_pos)>1){
      for (j in 1:length(loc_pos)) {
        count<-count+1
        param.list$num.line[[count]]<-i
        param.list$param.text[[count]]<-loc_str[[i]][[loc_pos[[j]]]]
        param.list$param.value[[count]]<-loc_val[[loc_pos[[j]]]]
      }
    }
  }

  #############################
  # Bounds of constraints
  #############################

  pos_loc1 <- grep("constraint:",problem_statement)
  pos_loc2 <- grep("box:",problem_statement)

  pos_loc <- c(pos_loc1,pos_loc2)

  #bound.list<-vector(mode="list")
  bound.list<-list()
  count<-0
  if (length(pos_loc)>1){
    for (k in 1:length(pos_loc)){
      loc_str <- strsplit(problem_statement[[pos_loc[[k]]]],"[>=<,]")
      loc_str <- loc_str[[1]]
      loc_str <- loc_str[loc_str!=""]

      #check for linearize,
      loc_find1 <- grep("linearize",loc_str)
      loc_find2 <- grep("quadratic",loc_str)
      loc_find3 <- grep("mip",loc_str)
      loc_find <-c(loc_find1,loc_find2,loc_find3)
      loc_find <-loc_find+1

      if (length(loc_find)>0){
        loc_str<-loc_str[-loc_find]
      }
      loc_val <- suppressWarnings(as.numeric(loc_str))
      loc_pos <- which(!is.na(loc_val))

    if(length(loc_pos)==1){
        count<-count+1
        bound.list$num.line[[count]]<-pos_loc[[k]]
        bound.list$param.text[[count]]<-loc_str[[loc_pos]]
        bound.list$param.value[[count]]<-loc_val[[loc_pos]]
      }
      else if(length(loc_pos)>1){
        for (j in 1:length(loc_pos)) {
          count<-count+1
          bound.list$num.line[[count]]<-pos_loc[[k]]
          bound.list$param.text[[count]]<-loc_str[[loc_pos[[j]]]]
          bound.list$param.value[[count]]<-loc_val[[loc_pos[[j]]]]
        }
      }
      #trimws
    }
  }

  #############################
  # Solver
  #############################

  solver.list<-list()
  pos_line <- grep("solver:",problem_statement)

  if (length(pos_line)>1){stop("To many lines with Solver: in the problem")}

  if (length(pos_line)==1){
    all_solvers <- c("van","car","buldozer","tank","vangrb","cargrb","heli")

    pos_solver <- NULL
    for (li in 1:length(all_solvers)){
      pos_solver_loc <- grep(all_solvers[[li]],strsplit(problem_statement[[pos_line]],"[:,]")[[1]])
      if (length(pos_solver_loc)>0) {pos_solver<-c(pos_solver,li)}
    }
    if (length(pos_solver)==2) {
      if (pos_solver[[1]]==1 && pos_solver[[2]]==5) {pos_solver<-5}
      if (pos_solver[[1]]==2 && pos_solver[[2]]==6) {pos_solver<-6}
    }
    pos_solver_test <- unlist(pos_solver)
    if (length(pos_solver_test)>1) {stop("To many solvers in the problem")}
    if (length(pos_solver_test)==1) {
      solver.list$num.line<-pos_line
      solver.list$param.text<-all_solvers[[pos_solver_test]]
      solver.list$param.value<-all_solvers[[pos_solver_test]]
    }


  }

  #############################
  # Options of Solver: precision, stages, timelimit
  #############################
  precision.list<-list()
  stages.list<-list()
  timelimit.list<-list()

  pos_line <- grep("solver:",problem_statement)

  if (length(pos_line)==1){
    loc_text <- strsplit(problem_statement[[pos_line]],"[:,=]")[[1]]
    pos_precision <- grep("precision",loc_text)
    pos_stages <- grep("stages",loc_text)
    pos_timelimit <- grep("timelimit",loc_text)
    # Precision
    if (length(pos_precision)==1 && length(loc_text)>pos_precision){
      loc_val <- suppressWarnings(as.numeric(loc_text[[pos_precision+1]]))
      if (!is.na(loc_val)){
        precision.list$num.line<-pos_line
        precision.list$param.text<-loc_text[[pos_precision+1]]
        precision.list$param.value<-loc_val
      }
    }
    # Stages
    if (length(pos_stages)==1 && length(loc_text)>pos_stages){
      loc_val <- suppressWarnings(as.numeric(loc_text[[pos_stages+1]]))
      if (!is.na(loc_val)){
        stages.list$num.line<-pos_line
        stages.list$param.text<-loc_text[[pos_stages+1]]
        stages.list$param.value<-loc_val
      }
    }

    if (length(pos_timelimit)==1 && length(loc_text)>pos_timelimit){
      loc_val <- suppressWarnings(as.numeric(loc_text[[pos_timelimit+1]]))
      if (!is.na(loc_val)){
        timelimit.list$num.line<-pos_line
        timelimit.list$param.text<-loc_text[[pos_timelimit+1]]
        timelimit.list$param.value<-loc_val
      }
    }
  }


  #############################
  # Options of Objective and Constraints: linearize, mip
  #############################
  linearize.list <- list()
  mip.list <- list()

  pos_line1 <- grep("linearize",problem_statement)
  pos_line2 <- grep("mip",problem_statement)

  # linearize
  count<-0
  if (length(pos_line1)>0){
    for (k in 1:length(pos_line1)){

      loc_str <- strsplit(problem_statement[[pos_line1[[k]]]],"[=,]")
      loc_str <- loc_str[[1]]
      pos_linearize <- grep("linearize",loc_str)
      if (length(loc_str)>pos_linearize){
        loc_val <- suppressWarnings(as.numeric(loc_str[[pos_linearize+1]]))
        if (!is.na(loc_val)){
          count<-count+1
          linearize.list$num.line[[count]]<-pos_line1[[k]]
          linearize.list$param.text[[count]]<-loc_str[[pos_linearize+1]]
          linearize.list$param.value[[count]]<-loc_val
        }
      }
    }
  }

  # mip
  count<-0
  if (length(pos_line2)>0){
    for (k in 1:length(pos_line2)){

      loc_str <- strsplit(problem_statement[[pos_line2[[k]]]],"[=,]")
      loc_str <- loc_str[[1]]
      pos_mip <- grep("mip",loc_str)
      #stopifnot(length(pos_mip)==1)
      if (length(loc_str)>pos_mip){
        loc_val <- suppressWarnings(as.numeric(loc_str[[pos_mip+1]]))
        if (!is.na(loc_val)){
          count<-count+1
          mip.list$num.line[[count]]<-pos_line2[[k]]
          mip.list$param.text[[count]]<-loc_str[[pos_mip+1]]
          mip.list$param.value[[count]]<-loc_val
        }
      }
    }
  }

  #############################
  # Coefficients of functions
  #############################
  coeff.list <- list()
  pos_line <- grep("*",problem_statement,fixed = TRUE)

  count<-0
  if (length(pos_line)>0){
    for (k in 1:length(pos_line)){
      loc_str <- strsplit(problem_statement[[pos_line[[k]]]],"[*,+,-, ]")
      loc_str <- loc_str[[1]]
      for (i in 1:length(loc_str)){        
        loc_val <- suppressWarnings(as.numeric(loc_str[[i]]))
        loc_pos <- which(!is.na(loc_val))
        if(length(loc_pos)==1){
          count<-count+1
          coeff.list$num.line[[count]]<-pos_line[[k]]
          coeff.list$param.text[[count]]<-loc_str[[i]][[loc_pos]]
          coeff.list$param.value[[count]]<-loc_val[[loc_pos]]
        }
      }

    }
  }
  #############################
  # Create new problem statement and list of parameters
  #############################

  problem_statement_new <- tolower(problem_statement_init)
  parameter.data.list <-list()
  #param.list
  #bound.list
  #solver.list
  #precision.list
  #stages.list
  #timelimit.list
  #linearize.list
  #mip.list
  #coeff.list

  #coeff.list
  if (length(coeff.list)>0){
    for (i in 1:length(coeff.list$num.line)){
      curr.line <- problem_statement_new[[coeff.list$num.line[[i]]]]
      new.line <- sub(coeff.list$param.text[[i]],sprintf(" coefficient_%g",i),curr.line,fixed = TRUE)
      problem_statement_new[[coeff.list$num.line[[i]]]] <- new.line

      text.loc <- sprintf("parameter.data.list$coefficient_%g <- %g",i,coeff.list$param.value[[i]])
      eval(parse(text=text.loc))
    }
  }
  #param.list
  if (length(param.list)>0){
    for (i in 1:length(param.list$num.line)){
      curr.line <- problem_statement_new[[param.list$num.line[[i]]]]
      new.line <- sub(param.list$param.text[[i]],sprintf(" parameter_%g",i),curr.line,fixed = TRUE)
      problem_statement_new[[param.list$num.line[[i]]]] <- new.line

      text.loc <- sprintf("parameter.data.list$parameter_%g <- %g",i,param.list$param.value[[i]])
      eval(parse(text=text.loc))
    }
  }
  #bound.list
  if (length(bound.list)>0){
    for (i in 1:length(bound.list$num.line)){
      curr.line <- problem_statement_new[[bound.list$num.line[[i]]]]
      new.line <- sub(bound.list$param.text[[i]],sprintf(" bound_%g",i),curr.line,fixed = TRUE)
      problem_statement_new[[bound.list$num.line[[i]]]] <- new.line

      text.loc <- sprintf("parameter.data.list$bound_%g <- %g",i,bound.list$param.value[[i]])
      eval(parse(text=text.loc))
    }
  }
  #solver.list
  if (length(solver.list)>0){
    for (i in 1:length(solver.list$num.line)){
      curr.line <- problem_statement_new[[solver.list$num.line[[i]]]]
      new.line <- sub(solver.list$param.text[[i]],sprintf(" solver_%g",i),curr.line,fixed = TRUE)
      problem_statement_new[[solver.list$num.line[[i]]]] <- new.line

      text.loc <- sprintf("parameter.data.list$solver_%g <- '%s'",i,solver.list$param.value[[i]])
      eval(parse(text=text.loc))
    }
  }
  #precision.list
  if (length(precision.list)>0){
    for (i in 1:length(precision.list$num.line)){
      curr.line <- problem_statement_new[[precision.list$num.line[[i]]]]

      pos_prec <- gregexpr(pattern ='precision',curr.line)
      part1 <- substr(curr.line,1,pos_prec[[1]][1]-1)
      part2 <- substr(curr.line,pos_prec[[1]][1],10000)

      part2_new <- sub(precision.list$param.text[[i]],sprintf(" precision_%g",i),part2,fixed = TRUE)
      new.line <- paste(part1,part2_new,sep =" ")

      #new.line <- sub(precision.list$param.text[[i]],sprintf(" precision_%g",i),curr.line)
      problem_statement_new[[precision.list$num.line[[i]]]] <- new.line

      text.loc <- sprintf("parameter.data.list$precision_%g <- %g",i,precision.list$param.value[[i]])
      eval(parse(text=text.loc))
    }
  }
  #stages.list
  if (length(stages.list)>0){
    for (i in 1:length(stages.list$num.line)){
      curr.line <- problem_statement_new[[stages.list$num.line[[i]]]]

      pos_stage <- gregexpr(pattern ='stages',curr.line)
      part1 <- substr(curr.line,1,pos_stage[[1]][1]-1)
      part2 <- substr(curr.line,pos_stage[[1]][1],10000)

      part2_new <- sub(stages.list$param.text[[i]],sprintf(" stages_%g",i),part2,fixed = TRUE)
      new.line <- paste(part1,part2_new,sep =" ")

      #new.line <- sub(stages.list$param.text[[i]],sprintf(" stages_%g",i),curr.line)
      problem_statement_new[[stages.list$num.line[[i]]]] <- new.line

      text.loc <- sprintf("parameter.data.list$stages_%g <- %g",i,stages.list$param.value[[i]])
      eval(parse(text=text.loc))
    }
  }
  #timelimit.list
  if (length(timelimit.list)>0){
    for (i in 1:length(timelimit.list$num.line)){
      curr.line <- problem_statement_new[[timelimit.list$num.line[[i]]]]

      pos_time <- gregexpr(pattern ='timelimit',curr.line)
      part1 <- substr(curr.line,1,pos_time[[1]][1]-1)
      part2 <- substr(curr.line,pos_time[[1]][1],10000)

      part2_new <- sub(timelimit.list$param.text[[i]],sprintf(" timelimit_%g",i),part2,fixed = TRUE)
      new.line <- paste(part1,part2_new,sep =" ")

      #new.line <- sub(timelimit.list$param.text[[i]],sprintf(" timelimit_%g",i),curr.line)
      problem_statement_new[[timelimit.list$num.line[[i]]]] <- new.line

      text.loc <- sprintf("parameter.data.list$timelimit_%g <- %g",i,timelimit.list$param.value[[i]])
      eval(parse(text=text.loc))
    }
  }
  #linearize.list
  if (length(linearize.list)>0){
    for (i in 1:length(linearize.list$num.line)){
      curr.line <- problem_statement_new[[linearize.list$num.line[[i]]]]

      pos_lin <- gregexpr(pattern ='linearize',curr.line)
      part1 <- substr(curr.line,1,pos_lin[[1]][1]-1)
      part2 <- substr(curr.line,pos_lin[[1]][1],10000)

      part2_new <- sub(linearize.list$param.text[[i]],sprintf(" linearize_%g",i),part2,fixed = TRUE)
      new.line <- paste(part1,part2_new,sep =" ")
      #new.line <- sub(linearize.list$param.text[[i]],sprintf(" linearize_%g",i),curr.line)
      problem_statement_new[[linearize.list$num.line[[i]]]] <- new.line

      text.loc <- sprintf("parameter.data.list$linearize_%g <- %g",i,linearize.list$param.value[[i]])
      eval(parse(text=text.loc))
    }
  }
  #mip.list
  if (length(mip.list)>0){
    for (i in 1:length(mip.list$num.line)){
      curr.line <- problem_statement_new[[mip.list$num.line[[i]]]]

      pos_mip <- gregexpr(pattern ='mip',curr.line)
      part1 <- substr(curr.line,1,pos_mip[[1]][1]-1)
      part2 <- substr(curr.line,pos_mip[[1]][1],10000)

      part2_new <- sub(mip.list$param.text[[i]],sprintf(" mip_%g",i),part2)
      new.line <- paste(part1,part2_new,sep =" ")

      #new.line <- sub(mip.list$param.text[[i]],sprintf(" mip_%g",i),curr.line)
      problem_statement_new[[mip.list$num.line[[i]]]] <- new.line

      text.loc <- sprintf("parameter.data.list$mip_%g <- %g",i,mip.list$param.value[[i]])
      eval(parse(text=text.loc))
    }
  }
  results <- list()
  results$problem_statement_new <- problem_statement_new
  results$parameter.data.list <- parameter.data.list

  return(results)

}





