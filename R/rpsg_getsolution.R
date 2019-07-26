rpsg_getsolution <- function(problem.res)
{
  solution.list <- list()
  
  if (!is.list(problem.res)){
    stop("Wrong PSG solution")
  }
  
  key_many_output = FALSE
  
  if (is.null(names(problem.res))){
    if (!is.null(names(problem.res[[1]]))){
    key_many_output = TRUE
    } else {
      stop("Wrong PSG solution")
    }
    
  }
  
  if (!key_many_output) {
    if (is.null(problem.res)){
      stop("PSG solution is NULL")
    }
    if (is.null(problem.res$output)){
      stop("PSG solution does not include output")
    } else{
    solution.list <- rpsg_getsolution_one_problem(problem.res)
    }
  }
    
  if (key_many_output) {
    for (i in 1:length(problem.res)){
      problem.res.loc <- problem.res[[i]]
    if (is.null(problem.res.loc)){
      stop("PSG solution is NULL")
    }
    if (is.null(problem.res.loc$output)){
      stop("PSG solution does not include output")
    } else{
    solution.list[[i]] <- rpsg_getsolution_one_problem(problem.res.loc)
    }
    }
  }
  return(solution.list)
  
}



rpsg_getsolution_one_problem <- function(problem.res)
{
  problem.report<-problem.res$output
  if (is.null(problem.report)) stop("PSG solution is NULL")

  if (!is.character(problem.report)) stop("PSG solution report must be character")

  solution.list<-list()

  elem.list<-unlist(strsplit(problem.report,"[ ]"))
  elem.list<-tolower(unlist(strsplit(elem.list,",")))

  #solution.status
  solution.status<-NULL
  loc1<-which(elem.list %in% "solution_status")
  if (length(loc1)==0) stop("Input is not a PSG solution report")

  for (i in 1:length(loc1)){
    j<-1
    while (elem.list[loc1[i]+j]!="=") {j<-j+1}
    solution.status<-c(solution.status,elem.list[loc1[i]+j+1])
  }

  solution.list$status<-solution.status

  #data_loading_time
  loading.time<-NULL
  loc1<-which(elem.list %in% "data_loading_time")
  if (length(loc1)==0) stop("Input is not a PSG solution report")

  for (i in 1:length(loc1)){
    j<-1
    while (elem.list[loc1[i]+j]!="=") {j<-j+1}
    loading.time<-c(loading.time,as.numeric(elem.list[loc1[i]+j+1]))
  }
  solution.list$loading.time<-loading.time

  #preprocessing_time
  preprocessing.time<-NULL
  loc1<-which(elem.list %in% "preprocessing_time")
  if (length(loc1)==0) stop("Input is not a PSG solution report")

  for (i in 1:length(loc1)){
    j<-1
    while (elem.list[loc1[i]+j]!="=") {j<-j+1}
    preprocessing.time<-c(preprocessing.time,as.numeric(elem.list[loc1[i]+j+1]))
  }
  solution.list$preprocessing.time<-preprocessing.time

  #solving_time
  solving.time<-NULL
  loc1<-which(elem.list %in% "solving_time")
  if (length(loc1)==0) stop("Input is not a PSG solution report")

  for (i in 1:length(loc1)){
    j<-1
    while (elem.list[loc1[i]+j]!="=") {j<-j+1}
    solving.time<-c(solving.time,as.numeric(elem.list[loc1[i]+j+1]))
  }
  solution.list$solving.time<-solving.time

  #objective
  solution.objective<-NULL
  loc1<-which(elem.list %in% "objective:")

  for (i in 1:length(loc1)){
    j<-1
    while (elem.list[loc1[i]+j]!="=") {j<-j+1}
    solution.objective<-c(solution.objective,as.numeric(elem.list[loc1[i]+j+1]))
  }
  solution.list$objective<-solution.objective

  #GAP
  if (solution.list$status!="calculated"){
    solution.gap<-NULL
    loc1<-which(elem.list %in% "objective:")
    for (i in 1:length(loc1)){
      j<-1
      while (substr(elem.list[loc1[i]+j],1,1)!="[") {j<-j+1}
      loc2<-elem.list[loc1[i]+j]
      solution.gap<-c(solution.gap,as.numeric(substr(loc2,2,nchar(loc2)-1)))
    }
    solution.list$gap<-solution.gap
  }

  #function value
  solution.function.value<-NULL
  solution.function.names<-NULL
  loc1<-which(elem.list %in% "function:")
  for (i in 1:length(loc1)){
    j<-1
    while (elem.list[loc1[i]+j]!="=") {j<-j+1}
    if (is.na(pmatch("vector",elem.list[loc1[i]+j+1])) && is.na(pmatch("point",elem.list[loc1[i]+j+1])) && is.na(pmatch("matrix",elem.list[loc1[i]+j+1]))){
      solution.function.value<-c(solution.function.value,as.numeric(elem.list[loc1[i]+j+1]))
      solution.function.names<-c(solution.function.names,paste(elem.list[(loc1[i]+1):(loc1[i]+j-1)],collapse = ", "))
    }
  }
  if (!is.null(solution.function.names)){
    names(solution.function.value)<-solution.function.names
    solution.list$function.value<-solution.function.value
  }


  #constraint value
  solution.constr.value<-NULL
  solution.constr.names<-NULL
  loc1<-NULL
  loc1<-which(elem.list %in% "constraint:")
  if (length(loc1)!=0) {
    for (i in 1:length(loc1)){
      j<-1
      while (elem.list[loc1[i]+j]!="=") {j<-j+1}
      if (is.na(pmatch("vector",elem.list[loc1[i]+j+1])) && is.na(pmatch("point",elem.list[loc1[i]+j+1])) && is.na(pmatch("matrix",elem.list[loc1[i]+j+1]))){
        solution.constr.value<-c(solution.constr.value,as.numeric(elem.list[loc1[i]+j+1]))
        solution.constr.names<-c(solution.constr.names,paste(elem.list[(loc1[i]+1):(loc1[i]+j-1)],collapse = ", "))
      }
    }
    if (!is.null(solution.constr.names)){
      names(solution.constr.value)<-solution.constr.names

      solution.list$constraint.value<-solution.constr.value
    }
  }

  list.members<-names(problem.res)
  for (i in 1:length(list.members)){
    if (!is.na(pmatch("vector",list.members[i])) || !is.na(pmatch("point",list.members[i])) || !is.na(pmatch("matrix",list.members[i]))){
      eval(parse(text = sprintf("solution.list$%s <- problem.res$%s",list.members[i],list.members[i])))

    }

  }
  return(solution.list)

}


