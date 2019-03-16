rpsg_restore_ps <- function(input)
{

  problem_statement_psg <- input$problem_statement
  all.memebers <- names(input)

for (iu in (1:length(all.memebers))){
  #print(iu)
  param.name <- all.memebers[[iu]]
  if (length(grep("parameter_",param.name))>0 ||
      length(grep("coefficient_",param.name))>0 ||
      length(grep("bound_",param.name))>0 ||
      length(grep("solver_",param.name))>0 ||
      length(grep("precision_",param.name))>0 ||
      length(grep("stages_",param.name))>0 ||
      length(grep("timelimit_",param.name))>0 ||
      length(grep("linearize_",param.name))>0 ||
      length(grep("mip_",param.name))>0)
     {
   pos_param <- grep(param.name,problem_statement_psg)
   if (length(pos_param)> 1) {
     pos_param <- pos_param[[1]]
   }
   if (length(pos_param)==1){
     param.value<-input[[param.name]]
     input[[param.name]]<-NULL
   problem_statement_psg[[pos_param]] <- sub(param.name,as.character(param.value),problem_statement_psg[[pos_param]])
   }
  }
}
output <- input
output$problem_statement <- problem_statement_psg

return(output)
}
