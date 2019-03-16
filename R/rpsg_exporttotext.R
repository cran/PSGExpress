rpsg_exporttotext <- function(path_tofiles, problem_list, rho=parent.frame(), allowExt=TRUE, rpsg_suppress.Messages = FALSE)
{
  if (!is.character(path_tofiles)) {
    stop("path_tofiles expected as string")
    return (FALSE)
  }
  if (!is.list(problem_list)) {
    if (is.character(problem_list)) {
      problem_list<-list(problem_statement=problem_list)
    }
  }
  if (is.na(match("problem_statement", names(problem_list)))) {
    stop("problem_statement not declared")
    return (NULL)
  }
  output.list<-.Call("crpsg_solver", as.integer(2), path_tofiles, problem_list, rho, allowExt, rpsg_suppress.Messages)
  return (output.list)
}
