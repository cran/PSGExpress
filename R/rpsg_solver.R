rpsg_solver <- function(problem_list, rho=parent.frame(), allowExt=TRUE, rpsg_suppress.Messages = FALSE)
{
  if (!is.list(problem_list)) {
    if (is.character(problem_list)) {
      problem_list<-list(problem_statement=problem_list)
    }
  }
  if (is.na(match("problem_statement", names(problem_list)))) {
    stop("problem_statement not declared")
    return (NULL)
  }
  output.list<-.Call("crpsg_solver", as.integer(0), as.character(""), problem_list, rho, allowExt, rpsg_suppress.Messages)
  return (output.list)
}
