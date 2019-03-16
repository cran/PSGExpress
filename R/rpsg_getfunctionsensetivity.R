rpsg_getfunctionsensetivity <- function(function_description, point_argument, rho=parent.frame(), allowExt=TRUE, rpsg_suppress.Messages = FALSE, allowFunVal = TRUE)
{
  if (!is.list(function_description)) {
    if (is.character(function_description)) {
      function_description<-list(function_description=function_description)
    }
  }
  if (is.na(match("function_description", names(function_description)))) {
    stop("function_description not declared")
    return (NULL)
  }
  if (missing(point_argument)) {
    stop("point_argument not defined")
    return (NULL)
  }
  function_description$point_argument<-point_argument
  output.list<-.Call("crpsg_getfunctionvalue", as.integer(2), function_description, rho, allowExt, rpsg_suppress.Messages, allowFunVal)
  return (output.list)
}
