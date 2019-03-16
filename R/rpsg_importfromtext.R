rpsg_importfromtext <- function(path_tofiles, rpsg_suppress.Messages = FALSE)
{
  if (!is.character(path_tofiles)) {
    stop("path_tofiles expected as string")
    return (FALSE)
  }
  if (!file.exists(path_tofiles)) {
    stop(paste(path_tofiles, "not exist", sep=" "))
    return (FALSE)
  }
  loutput<-.Call("crpsg_importfromtext", path_tofiles, rpsg_suppress.Messages)
  return (loutput)
}
