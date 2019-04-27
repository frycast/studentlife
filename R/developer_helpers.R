`transfer_EMA_attrs<-` <- function(to, ..., value) {

  attr(to, "dropped_students") <- attr(value, "dropped_students")
  attr(to, "missing_students") <- attr(value, "missing_students")
  attr(to, "EMA_name") <- attr(value, "EMA_name")
  attr(to, "EMA_questions") <- attr(value, "EMA_questions")

  return(to)
}

get_name_from_path <- function(path, split = '/') {
  splat <- unlist(strsplit(path, split=split, fixed=TRUE))
  name <- splat[length(splat)]
  return(name)
}

generate_paths <- function(location, path, name, ext = ".csv") {
  pr <- paste0(location, "/", path, "/", name, "_u")
  paths <- c(paste0(pr, "0", seq(0,9), ext),
             paste0(pr, seq(10,59), ext))
}
