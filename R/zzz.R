.onLoad <- function(libname, pkgname) {

  op <- list(SL_start = as.Date("2013-03-24"))
  toset <- !(names(op) %in% names(options()))
  if(any(toset)) options(op[toset])
}
