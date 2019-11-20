.onLoad <- function(libname, pkgname) {

  op <- list(SL_start = as.Date("2013-03-24"),
             SL_duration = 93,
             SL_epoch_levels = c("nig","mor","aft","eve"),
             SL_epoch_ubs = c(6, 12, 18, 24),
             SL_uids = 0:59,
             SL_timezone = -5)
  toset <- !(names(op) %in% names(options()))
  if(any(toset)) options(op[toset])
}
