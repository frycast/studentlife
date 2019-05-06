








### INCOMPLETE
#
# A summary should apply to each tibble/dataset type.
#
# Every summary should give the dropped/missing students, an analysis of missing
# values, the names of the columns, the timestamp details (epochs etc) and
# averages of the important data e.g. by epoch, some missing value analysis by
# epoch, etc.
#
# The summary will demonstrate the philosophy of the package so it is important.
# We are particularly interested in missing values, response rates by student,
# and intervals between response times.
# The intervals between timestamps are not constant.
#
# When data is combined within epochs, the user should be able to specify an arbitrary
# function for doing the combination (could be max, min, median, var etc). The value in
# building this into the package is that we can create an attribute with the function
# definition so that we can keep track of the analysis. This could already be covered by
# another package though (I can't remember the name of the package that tracks the
# steps of the analysis in ML).
#
#
# The EMA summary should also include question information.
#
###

summary.EMA_tbl <- function(object, ...) {

  s <- list()
  s$EMA_name <- attr(object, "EMA_name")
  s$EMA_questions <- attr(object, "EMA_questions")
  s$dropped_students <- attr(object, "dropped_students")
  s$names <- names(object)

  class(s) <- "summary.EMA_tbl"
  return(s)
}

print.summary.EMA_tbl <- function(x, ...) {


}


## Examples writing generics

# # ## HERE ARE SOME GENERICS THAT PROTECT INTEGRITY OF CLASSES
#
# `$<-.timestamp_SL_tbl` <- function (x, name, value)
# {
#   if (name == "timestamp") {
#
#     warning(paste0(name, " was tampered with so the ",
#                    "timestamp_SL_tbl class was dropped"))
#     class(x) <- class(x)[which(class(x) != "timestamp_SL_tbl")]
#   }
#
#   NextMethod("$<-")
# }
#
# `$<-.interval_SL_tbl` <- function (x, name, value)
# {
#   if (name == "start_timestamp" || name == "end_timestamp") {
#
#     warning(paste0(name, " was tampered with so the ",
#                    "interval_SL_tbl class was dropped"))
#     class(x) <- class(x)[which(class(x) != "interval_SL_tbl")]
#   }
#
#   NextMethod("$<-")
# }
#
# `$<-.dateonly_SL_tbl` <- function (x, name, value)
# {
#   if (name == "date") {
#
#     warning(paste0(name, " was tampered with so the ",
#                    "dateonly_SL_tbl class was dropped"))
#     class(x) <- class(x)[which(class(x) != "dateonly_SL_tbl")]
#   }
#
#   NextMethod("$<-")
# }
#
#
#
# studs <- studs_t
# studs$thing <- 1
# class(studs)
# studs$timestamp <- 1
# class(studs)
#
# studs1 <- studs_p
# studs2 <- studs_p
# studs$thing <- 1
# class(studs1)
# studs1$start_timestamp <- 1
# class(studs1)
# class(studs2)
# studs2$end_timestamp <- 1
# class(studs2)
#
# studs <- studs_d
# studs$thing <- 1
# class(studs)
# studs$date <- 1
# class(studs)
#
# studs <- studs_t
# studs$thing <- 1
# class(studs)
# studs$timestamp <- 1
# class(studs)




# summary.SL_tbl <- function (x, ...)
# {
#   print("hi")
#
#   NextMethod("summary")
# }
#
# summary(studs)
#
# rm(`$<-.SL_tbl`)
#
#
# regularise_time <- function(x) {
#   UseMethod("regularise_time")
# }



#rbind.timestamp_SL_tbl <-
#cbind.timestamp_SL_tbl <-




