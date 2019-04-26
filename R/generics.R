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
