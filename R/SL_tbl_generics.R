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





#'@keywords internal
#'
#'@export
`$<-.timestamp_SL_tbl` <- function (x, name, value) {

  if (name == "timestamp") {

    warning(paste0(name, " was tampered with so the ",
                   "timestamp_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("timestamp_SL_tbl", class(x))]
  }

  NextMethod("$<-")
}

#'@keywords internal
#'
#'@export
`$<-.interval_SL_tbl` <- function (x, name, value) {

  if (name == "start_timestamp" || name == "end_timestamp") {

    warning(paste0(name, " was tampered with so the ",
                   "interval_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("interval_SL_tbl", class(x))]
  }

  NextMethod("$<-")
}

#'@keywords internal
#'
#'@export
`$<-.dateonly_SL_tbl` <- function (x, name, value) {

  if (name == "date") {

    warning(paste0(name, " was tampered with so the ",
                   "dateonly_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("dateonly_SL_tbl", class(x))]
  }

  NextMethod("$<-")
}

#'@keywords internal
#'
#'@export
`[[<-.timestamp_SL_tbl` <- function (x, i, j, value) {

  if (i == "timestamp"
      || !is.na(names(x)[i]) && names(x)[i] == "timestamp") {
    warning(paste0("timestamp was tampered with so the ",
                   "timestamp_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("timestamp_SL_tbl", class(x))]
  }

  NextMethod("[[<-")
}

#'@keywords internal
#'
#'@export
`[[<-.interval_SL_tbl` <- function (x, i, j, value) {

  if (i == "start_timestamp"
      || !is.na(names(x)[i]) && names(x)[i] == "start_timestamp") {
    warning(paste0("start_timestamp was tampered with so the ",
                   "interval_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("interval_SL_tbl", class(x))]
  }
  else if (i == "end_timestamp"
      || !is.na(names(x)[i]) && names(x)[i] == "end_timestamp") {
    warning(paste0("end_timestamp was tampered with so the ",
                   "interval_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("interval_SL_tbl", class(x))]
  }

  NextMethod("[[<-")
}

#'@keywords internal
#'
#'@export
`[[<-.dateonly_SL_tbl` <- function (x, i, j, value) {

  if (i == "date"
      || !is.na(names(x)[i]) && names(x)[i] == "date") {
    warning(paste0("date was tampered with so the ",
                   "dateonly_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("dateonly_SL_tbl", class(x))]
  }

  NextMethod("[[<-")
}

#'@keywords internal
#'
#'@export
`[<-.timestamp_SL_tbl` <- function(x, i, j, value) {

  if (is.matrix(i))
    v <- i[,2]
  else if (missing(j)) v <- i
  else v <- j

  for (k in v) {
    if (k == "timestamp"
        || !is.na(names(x)[k]) && names(x)[k] == "timestamp") {
      warning(paste0("timestamp was tampered with so the ",
                     "timestamp_SL_tbl class was dropped"))
      class(x) <- class(x)[-pmatch("timestamp_SL_tbl", class(x))]
      break
    }
  }

  NextMethod("[<-")
}

#'@keywords internal
#'
#'@export
`[<-.interval_SL_tbl` <- function(x, i, j, value) {

  if (is.matrix(i))
    v <- i[,2]
  else if (missing(j)) v <- i
  else v <- j

  for (k in v) {
    if (k == "start_timestamp" || k == "end_timestamp"
        || !is.na(names(x)[k]) &&
        (names(x)[k] == "start_timestamp" || names(x)[k] == "end_timestamp")) {
      warning(paste0("start_timestamp or end_timestamp was ",
                     "tampered with so the ",
                     "interval_SL_tbl class was dropped"))
      class(x) <- class(x)[-pmatch("interval_SL_tbl", class(x))]
      break
    }
  }

  NextMethod("[<-")
}

#'@keywords internal
#'
#'@export
`[<-.dateonly_SL_tbl` <- function(x, i, j, value) {

  if (is.matrix(i))
    v <- i[,2]
  else if (missing(j)) v <- i
  else v <- j

  for (k in v) {
    if (k == "date"
        || !is.na(names(x)[k]) && names(x)[k] == "date") {
      warning(paste0("date was tampered with so the ",
                     "dateonly_SL_tbl class was dropped"))
      class(x) <- class(x)[-pmatch("dateonly_SL_tbl", class(x))]
      break
    }
  }

  NextMethod("[<-")
}

#'@keywords internal
#'
#'@export
`names<-.timestamp_SL_tbl` <- function (x, value) {

  if ( !("timestamp" %in% value) ) {
    warning(paste0("timestamp was tampered with so the ",
                   "timestamp_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("timestamp_SL_tbl", class(x))]
  }

  NextMethod("names<-")
}

#'@keywords internal
#'
#'@export
`names<-.interval_SL_tbl` <- function (x, value) {

  if ( !("start_timestamp" %in% value && "end_timestamp" %in% value) ) {
    warning(paste0("start_timestamp or end_timestamp was ",
                   "tampered with so the ",
                   "interval_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("interval_SL_tbl", class(x))]
  }

  NextMethod("names<-")
}

#'@keywords internal
#'
#'@export
`names<-.dateonly_SL_tbl` <- function (x, value) {

  if ( !("date" %in% value) ) {
    warning(paste0("date was tampered with so the ",
                   "dateonly_SL_tbl class was dropped"))
    class(x) <- class(x)[-pmatch("dateonly_SL_tbl", class(x))]
  }

  NextMethod("names<-")
}
