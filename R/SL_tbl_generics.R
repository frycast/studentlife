#'@keywords internal
#'
#'@export
summary.EMA_SL_tbl <- function(object, s, ...) {

  if (missing(s)) s <- list()
  s$EMA_questions <- attr(object, "EMA_questions")

  ds <- attr(object, "dropped_students")
  if (length(ds) == 0) ds <- "None"
  s$dropped_students <- ds

  class(s) <- c("summary.EMA_SL_tbl", class(s))
  NextMethod("summary", object, s, ...)
}

#'@keywords internal
#'
#'@export
summary.timestamp_SL_tbl <- function(object, s, ...) {

  if (missing(s)) s <- list()

  s$time_info <- unique(c("timestamp", s$time_info))

  class(s) <- c("summary.timestamp_SL_tbl", class(s))
  NextMethod("summary", object, s, ...)
}

#'@keywords internal
#'
#'@export
summary.interval_SL_tbl <- function(object, s, ...) {

  if (missing(s)) s <- list()

  s$time_info <- unique(c("interval", s$time_info))

  class(s) <- c("summary.interval_SL_tbl", class(s))
  NextMethod("summary", object, s, ...)
}

#'@keywords internal
#'
#'@export
summary.dateonly_SL_tbl <- function(object, s, ...) {

  if (missing(s)) s <- list()

  s$time_info <- unique(c("date-only", s$time_info))

  class(s) <- c("summary.dateonly_SL_tbl", class(s))
  NextMethod("summary", object, s, ...)
}

#'@keywords internal
#'
#'@export
summary.reg_SL_tbl <- function(object, s, ...) {

  if (missing(s)) s <- list()

  s$blocks <- attr(object, "blocks")

  class(s) <- c("summary.dateless_SL_tbl", class(s))
  NextMethod("summary", object, s, ...)
}

#'@keywords internal
#'
#'@export
summary.dateless_SL_tbl <- function(object, s, ...) {

  if (missing(s)) s <- list()

  s$time_info <- unique(c("none", s$time_info))

  class(s) <- c("summary.reg_SL_tbl", class(s))
  NextMethod("summary", object, s, ...)
}

#'@keywords internal
#'
#'@export
summary.SL_tbl <- function(object, s, ...) {

  if (missing(s)) s <- list()

  s$column_names <- names(object)

  s$schema <- attr(object, "schema")
  s$table <- attr(object, "table")

  s$skim <- skimr::skim(object)

  class(s) <- c("summary.SL_tbl", class(s))
  return(s)
}

#'@keywords internal
#'
#'@export
print.summary.SL_tbl <- function(x, ...) {

  if (get_schema(x) == "survey") {

  } else {
    for (n in names(x)) {
      k <- x[[n]]
      if (is.list(k))
        print(k)
      else {cat(n, "\n", k, "\n\n")}
    }
  }
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

  if (!missing(i) && is.matrix(i))
    v <- i[,2]
  else if (missing(j) && !missing(i)) v <- i
  else if (!missing(j)) v <- j

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

  if (!missing(i) && is.matrix(i))
    v <- i[,2]
  else if (missing(j) && !missing(i)) v <- i
  else if (!missing(j)) v <- j

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

  if (!missing(i) && is.matrix(i))
    v <- i[,2]
  else if (missing(j) && !missing(i)) v <- i
  else if (!missing(j)) v <- j

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


