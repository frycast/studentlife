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

  if ( get_schema(object) == "survey" ) {
    exc <- which(names(object) %in% c("uid","type"))
    s$survey_questions <- paste0(
      names(object[,-exc]), ": ", attr(object, "survey_questions"))
  }


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

  for (n in names(x)) {
    k <- x[[n]]
    if (is.list(k)) {
      cat(crayon::green(n), "\n")
      print(k)
      cat("\n\n")
    }
    else {
      if (is.character(k) && sum(nchar(k)) > 160)
        k <- paste0(k, sep = "\n")
      cat(crayon::green(n), "\n", k, "\n\n")
    }
  }
}

