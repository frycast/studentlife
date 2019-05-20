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




















function (x, i, j, value)
{
  if (!all(names(sys.call()) %in% c("", "value")))
    warning("named arguments are discouraged")
  nA <- nargs()
  if (nA == 4L) {
    has.i <- !missing(i)
    has.j <- !missing(j)
  }
  else if (nA == 3L) {
    if (is.atomic(value) && !is.null(names(value)))
      names(value) <- NULL
    if (missing(i) && missing(j)) {
      i <- j <- NULL
      has.i <- has.j <- FALSE
      if (is.null(value))
        return(x[logical()])
    }
    else {
      if (is.numeric(i) && is.matrix(i) && ncol(i) == 2) {
        index <- rep.int(FALSE, prod(dim(x)))
        dim(index) <- dim(x)
        tryCatch(index[i] <- TRUE, error = function(e) stop(conditionMessage(e),
                                                            call. = FALSE))
        o <- order(i[, 2], i[, 1])
        N <- length(value)
        if (length(o)%%N != 0L)
          warning("number of items to replace is not a multiple of replacement length")
        if (N < length(o))
          value <- rep(value, length.out = length(o))
        value <- value[o]
        i <- index
      }
      if (is.logical(i) && is.matrix(i) && all(dim(i) ==
                                               dim(x))) {
        nreplace <- sum(i, na.rm = TRUE)
        if (!nreplace)
          return(x)
        N <- length(value)
        if (N > 1L && N < nreplace && (nreplace%%N) ==
            0L)
          value <- rep(value, length.out = nreplace)
        if (N > 1L && (length(value) != nreplace))
          stop("'value' is the wrong length")
        n <- 0L
        nv <- nrow(x)
        for (v in seq_len(dim(i)[2L])) {
          thisvar <- i[, v, drop = TRUE]
          nv <- sum(thisvar, na.rm = TRUE)
          if (nv) {
            if (is.matrix(x[[v]]))
              x[[v]][thisvar, ] <- if (N > 1L)
                value[n + seq_len(nv)]
            else value
            else x[[v]][thisvar] <- if (N > 1L)
              value[n + seq_len(nv)]
            else value
          }
          n <- n + nv
        }
        return(x)
      }
      if (is.matrix(i))
        stop("unsupported matrix index in replacement")
      j <- i
      i <- NULL
      has.i <- FALSE
      has.j <- TRUE
    }
  }
  else {
    stop("need 0, 1, or 2 subscripts")
  }
  if (has.j && length(j) == 0L)
    return(x)
  cl <- oldClass(x)
  class(x) <- NULL
  new.cols <- NULL
  nvars <- length(x)
  nrows <- .row_names_info(x, 2L)
  if (has.i && length(i)) {
    rows <- NULL
    if (anyNA(i))
      stop("missing values are not allowed in subscripted assignments of data frames")
    if (char.i <- is.character(i)) {
      rows <- attr(x, "row.names")
      ii <- match(i, rows)
      nextra <- sum(new.rows <- is.na(ii))
      if (nextra > 0L) {
        ii[new.rows] <- seq.int(from = nrows + 1L, length.out = nextra)
        new.rows <- i[new.rows]
      }
      i <- ii
    }
    if (all(i >= 0L) && (nn <- max(i)) > nrows) {
      if (is.null(rows))
        rows <- attr(x, "row.names")
      if (!char.i) {
        nrr <- (nrows + 1L):nn
        if (inherits(value, "data.frame") && (dim(value)[1L]) >=
            length(nrr)) {
          new.rows <- attr(value, "row.names")[seq_along(nrr)]
          repl <- duplicated(new.rows) | match(new.rows,
                                               rows, 0L)
          if (any(repl))
            new.rows[repl] <- nrr[repl]
        }
        else new.rows <- nrr
      }
      x <- xpdrows.data.frame(x, rows, new.rows)
      rows <- attr(x, "row.names")
      nrows <- length(rows)
    }
    iseq <- seq_len(nrows)[i]
    if (anyNA(iseq))
      stop("non-existent rows not allowed")
  }
  else iseq <- NULL
  if (has.j) {
    if (anyNA(j))
      stop("missing values are not allowed in subscripted assignments of data frames")
    if (is.character(j)) {
      if ("" %in% j)
        stop("column name \"\" cannot match any column")
      jj <- match(j, names(x))
      nnew <- sum(is.na(jj))
      if (nnew > 0L) {
        n <- is.na(jj)
        jj[n] <- nvars + seq_len(nnew)
        new.cols <- j[n]
      }
      jseq <- jj
    }
    else if (is.logical(j) || min(j) < 0L)
      jseq <- seq_along(x)[j]
    else {
      jseq <- j
      if (max(jseq) > nvars) {
        new.cols <- paste0("V", seq.int(from = nvars +
                                          1L, to = max(jseq)))
        if (length(new.cols) != sum(jseq > nvars))
          stop("new columns would leave holes after existing columns")
        if (is.list(value) && !is.null(vnm <- names(value))) {
          p <- length(jseq)
          if (length(vnm) < p)
            vnm <- rep_len(vnm, p)
          new.cols <- vnm[jseq > nvars]
        }
      }
    }
  }
  else jseq <- seq_along(x)
  if (anyDuplicated(jseq))
    stop("duplicate subscripts for columns")
  n <- length(iseq)
  if (n == 0L)
    n <- nrows
  p <- length(jseq)
  if (is.null(value)) {
    value <- list(NULL)
  }
  m <- length(value)
  if (!is.list(value)) {
    if (p == 1L) {
      N <- NROW(value)
      if (N > n)
        stop(sprintf(ngettext(N, "replacement has %d row, data has %d",
                              "replacement has %d rows, data has %d"), N,
                     n), domain = NA)
      if (N < n && N > 0L)
        if (n%%N == 0L && length(dim(value)) <= 1L)
          value <- rep(value, length.out = n)
        else stop(sprintf(ngettext(N, "replacement has %d row, data has %d",
                                   "replacement has %d rows, data has %d"), N,
                          nrows), domain = NA)
        if (!is.null(names(value)))
          names(value) <- NULL
        value <- list(value)
    }
    else {
      if (m < n * p && (m == 0L || (n * p)%%m))
        stop(sprintf(ngettext(m, "replacement has %d item, need %d",
                              "replacement has %d items, need %d"), m, n *
                       p), domain = NA)
      value <- matrix(value, n, p)
      value <- split(c(value), col(value))
    }
    dimv <- c(n, p)
  }
  else {
    value <- unclass(value)
    lens <- vapply(value, NROW, 1L)
    for (k in seq_along(lens)) {
      N <- lens[k]
      if (n != N && length(dim(value[[k]])) == 2L)
        stop(sprintf(ngettext(N, "replacement element %d is a matrix/data frame of %d row, need %d",
                              "replacement element %d is a matrix/data frame of %d rows, need %d"),
                     k, N, n), domain = NA)
      if (N > 0L && N < n && n%%N)
        stop(sprintf(ngettext(N, "replacement element %d has %d row, need %d",
                              "replacement element %d has %d rows, need %d"),
                     k, N, n), domain = NA)
      if (N > 0L && N < n)
        value[[k]] <- rep(value[[k]], length.out = n)
      if (N > n) {
        warning(sprintf(ngettext(N, "replacement element %d has %d row to replace %d rows",
                                 "replacement element %d has %d rows to replace %d rows"),
                        k, N, n), domain = NA)
        value[[k]] <- value[[k]][seq_len(n)]
      }
    }
    dimv <- c(n, length(value))
  }
  nrowv <- dimv[1L]
  if (nrowv < n && nrowv > 0L) {
    if (n%%nrowv == 0L)
      value <- value[rep_len(seq_len(nrowv), n), , drop = FALSE]
    else stop(sprintf(ngettext(nrowv, "%d row in value to replace %d rows",
                               "%d rows in value to replace %d rows"), nrowv, n),
              domain = NA)
  }
  else if (nrowv > n)
    warning(sprintf(ngettext(nrowv, "replacement data has %d row to replace %d rows",
                             "replacement data has %d rows to replace %d rows"),
                    nrowv, n), domain = NA)
  ncolv <- dimv[2L]
  jvseq <- seq_len(p)
  if (ncolv < p)
    jvseq <- rep_len(seq_len(ncolv), p)
  else if (p != 0L && ncolv > p) {
    warning(sprintf(ngettext(ncolv, "provided %d variable to replace %d variables",
                             "provided %d variables to replace %d variables"),
                    ncolv, p), domain = NA)
    new.cols <- new.cols[seq_len(p)]
  }
  if (length(new.cols)) {
    nm <- names(x)
    rows <- .row_names_info(x, 0L)
    a <- attributes(x)
    a["names"] <- NULL
    x <- c(x, vector("list", length(new.cols)))
    attributes(x) <- a
    names(x) <- c(nm, new.cols)
    attr(x, "row.names") <- rows
  }
  if (has.i)
    for (jjj in seq_len(p)) {
      jj <- jseq[jjj]
      vjj <- value[[jvseq[[jjj]]]]
      if (jj <= nvars) {
        if (length(dim(x[[jj]])) != 2L)
          x[[jj]][iseq] <- vjj
        else x[[jj]][iseq, ] <- vjj
      }
      else {
        x[[jj]] <- vjj[FALSE]
        if (length(dim(vjj)) == 2L) {
          length(x[[jj]]) <- nrows * ncol(vjj)
          dim(x[[jj]]) <- c(nrows, ncol(vjj))
          x[[jj]][iseq, ] <- vjj
        }
        else {
          length(x[[jj]]) <- nrows
          x[[jj]][iseq] <- vjj
        }
      }
    }
  else if (p > 0L)
    for (jjj in p:1L) {
      o <- order(jseq)
      jseq <- jseq[o]
      jvseq <- jvseq[o]
      jj <- jseq[jjj]
      v <- value[[jvseq[[jjj]]]]
      if (!is.null(v) && nrows > 0L && !length(v))
        length(v) <- nrows
      x[[jj]] <- v
      if (!is.null(v) && is.atomic(x[[jj]]) && !is.null(names(x[[jj]])))
        names(x[[jj]]) <- NULL
    }
  if (length(new.cols) > 0L) {
    new.cols <- names(x)
    if (anyDuplicated(new.cols))
      names(x) <- make.unique(new.cols)
  }
  class(x) <- cl
  x
}
