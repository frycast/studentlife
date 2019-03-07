#' JSON2tibble
#'
#' This function is mostly used for reading StudentLife EMA data into R.
#' Note that \code{\link{na.exclude}} is used. Only the columns specified
#' by \code{var} will be selected.
#'
#' @param prefix The path prefix
#' @param name The name of the folder in the StudentLife dataset,
#' usually as a subfolder of the EMA/response directory
#' @param vars Vector of names of variables to read for each student
#' @param timestamp Character string indicating which variable
#' contains unix time stamps to convert to columns date,
#' day, week and month. Leave unspecified for no conversion.
#' @param month1,week1,day1 Integers specifying the
#' month, week and day of the year that the study begins.
#' Best left as defaults.
#'
#'
#' @examples
#' # Import Stress EMAs
#' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset/EMA/response/"
#' n <-  "Stress"
#' v <- c("level", "resp_time")
#' stress <- JSON2tibble(p, n, v)
#'
#' # Import PAM EMAs
#' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset/EMA/response/"
#' n <-  "PAM"
#' v <- c("picture_idx", "resp_time")
#' pam <- JSON2tibble(p, n, v)
#' pam <- JSON2tibble(p, n, v, timestamp = "resp_time")
#'
#' @export

JSON2tibble <- function(prefix, name, vars, timestamp,
                        month1 = 3, week1 = 11, day1 = 83) {

  `%>%` <- dplyr::`%>%`

  pr <- paste0(prefix, name, "/", name, "_u")
  paths <- c(paste0(pr, "0", seq(0,9), ".json"),
             paste0(pr, seq(10,59), ".json"))
  studs <- list()
  missing_studs <- 0
  for (i in 1:60) {
    tryCatch(studs[[length(studs)+1]] <- jsonlite::fromJSON(paths[i]),
             error = function(e){missing_studs <<- missing_studs + 1},
             warning = function(w){})
  }

  # Drop lists not sharing variable names specified by parameter 'vars'
  null_ind <- c()
  for (i in 1:length(studs)) {
    if ( !all(vars %in% names(studs[[i]]) )) {
      null_ind <- c(null_ind, i)
    }
  }
  studs[null_ind] <- NULL

  # Drop all columns other than those specified by parameter 'vars'
  studs <- lapply(studs, function(x){
    dplyr::select(x, vars)
  })

  # Bind rows and exclude NA
  studs <- dplyr::bind_rows(studs, .id = "student") %>%
    na.exclude() %>%
    tibble::as_tibble()

  if ( !missing(timestamp) ) {

    studs <- studs %>%
      dplyr::mutate(
        date = as.Date(as.POSIXct(get(timestamp),
                                  origin="1970-01-01")),
        month = as.numeric(format(date, "%m")) - month1,
        week = as.numeric(format(date, "%W")) - week1,
        day = as.numeric(format(date, "%j")) - day1)
  }

  return(studs)
}


#' csv2tibble
#'
#' This function is usually used for importing sensor data from csv
#' files in the StudentLife dataset. Note that \code{\link{na.exclude}}
#' is used.
#'
#' @param prefix The path prefix
#' @param name The name of the folder in the StudentLife dataset,
#' a subfolder of the folder specified by \code{prefix}
#' @param timestamp Character string indicating which variable
#' contains unix time stamps to convert to columns date,
#' day, week and month. Leave unspecified for no conversion.
#'
#' @examples
#' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset/sensing/"
#' n <- "conversation"
#' csv2tibble(p,n)
#' csv2tibble(p,n, timestamp = "start_timestamp")
#'
#' @export

csv2tibble <- function(prefix, name, timestamp,
                       month1 = 3, week1 = 11, day1 = 83) {

  `%>%` <- dplyr::`%>%`

  # Import the data
  pr <- paste0(prefix, name, "/", name, "_u")
  paths <- c(paste0(pr, "0", seq(0,9), ".csv"),
             paste0(pr, seq(10,59), ".csv"))
  studs <- list()
  missing_studs <- 0
  for (i in 1:60) {
    tryCatch(studs[[length(studs)+1]] <- read.csv(paths[i]),
             error = function(e){missing_studs <<- missing_studs + 1},
             warning = function(w) {})
  }

  # Bind and exclude NAs
  studs <- studs %>%
    dplyr::bind_rows(.id = "student") %>%
    na.exclude() %>%
    tibble::as_tibble()

  if ( !missing(timestamp) ) {

    studs <- studs %>%
      dplyr::mutate(
        date = as.Date(as.POSIXct(get(timestamp),
                                  origin="1970-01-01")),
        month = as.numeric(format(date, "%m")) - month1,
        week = as.numeric(format(date, "%W")) - week1,
        day = as.numeric(format(date, "%j")) - day1)
  }

  return(studs)
}


