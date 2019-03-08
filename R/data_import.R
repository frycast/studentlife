#' JSON2tibble
#'
#' This function is mostly used for reading StudentLife EMA data into R.
#' Note that \code{\link{na.exclude}} is used. Only the columns specified
#' by \code{var} will be selected.
#'
#' @param prefix The path prefix.
#' @param EMA_name The name of the folder in the StudentLife dataset,
#' as a subfolder of the folder specified by \code{prefix}.
#' Usually as a subfolder of the EMA/response directory.
#' @param vars Vector of names of variables to read for each student.
#' @param timestamp Character string indicating which variable
#' contains unix time stamps to convert to columns date,
#' day, week and month. Leave as default for no conversion.
#' @param month1,week1,day1 Integers specifying the
#' month, week and day of the year that the study begins.
#' Best left as defaults.
#' @param days_in_weeks Logical, only used if \code{timestamp}
#' is not default. If \code{TRUE} then days are per week and
#' factors rather than per year and integer.
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
#' pam1 <- JSON2tibble(p, n, v)
#' pam2 <- JSON2tibble(p, n, v, timestamp = "resp_time")
#' pam3 <- studentlife::JSON2tibble(p, n, v, "resp_time",
#'                                 days_in_weeks = TRUE)
#'
#' @export

JSON2tibble <- function(prefix, EMA_name, vars, timestamp = "",
                        month1 = 3, week1 = 11, day1 = 83,
                        days_in_weeks = FALSE) {

  `%>%` <- dplyr::`%>%`

  pr <- paste0(prefix, EMA_name, "/", EMA_name, "_u")
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

  studs <- do_transformations(studs, month1, week1, day1,
                              days_in_weeks, timestamp)

  return(studs)
}


#' csv2tibble
#'
#' This function is usually used for importing sensor data from csv
#' files in the StudentLife dataset. Note that \code{\link{na.exclude}}
#' is used.
#'
#' @param name The name of the folder in the StudentLife dataset,
#' a subfolder of the folder specified by \code{prefix}
#' @inheritParams JSON2tibble
#'
#' @examples
#' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset/sensing/"
#' n <- "conversation"
#' con1 <- csv2tibble(p,n)
#' con2 <- csv2tibble(p,n, timestamp = "start_timestamp")
#' con3 <- csv2tibble(p,n, timestamp = "start_timestamp",
#'                    days_in_weeks = TRUE)
#' @export

csv2tibble <- function(prefix, name, timestamp = "",
                       month1 = 3, week1 = 11, day1 = 83,
                       days_in_weeks = FALSE) {

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

  studs <- do_transformations(studs, month1, week1, day1,
                              days_in_weeks, timestamp)

  return(studs)
}



# Helper functions --------------------------------------------------------
weekdays <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")


do_transformations <- function(studs, month1, week1, day1,
                               days_in_weeks, timestamp) {

  `%>%` <- dplyr::`%>%`

  # Bind and exclude NAs
  studs <- studs %>%
    dplyr::bind_rows(.id = "student") %>%
    na.exclude() %>%
    tibble::as_tibble()

  if ( !(timestamp == "") ) {
    studs <- make_daily(timestamp, studs, month1, week1, day1)

    if ( days_in_weeks ) {
      studs <- make_days_in_weeks(studs)
    }
  }

  return(studs)
}


make_daily <- function(timestamp, studs, month1, week1, day1) {

  `%>%` <- dplyr::`%>%`

  studs <- studs %>%
    dplyr::mutate(
      date = as.Date(as.POSIXct(get(timestamp),
                                origin="1970-01-01")),
      month = as.numeric(format(date, "%m")) - month1,
      week = as.numeric(format(date, "%W")) - week1,
      day = as.numeric(format(date, "%j")) - day1)

#  # Get days within weeks
#  temp <- studs %>% dplyr::mutate(day = (day - 1) %% 7 )
#
#  start_week <- min(studs$week)
#  start_day <- min(temp[which(temp$week == start_week),]$day)
#
#  attr(studs, "start_week") <- start_week
#  attr(studs, "start_day") <- weekdays[start_day+1]

  return(studs)
}


make_days_in_weeks <- function(studs) {

  `%>%` <- dplyr::`%>%`

  return( studs %>% dplyr::mutate(
    day = factor(weekdays[((day - 1) %% 7 + 1)],
                 levels = weekdays)) )
}




