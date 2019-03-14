#' timestamp_convert
#'
#' Convert StudentLife UNIX timestamps to date, day, week and month
#' in study.
#'
#' @param studs A data.frame where each row corresponds to an
#' observation on a student, and at least one column contains
#' UNIX timestamps.
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
#' @examples
#' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset"
#' pam <- studentlife::read_from_SL(menu1 = 2, menu2 = 22, prefix = p)
#'
#' timestamp_convert(pam)
#' # Compare to
#' timestamp_convert(pam, days_in_weeks = TRUE)
#'
#' @export

timestamp_convert <- function(studs, month1 = 3, week1 = 11, day1 = 83,
                              days_in_weeks = FALSE, timestamp = "resp_time") {

  `%>%` <- dplyr::`%>%`

  # Bind and exclude NAs
  studs <- tibble::as_tibble(studs)

  if ( !(timestamp == "") ) {
    studs <- make_daily(timestamp, studs, month1, week1, day1)

    if ( days_in_weeks ) {
      studs <- make_days_in_weeks(studs)
    }
  }

  return(studs)
}

#'make_days_in_weeks
#'
#' Given a data.frame with a column representing days
#' since the start of the study, creates a column
#' called "day" that is a factor representing the
#' day of the week.
#'
#' @param studs A data.frame with a column representing number
#' of days since the start of the study.
#' @param day The name of the column that represents the
#' number of days since the start of the study.
#'
#' @examples
#' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset"
#' pam <- studentlife::read_from_SL(menu1 = 2, menu2 = 22, prefix = p)
#' make_days_in_weeks(timestamp_convert(pam))
#'
#'
#'
#'@export
make_days_in_weeks <- function(studs, day = "day") {

  `%>%` <- dplyr::`%>%`

  d <- studs[[day]]
  weekdays <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")

  return( studs %>% dplyr::mutate(
    day = factor(weekdays[((d - 1) %% 7 + 1)],
                 levels = weekdays)) )
}




# Helper functions ----------------------------------------------

make_daily <- function(timestamp, studs, month1, week1, day1) {

  `%>%` <- dplyr::`%>%`

  studs <- studs %>%
    dplyr::mutate(
      date = as.Date(as.POSIXct(get(timestamp),
                                origin="1970-01-01")),
      month = as.numeric(format(date, "%m")) - month1,
      week = as.numeric(format(date, "%W")) - week1,
      day = as.numeric(format(date, "%j")) - day1)

  return(studs)
}



