


### INCOMPLETE
## ISSUES:
# Needs documentation and greater generality.
#
###
#'group_by_epoch
#'
#' Group by day and epoch with mean response
#'
#' @param ... Arguments passed to summarise for group aggregation
#' @param timestamp Character string determining what column contains
#' timestamps for keeping if \code{keep_timestamps = TRUE}
#'
#'@export
group_by_epoch <- function(studs, ..., keep_timestamps = TRUE, timestamp = "resp_time") {

  `%>%` <- dplyr::`%>%`

  if ( keep_timestamps ) {
    studsg <- studs %>% dplyr::group_by(uid, day, epoch) %>%
      dplyr::summarise(
        ..., median_timestamp = as.integer(median(get(timestamp)))) %>%
      dplyr::ungroup()

  } else {
    studsg <- studs %>% dplyr::group_by(uid, day, epoch) %>%
      dplyr::summarise(...) %>%
      dplyr::ungroup()

  }

  return(studsg)
}



#' equalise_spacing
#'
#'
#'
#' @param studs
#' @param FUN A function used to aggregate values when multiple
#' observations are encountered in a time interval.
#' @param by A character vector naming one or more
#' of the columns of \code{studs}. The returned \code{data.frame}
#' will have one observation (possibly NA) for each unique
#' value of the columns specified.
#' @param y An optional character vector specifying one or
#' more of the columns in \code{studs} to be aggregated by \code{FUN}.
#' @param per_student A logical. If \code{per_student = FALSE} then
#' values are aggregated across students.
#'
#' @export
equalise_spacing <- function(studs, FUN = mean, by = c("epoch", "day"),
                             per_student = TRUE) {



}


### INCOMPLETE
## ISSUES:
# Needs documentation and greater generality.
#
###
#' add_NAs
#'
#'
#' @export
add_NAs <- function(studs, day = "day") {

  `%>%` <- dplyr::`%>%`

  uids <- unique(studs$uid)
  e <- unique(studs$epoch)
  el <- c("night", "morning", "afternoon", "evening")
  d <- studs[[day]]
  ndays <- max(d) - min(d)

  studs <- data.frame(uid = rep(uids, each = length(e)*ndays),
                     epoch = e,
                     day = rep(0:ndays, each = length(e))) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(epoch = factor(epoch, levels = el)) %>%
    dplyr::left_join(studs, by = c("uid", "epoch", "day"))

  return(studs)
}


## #' timestamp_convert
## #'
## #' Convert StudentLife UNIX timestamps to date, day, week and month
## #' in study.
## #'
## #' @param studs A data.frame where each row corresponds to an
## #' observation on a student, and at least one column contains
## #' UNIX timestamps.
## #' @param timestamp Character string indicating which variable
## #' contains unix time stamps to convert to columns date,
## #' day, week and month.
## #' @param month1,week1,day1 Integers specifying the
## #' month, week and day of the year that the study begins.
## #' Best left as defaults.
## #' @param days_in_weeks Logical, only used if \code{timestamp}
## #' is not default. If \code{TRUE} then days are per week and
## #' factors rather than per year and integer.
## #' @include_epochs Logical. If TRUE then each day is split into 4
## #' epochs: night (12am-6am), morning (6am-12pm), afternoon (12pm-6pm)
## #' and evening (6pm-12am).
## #'
## #' @examples
## #' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset"
## #' pam <- studentlife::load_SL_tibble(schema = 2, table = 22, location = p)
## #'
## #' timestamp_convert(pam)
## #' # Compare to
## #' timestamp_convert(pam, days_in_weeks = TRUE)
## #'
## #' @export
## timestamp_convert <- function(studs, day1 = 83,
##                               timestamp = "resp_time",
##                               include_epochs = FALSE) {
##
##   `%>%` <- dplyr::`%>%`
##
##   studs <- make_daily(timestamp, studs, day1, include_epochs)
##
##   return(studs)
## }



### INCOMPLETE
## ISSUES
# Needing documentation
#
###
#' add_days
#'
#'@export
add_days <- function(studs, year_day_1 = 83, timestamp = "resp_time", fill_NAs = FALSE) {

  if ( !(timestamp %in% names(studs)) )
    stop("Column not found, try changing the timestamp parameter")

  `%>%` <- dplyr::`%>%`

  date <- as.Date(as.POSIXct(studs[[timestamp]], origin="1970-01-01"))
  studs <- dplyr::mutate(studs, day = as.integer(format(date, "%j")) - year_day_1)

  return(studs)
}


### INCOMPLETE
## ISSUES
# Needing documentation
#
###
#' add_epochs
#'
#'@export
add_epochs <- function(studs, timestamp = "resp_time",
                       add_days = TRUE, year_day_1 = 83) {

  if ( !(timestamp %in% names(studs)) )
    stop("Column not found, try changing the timestamp parameter")

  `%>%` <- dplyr::`%>%`

  posix <- as.POSIXct(studs[[timestamp]], origin="1970-01-01")

  epochs <- c("night","morning","afternoon","evening")
  ub <- c(6, 12, 18, 24)
  hours <- as.integer(strftime(posix, format="%H"))
  epc <- purrr::map_chr(hours, function(x){
    epochs[which(x <= ub)[1]]
  })

  studs <- studs %>% dplyr::mutate(
           epoch = factor(epc, levels = epochs))

  if (add_days) {

    studs <- studs %>% dplyr::mutate(
      day = as.numeric(format(as.Date(posix), "%j")) - year_day_1 )

  }

  return(studs)
}

### INCOMPLETE
## ISSUES
# Needing documentation
#
###
#' add_times
#'
#'@export
add_times <- function(studs, timestamp = "resp_time") {

  if ( !(timestamp %in% names(studs)) )
    stop("Column not found, try changing the timestamp parameter")

  `%>%` <- dplyr::`%>%`

  posix <- as.POSIXct(studs[[timestamp]], origin="1970-01-01")

  return(studs %>% dplyr::mutate(
         time = strftime(posix, format="%H:%M:%S")))
}

### INCOMPLETE
## ISSUES
# Needing documentation
#
###
#' add_dates
#'
#' @export
add_dates <- function(studs, timestamp = "resp_time") {

  if ( !(timestamp %in% names(studs)) )
    stop("Column not found, try changing the timestamp parameter")

  `%>%` <- dplyr::`%>%`

  posix <- as.POSIXct(studs[[timestamp]], origin="1970-01-01")

  return(studs %>% dplyr::mutate(
         date = as.Date(posix)))
}



### INCOMPLETE
## ISSUES
# Needs documentation
#
#
###
#'add_weeks
#'
#' Extract a week variable from the timestamp column
#' specified by the
#' \code{timestamp} parameter
#'
#'@export
add_weeks <- function(studs, timestamp = "resp_time", week1 = 11) {

  if ( !(timestamp %in% names(studs)) )
    stop("Column not found, try changing the timestamp parameter")

  `%>%` <- dplyr::`%>%`

  d <- as.Date(as.POSIXct(studs[[timestamp]], origin="1970-01-01"))

  studs <- studs %>%
    dplyr::mutate(
      week = as.numeric(format(d, "%W")) - week1)

  return( studs )
}


#'add_weekdays
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
#' pam <- studentlife::load_SL_tibble(schema = 2, table = 22, location = p)
#' add_weekdays(timestamp_convert(pam))
#'
#'
#'
#'@export
add_weekdays <- function(studs, day = "day") {

  if ( !(day %in% names(studs)) )
    stop("Column not found, try changing the day parameter")

  `%>%` <- dplyr::`%>%`

  d <- studs[[day]]
  weekdays <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")

  return( studs %>% dplyr::mutate(
    weekday = factor(weekdays[((d - 1) %% 7 + 1)], levels = weekdays)))
}


#' PAM_categorise
#'
#' Categorise Photographic Affect Meter (PAM) scores into
#' 4 categories by either PAM Quadrant, Valence or Arousal
#' (or multiple of these).
#'
#' The 4 Quadrant categories are as follows:
#' Quadrant 1: low valence, low arousal.
#' Quadrant 2: low valence, high arousal.
#' Quadrant 3: high valence, low arousal.
#' Quadrant 4: high valence, high arousal.
#'
#' The 4 valence / arousal categories are: low (1),
#' med-low (2), high-low (3), high (4).
#'
#'@param studs A data.frame with a column representing
#'Photographic Affect Meter (PAM) score.
#'@param pam_name Character. The name of the column
#'representing PAM.
#'@param types Character vector containing the categories,
#'one or more of "quadrant", "valence" and "arousal" into
#'which to code PAM scores.
#'
#' @export
PAM_categorise <- function(studs, pam_name = "picture_idx",
                           types = c("quadrant", "valence", "arousal") ) {
  ub <- c(4, 8, 12, 16)
  pams <- studs[[pam_name]]
  ## Quadrant
  if ( "quadrant" %in% types ) {
    qc <- purrr::map_int(pams, function(x) { which(x <= ub)[1] })
    studs$pam_q <- qc
  }
  ## Valence
  v1 <- c(1, 2, 5, 6, 3, 4, 7, 8, 9, 10, 13, 14, 11, 12, 15, 16)
  if ( "valence" %in% types ) {
    vc <- purrr::map_int(v1[pams], function(x) { which(x <= ub)[1] })
    studs$pam_v <- vc
  }
  ## Arousal
  a1 <- c(1, 3, 9, 11, 2, 4, 10, 12, 5, 7, 13, 15, 6, 8, 14, 16)
  if ( "arousal" %in% types ) {
    ac <- purrr::map_int(a1[pams], function(x) { which(x <= ub)[1] })
    studs$pam_a <- ac
  }
  return(studs)
}





# Helper functions ----------------------------------------------

## make_daily <- function(timestamp, studs, day1, include_epochs) {
##
##   `%>%` <- dplyr::`%>%`
##
##   if ( !(timestamp %in% names(studs)) )
##     stop("Column not found, try changing the timestamp parameter")
##
##   posix <- as.POSIXct(studs[[timestamp]], origin="1970-01-01")
##
##   if ( include_epochs ) {
##
##     epochs <- c("night","morning","afternoon","evening")
##     ub <- c(6, 12, 18, 24)
##     hours <- as.integer(strftime(posix, format="%H"))
##     epc <- purrr::map_chr(hours, function(x){
##       epochs[which(x <= ub)[1]]
##     })
##
##     studs <- studs %>%
##        dplyr::mutate(
##          time = strftime(posix, format="%H:%M:%S"),
##          epoch = factor(epc, levels = epochs),
##          date = as.Date(posix),
##          day = as.numeric(format(date, "%j")) - day1)
##
##    } else {
##
##      studs <- studs %>%
##        dplyr::mutate(
##          date = as.Date(posix),
##          day = as.numeric(format(date, "%j")) - day1)
##    }
##
##   return(studs)
## }
##




