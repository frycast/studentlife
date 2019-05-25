#' regularise_time
#'
#' Transform an \code{SL_tibble} (as produced by
#' \code{\link[studentlife]{load_SL_tibble}})
#' in such a way that the observations are aggregated in
#' equal length intervals called 'blocks'.
#'
#' @param tab An \code{SL_tibble} as returned
#' by the function \code{\link[studentlife]{load_SL_tibble}}.
#' The \code{SL_tibble} must have some date-time information.
#' @param ... Arguments passed to \code{\link[dplyr]{summarise}},
#' used to aggregate values when multiple
#' observations are encountered in a block. Any columns
#' not specified here or under \code{blocks} will be dropped.
#' @param blocks A character vector naming one or more of the
#' block options "hour", "epoch", "day", "week", "weekday", "month" or "date".
#' If not present as column names in
#' \code{tab}, an attempt will be made to infer the blocks from existing
#' time information with \code{\link[studentlife]{add_block_labels}}.
#' The returned \code{data.frame} will
#' have one observation (possibly \code{NA}) for each block.
#' @param add_NAs A logical. If TRUE then NAs will be introduced
#' to fill missing blocks.
#' @param study_duration Integer. The duration of the StudentLife
#' study in days. This parameter does nothing if \code{limit_date_range}
#' it \code{TRUE}.
#' @param start_date Date. The date that the StudentLife study started.
#' @param epoch_levels A character vector of epoch labels.
#' @param epoch_ubs An integer vector that defines the hour that is
#' the upper boundary of each epoch.
#' @param uid_range An integer vector. The range of uids in
#' the StudentLife study.
#' @param date_range A vector of dates to be
#' used if \code{limit_date_range} is \code{FALSE}.
#'
#' @examples
#' \donttest{
#' d <- "D:/Datasets/studentlife"
#' download_studentlife(dest = d)
#'
#' tab <- load_SL_tibble(
#'   loc = d, schema = "sensing", table = "activity", csv_nrows = 10)
#'
#' regularise_time(
#'   tab, blocks = c("day","weekday"),
#'   act_inf = max(activity_inference), add_NAs = FALSE)
#' }
#'
#' @export
regularise_time <- function(
  tab, ..., blocks = c("epoch", "day"),
  add_NAs = TRUE,
  study_duration = getOption("SL_duration"),
  start_date = getOption("SL_start"),
  epoch_levels = getOption("SL_epoch_levels"),
  epoch_ubs = getOption("SL_epoch_ubs"),
  uid_range = getOption("SL_uids"),
  date_range = seq(from = start_date, by = 1, length.out = study_duration)) {

  blocks <- tolower(blocks)
  if ( "day" %in% blocks ) blocks <- c("date", blocks)
  opt <- c("month", "week", "day", "date", "weekday", "epoch", "hour")
  options_check(par = blocks, opt = opt)
  blocks <- sort(factor(blocks, levels = opt))

  eh <- c("epoch", "hour")
  ft <- c("date", eh[which(eh %in% blocks)])

  if ( "interval_SL_tbl" %in% class(tab) ) {

    if (!confirm_interval_SL_tibble(tab))
      stop("corrupt interval_SL_tbl")

    tab <- add_block_labels(
      tab, type = ft, start_date = start_date,
      epoch_levels = epoch_levels, epoch_ubs = epoch_ubs)

  } else if ( "timestamp_SL_tbl" %in% class(tab) ) {

    if (!confirm_timestamp_SL_tibble(tab))
      stop("corrupt timestamp_SL_tbl")

    tab <- add_block_labels(
      tab, type = ft, start_date = start_date,
      epoch_levels = epoch_levels, epoch_ubs = epoch_ubs)

  } else if ( "dateonly_SL_tbl" %in% class(tab) ) {

    if (!confirm_dateonly_SL_tibble(tab))
      stop("corrupt dateonly_SL_tbl")

    v <- (blocks == "epoch" || blocks == "hour")
    if (any(v)) {
      blocks <- blocks[which(!v)]
      warning("Not enough time information to derive epoch or hour")
    }

    tab <- add_block_labels(
      tab, type = "date", start_date = start_date,
      epoch_levels = epoch_levels, epoch_ubs = epoch_ubs)

  } else {

    stop(paste0("tab is not an interval_SL_tbl, ",
                "timestamp_SL_tbl or dateonly_SL_tbl."))
  }

  if (add_NAs) {
    if ("hour" %in% names(tab)) {
      full <- data.frame(
        uid = factor(
          rep(uid_range, each = length(date_range)*length(epoch_levels)*24),
          levels = uid_range),
        date = rep(date_range, each = length(epoch_levels)*24),
        epoch = factor(epoch_levels, levels = epoch_levels))
      tabg <- dplyr::left_join(full, tab, by = c("uid", "hour", "epoch", "date"))
    } else if ("epoch" %in% names(tab)){
      full <- data.frame(
        uid = factor(
          rep(uid_range, each = length(date_range)*length(epoch_levels)),
          levels = uid_range),
        date = rep(date_range, each = length(epoch_levels)),
        epoch = factor(epoch_levels, levels = epoch_levels))
      tabg <- dplyr::left_join(full, tab, by = c("uid", "epoch", "date"))
    } else if ("date" %in% names(tab)) {
      full <- data.frame(
        uid = factor(
          rep(uid_range, each = length(date_range)),
          levels = uid_range),
        date = rep(date_range, each = length(epoch_levels)))
      tabg <- dplyr::left_join(full, tab, by = c("uid", "date"))
    }
  } else {

    tabg <- tab
  }

  if ( all(c("date","uid") %in% names(tabg)) ) {
    class(tabg) <- c("dateonly_SL_tbl", "SL_tbl", class(tabg))
    transfer_SL_tbl_attrs(tabg) <- tab
  }

  tabg <- add_block_labels(
    tabg, type = blocks[which(!(blocks %in% ft))],
    start_date = start_date, epoch_levels = epoch_levels,
    epoch_ubs = epoch_ubs)

  `%>%` <- dplyr::`%>%`
  tabg <- tabg %>% dplyr::group_by_at(c("uid", as.character(blocks))) %>%
    dplyr::summarise(...) %>%
    dplyr::ungroup()

  if ( all(c("date","uid") %in% names(tabg)) ) {
    class(tabg) <- c("reg_SL_tbl", "dateonly_SL_tbl", "SL_tbl", class(tabg))
  }

  transfer_SL_tbl_attrs(tabg) <- tab
  attr(tabg, "blocks") <- as.character(blocks)

  return(tabg)
}



#'add_block_labels
#'
#'Classify observations from an \code{SL_tibble}
#'into block labels using available
#'date-time information. See more information
#'about "blocks" under the details section.
#'
#'Block label types can be one or more of "epoch"
#'(giving labels morning, evening, afternoon and night),
#'"day" (giving number of days since the \code{start_date} of the
#'StudentLife study),
#'"week" (giving integer number of weeks since the first week of the
#'StudentLife study, rounded downs),
#'"weekday" (giving the day of the week),
#'"month" (giving integer number of months since the start of the
#'StudentLife study, rounded down) and "date".
#'
#'@param tab An \code{SL_tibble} as returned
#' by the function \code{\link[studentlife]{load_SL_tibble}}.
#'@param type A character vector of block label types
#'to include. Can be one or more of "epoch", "day",
#'"week", "weekday", "month" and "date". Any block label types that
#'are not inferrable from the available date-time data are ignored.
#'@param interval A character string that decides how block
#'membership is decided when \code{stude} is of class
#'\code{interval_SL_tibble}. Can be either "start"
#'(use \code{start_timestamp}),
#'"end" (use \code{end_timestamp}) or "middle" (use the midpoint between
#'\code{start_timestamp} and \code{end_timestamp}).
#'@param warning Logical. If \code{TRUE} then a warning is produced
#'whenever a block label type is not inferrable from the
#'available date-time data.
#'@param start_date Date. The date that the StudentLife study started.
#'@param epoch_levels A character vector of epoch levels.
#'@param epoch_ubs An integer vector that defines the hour that is
#'the upper boundary of each epoch
#'
#'@export
add_block_labels <- function(
  tab, type = c("hour", "epoch", "day", "week", "weekday", "month", "date"),
  interval = "start", warning = TRUE, start_date = getOption("SL_start"),
  epoch_levels = getOption("SL_epoch_levels"),
  epoch_ubs = getOption("SL_epoch_ubs")) {

  interval <- tolower(interval)
  type <- tolower(type)
  opt <- c("month", "week", "day", "date", "weekday", "epoch", "hour")
  options_check(par = type, opt = opt)
  opt <- c("start", "end", "middle")
  options_check(par = interval, opt = opt)

  day_0 <- julian(start_date, origin = as.Date("2013-01-01"))[1] + 1
  week_0 <- floor(day_0/7)

  timestamp <- NULL
  date <- NULL

  if ( "interval_SL_tbl" %in% class(tab) ) {

    if (!confirm_interval_SL_tibble(tab))
      stop("corrupt interval_SL_tbl")

    if ( interval == "start" )
      timestamp <- tab$start_timestamp else
        if ( interval == "end" )
          timestamp <- tab$end_timestamp else
            if ( interval == "middle" )
              timestamp <- (tab$start_timestamp + tab$end_timestamp)/2

  } else if ( "timestamp_SL_tbl" %in% class(tab) ) {

    if (!confirm_timestamp_SL_tibble(tab))
      stop("corrupt timestamp_SL_tbl")

    timestamp <- tab$timestamp

  } else if ( "dateonly_SL_tbl" %in% class(tab) ) {

    if (!confirm_dateonly_SL_tibble(tab))
      stop("corrupt dateonly_SL_tbl")

    date <- tab$date

  } else {

    warning(paste0("tab is not an interval_SL_tbl, ",
                  "timestamp_SL_tbl or dateonly_SL_tbl. ",
                  "No block labels added"))
    return(tab)
  }

  if ( !is.null(timestamp) ) {
    timestamp <- as.POSIXct(timestamp, origin = "1970-01-01")
    date <- as.Date(timestamp)
  }

  hours <- NULL
  if ( "hour" %in% type ) {
    if ( !is.null(timestamp) ) {
      hours <- as.integer(strftime(timestamp, format="%H"))
      tab$hour <- hours
    } else {
      if (warning)
        warning("not enough date-time information to derive hour")
    }
  }

  if ( "epoch" %in% type ) {

    if( !is.null(timestamp) ) {

      if (is.null(hours)) {
        hours <- as.integer(strftime(timestamp, format="%H"))}
      epc <- purrr::map_chr(hours, function(x){
        epoch_levels[which(x <= epoch_ubs)[1]]
      })

      tab$epoch <- factor(epc, levels = epoch_levels)

    } else {

      if (warning)
        warning("not enough date-time information to derive epoch")
    }
  }

  if ( "day" %in% type ) {

    if ( !is.null(date) ) {

      tab$day <- as.integer(format(date, "%j")) - day_0

    } else {

      if (warning)
        warning("not enough date-time information to derive day")
    }
  }

  if ( "week" %in% type ) {

    if ( !is.null(date) ) {

      tab$week <- as.numeric(format(date, "%W")) - week_0

    } else {

      if (warning)
        warning("not enough date-time information to derive week")
    }
  }

  if ( "weekday" %in% type ) {

    if ( !is.null(date) ) {

      tab$weekday <- factor(
        tolower(weekdays(date, abbreviate = TRUE)),
        levels = c("mon","tue","wed","thu","fri","sat","sun"))

    } else {

      if (warning)
        warning("not enough date-time information to derive weekday")
    }
  }

  if ( "month" %in% type ) {

    if ( !is.null(date) ) {

      tab$month <- factor(
        months(date, abbreviate = TRUE),
        levels = c("Jan","Feb","Mar","Apr","May","Jun",
                   "Jul","Aug","Sep","Oct","Nov","Dec"))
    } else {

      if (warning)
        warning("not enough date-time information to derive month")
    }
  }

  if ( "date" %in% type ) {

    if ( !is.null(date) ) {

      oc <- class(tab)
      suppressWarnings(tab$date <- date)
      class(tab) <- oc

    } else {

      if (warning)
        warning("not enough date-time information to derive date")
    }
  }

  return(tab)
}



# Helper functions --------------------------------------------------------

#add_NAs <- function(tab, finest_block = "epoch") {
#
#  `%>%` <- dplyr::`%>%`
#
#  uids <- unique(tab$uid)
#  e <- unique(tab$epoch)
#  el <- c("nig", "mor", "aft", "eve")
#  d <- tab[[day]]
#  ndays <- max(d) - min(d)
#
#  tab <- data.frame(uid = rep(uids, each = length(e)*ndays),
#                     epoch = e,
#                     day = rep(0:ndays, each = length(e))) %>%
#    tibble::as_tibble() %>%
#    dplyr::mutate(epoch = factor(epoch, levels = el)) %>%
#    dplyr::left_join(tab, by = c("uid", "epoch", "day"))
#
#  return(tab)
#}





#### INCOMPLETE
### ISSUES:
## Needs documentation and greater generality.
##
####
# #' add_NAs
# #'
# #'
# #' @export
#add_NAs <- function(tab, day = "day") {
#
#  `%>%` <- dplyr::`%>%`
#
#  uids <- unique(tab$uid)
#  e <- unique(tab$epoch)
#  el <- c("night", "morning", "afternoon", "evening")
#  d <- tab[[day]]
#  ndays <- max(d) - min(d)
#
#  tab <- data.frame(uid = rep(uids, each = length(e)*ndays),
#                     epoch = e,
#                     day = rep(0:ndays, each = length(e))) %>%
#    tibble::as_tibble() %>%
#    dplyr::mutate(epoch = factor(epoch, levels = el)) %>%
#    dplyr::left_join(tab, by = c("uid", "epoch", "day"))
#
#  return(tab)
#}
#
#
### #' timestamp_convert
### #'
### #' Convert StudentLife UNIX timestamps to date, day, week and month
### #' in study.
### #'
### #' @param tab A data.frame where each row corresponds to an
### #' observation on a student, and at least one column contains
### #' UNIX timestamps.
### #' @param timestamp Character string indicating which variable
### #' contains unix time stamps to convert to columns date,
### #' day, week and month.
### #' @param month1,week1,day1 Integers specifying the
### #' month, week and day of the year that the study begins.
### #' Best left as defaults.
### #' @param days_in_weeks Logical, only used if \code{timestamp}
### #' is not default. If \code{TRUE} then days are per week and
### #' factors rather than per year and integer.
### #' @include_epochs Logical. If TRUE then each day is split into 4
### #' epochs: night (12am-6am), morning (6am-12pm), afternoon (12pm-6pm)
### #' and evening (6pm-12am).
### #'
### #' @examples
### #' p <- "D:/Datasets/studentlife"
### #' pam <- studentlife::load_SL_tibble(schema = 2, table = 22, location = p)
### #'
### #' timestamp_convert(pam)
### #' # Compare to
### #' timestamp_convert(pam, days_in_weeks = TRUE)
### #'
### #' @export
### timestamp_convert <- function(tab, day1 = 83,
###                               timestamp = "resp_time",
###                               include_epochs = FALSE) {
###
###   `%>%` <- dplyr::`%>%`
###
###   tab <- make_daily(timestamp, tab, day1, include_epochs)
###
###   return(tab)
### }
#
#
#
#### INCOMPLETE
### ISSUES
## Needing documentation
##
####
# #' add_days
# #'
# #'@export
#add_days <- function(tab, year_day_1 = 83, timestamp = "resp_time", fill_NAs = FALSE) {
#
#  if ( !(timestamp %in% names(tab)) )
#    stop("Column not found, try changing the timestamp parameter")
#
#  `%>%` <- dplyr::`%>%`
#
#  date <- as.Date(as.POSIXct(tab[[timestamp]], origin="1970-01-01"))
#  tab <- dplyr::mutate(tab, day = as.integer(format(date, "%j")) - year_day_1)
#
#  return(tab)
#}
#
#
#### INCOMPLETE
### ISSUES
## Needing documentation
##
####
# #' add_epochs
# #'
# #'@export
#add_epochs <- function(tab, timestamp = "resp_time",
#                       add_days = TRUE, year_day_1 = 83) {
#
#  if ( !(timestamp %in% names(tab)) )
#    stop("Column not found, try changing the timestamp parameter")
#
#  `%>%` <- dplyr::`%>%`
#
#  posix <- as.POSIXct(tab[[timestamp]], origin="1970-01-01")
#
#  epochs <- c("night","morning","afternoon","evening")
#  ub <- c(6, 12, 18, 24)
#  hours <- as.integer(strftime(posix, format="%H"))
#  epc <- purrr::map_chr(hours, function(x){
#    epochs[which(x <= ub)[1]]
#  })
#
#  tab <- tab %>% dplyr::mutate(
#           epoch = factor(epc, levels = epochs))
#
#  if (add_days) {
#
#    tab <- tab %>% dplyr::mutate(
#      day = as.numeric(format(as.Date(posix), "%j")) - year_day_1 )
#
#  }
#
#  return(tab)
#}
#
#### INCOMPLETE
### ISSUES
## Needing documentation
##
####
##' add_times
##'
# #'@export
#add_times <- function(tab, timestamp = "resp_time") {
#
#  if ( !(timestamp %in% names(tab)) )
#    stop("Column not found, try changing the timestamp parameter")
#
#  `%>%` <- dplyr::`%>%`
#
#  posix <- as.POSIXct(tab[[timestamp]], origin="1970-01-01")
#
#  return(tab %>% dplyr::mutate(
#         time = strftime(posix, format="%H:%M:%S")))
#}
#
#### INCOMPLETE
### ISSUES
## Needing documentation
##
####
##' add_dates
##'
# #' @export
#add_dates <- function(tab, timestamp = "resp_time") {
#
#  if ( !(timestamp %in% names(tab)) )
#    stop("Column not found, try changing the timestamp parameter")
#
#  `%>%` <- dplyr::`%>%`
#
#  posix <- as.POSIXct(tab[[timestamp]], origin="1970-01-01")
#
#  return(tab %>% dplyr::mutate(
#         date = as.Date(posix)))
#}
#
#
#
#### INCOMPLETE
### ISSUES
## Needs documentation
##
##
####
##'add_weeks
##'
##' Extract a week variable from the timestamp column
##' specified by the
# #' \code{timestamp} parameter
##'
# #'@export
#add_weeks <- function(tab, timestamp = "resp_time", week1 = 11) {
#
#  if ( !(timestamp %in% names(tab)) )
#    stop("Column not found, try changing the timestamp parameter")
#
#  `%>%` <- dplyr::`%>%`
#
#  d <- as.Date(as.POSIXct(tab[[timestamp]], origin="1970-01-01"))
#
#  tab <- tab %>%
#    dplyr::mutate(
#      week = as.numeric(format(d, "%W")) - week1)
#
#  return( tab )
#}
#
#
##'add_weekdays
##'
##' Given a data.frame with a column representing days
##' since the start of the study, creates a column
##' called "day" that is a factor representing the
##' day of the week.
##'
# #' @param tab A data.frame with a column representing number
# #' of days since the start of the study.
# #' @param day The name of the column that represents the
# #' number of days since the start of the study.
# #'
# #' @examples
# #' p <- "D:/Datasets/studentlife"
# #' pam <- studentlife::load_SL_tibble(schema = 2, table = 22, location = p)
# #' add_weekdays(timestamp_convert(pam))
# #'
# #'
# #'
# #'@export
#add_weekdays <- function(tab, day = "day") {
#
#  if ( !(day %in% names(tab)) )
#    stop("Column not found, try changing the day parameter")
#
#  `%>%` <- dplyr::`%>%`
#
#  d <- tab[[day]]
#  weekdays <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
#
#  return( tab %>% dplyr::mutate(
#    weekday = factor(weekdays[((d - 1) %% 7 + 1)], levels = weekdays)))
#}
#
#
##' PAM_categorise
##'
##' Categorise Photographic Affect Meter (PAM) scores into
##' 4 categories by either PAM Quadrant, Valence or Arousal
##' (or multiple of these).
##'
##' The 4 Quadrant categories are as follows:
##' Quadrant 1: low valence, low arousal.
##' Quadrant 2: low valence, high arousal.
##' Quadrant 3: high valence, low arousal.
##' Quadrant 4: high valence, high arousal.
##'
##' The 4 valence / arousal categories are: low (1),
##' med-low (2), high-low (3), high (4).
##'
# #'@param tab A data.frame with a column representing
# #'Photographic Affect Meter (PAM) score.
# #'@param pam_name Character. The name of the column
# #'representing PAM.
# #'@param types Character vector containing the categories,
# #'one or more of "quadrant", "valence" and "arousal" into
# #'which to code PAM scores.
# #'
# #' @export
#PAM_categorise <- function(tab, pam_name = "picture_idx",
#                           types = c("quadrant", "valence", "arousal") ) {
#  ub <- c(4, 8, 12, 16)
#  pams <- tab[[pam_name]]
#  ## Quadrant
#  if ( "quadrant" %in% types ) {
#    qc <- purrr::map_int(pams, function(x) { which(x <= ub)[1] })
#    tab$pam_q <- qc
#  }
#  ## Valence
#  v1 <- c(1, 2, 5, 6, 3, 4, 7, 8, 9, 10, 13, 14, 11, 12, 15, 16)
#  if ( "valence" %in% types ) {
#    vc <- purrr::map_int(v1[pams], function(x) { which(x <= ub)[1] })
#    tab$pam_v <- vc
#  }
#  ## Arousal
#  a1 <- c(1, 3, 9, 11, 2, 4, 10, 12, 5, 7, 13, 15, 6, 8, 14, 16)
#  if ( "arousal" %in% types ) {
#    ac <- purrr::map_int(a1[pams], function(x) { which(x <= ub)[1] })
#    tab$pam_a <- ac
#  }
#  return(tab)
#}
#
#
#
#
#
## Helper functions ----------------------------------------------
#
### make_daily <- function(timestamp, tab, day1, include_epochs) {
###
###   `%>%` <- dplyr::`%>%`
###
###   if ( !(timestamp %in% names(tab)) )
###     stop("Column not found, try changing the timestamp parameter")
###
###   posix <- as.POSIXct(tab[[timestamp]], origin="1970-01-01")
###
###   if ( include_epochs ) {
###
###     epochs <- c("night","morning","afternoon","evening")
###     ub <- c(6, 12, 18, 24)
###     hours <- as.integer(strftime(posix, format="%H"))
###     epc <- purrr::map_chr(hours, function(x){
###       epochs[which(x <= ub)[1]]
###     })
###
###     tab <- tab %>%
###        dplyr::mutate(
###          time = strftime(posix, format="%H:%M:%S"),
###          epoch = factor(epc, levels = epochs),
###          date = as.Date(posix),
###          day = as.numeric(format(date, "%j")) - day1)
###
###    } else {
###
###      tab <- tab %>%
###        dplyr::mutate(
###          date = as.Date(posix),
###          day = as.numeric(format(date, "%j")) - day1)
###    }
###
###   return(tab)
### }
###




