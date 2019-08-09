#' regularise_time
#'
#' Transform an \code{SL_tibble} (as produced by
#' \code{\link[studentlife]{load_SL_tibble}})
#' in such a way that the observations are aggregated in
#' equal length intervals called 'blocks' (for more
#' information on blocks see
#' \code{\link[studentlife]{add_block_labels}}).
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
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab <- load_SL_tibble(
#'   loc = d, schema = "sensing", table = "activity", csv_nrows = 10)
#'
#' r_tab <- regularise_time(
#'   tab, blocks = c("day","weekday"),
#'   act_inf = max(activity_inference), add_NAs = FALSE)
#'
#' r_tab
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
#'membership is decided when \code{tab} is of class
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
#'the upper boundary of each epoch.
#'
#' @examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab <- load_SL_tibble(
#'   loc = d, schema = "sensing", table = "activity", csv_nrows = 10)
#'
#' b_tab <- add_block_labels(tab)
#' b_tab
#'
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

