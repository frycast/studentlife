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
#' day, week and month.
#' @param month1,week1,day1 Integers specifying the
#' month, week and day of the year that the study begins.
#' Best left as defaults.
#' @param days_in_weeks Logical, only used if \code{timestamp}
#' is not default. If \code{TRUE} then days are per week and
#' factors rather than per year and integer.
#' @include_epochs Logical. If TRUE then each day is split into 4
#' epochs: night (12am-6am), morning (6am-12pm), afternoon (12pm-6pm)
#' and evening (6pm-12am).
#'
#' @examples
#' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset"
#' pam <- studentlife::read_from_SL(menu1 = 2, menu2 = 22, location = p)
#'
#' timestamp_convert(pam)
#' # Compare to
#' timestamp_convert(pam, days_in_weeks = TRUE)
#'
#' @export

timestamp_convert <- function(studs, month1 = 3, week1 = 11, day1 = 83,
                              days_in_weeks = FALSE, timestamp = "resp_time",
                              include_epochs = FALSE) {

  `%>%` <- dplyr::`%>%`

  # Bind and exclude NAs
  studs <- tibble::as_tibble(studs)

  studs <- make_daily(timestamp, studs, month1, week1, day1, include_epochs)

  if ( days_in_weeks ) {
    studs <- make_days_in_weeks(studs)
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
#' pam <- studentlife::read_from_SL(menu1 = 2, menu2 = 22, location = p)
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




###### INCOMPLETE
## ISSUES:
##   Need to return the tibble
##
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
  if ( "quadrant" %in% types )
    qs <- purrr::map_int(pams, function(x) { which(x <= ub)[1] })

  ## Valence
  v1 <- c(1, 2, 5, 6, 3, 4, 7, 8, 9, 10, 13, 14, 11, 12, 15, 16)
  if ( "valence" %in% types )
    vs <- purrr::map_int(v1[pams], function(x) { which(x <= ub)[1] })

  ## Arousal
  a1 <- c(1, 3, 9, 11, 2, 4, 10, 12, 5, 7, 13, 15, 6, 8, 14, 16)
  if ( "arousal" %in% types )
    as <- purrr::map_int(a1[pams], function(x) { which(x <= ub)[1] })

}





# Helper functions ----------------------------------------------

make_daily <- function(timestamp, studs, month1, week1, day1, include_epochs) {

  `%>%` <- dplyr::`%>%`

  posix <- as.POSIXct(studs[[timestamp]], origin="1970-01-01")

  if ( include_epochs ) {

    epochs <- c("night","morning","afternoon","evening")
    ub <- c(6, 12, 18, 24)
    hours <- as.integer(strftime(posix, format="%H"))
    epc <- purrr::map_chr(hours, function(x){
      epochs[which(x <= ub)[1]]
    })

    studs <- studs %>%
       dplyr::mutate(
         time = strftime(posix, format="%H:%M:%S"),
         epoch = factor(epc, levels = epochs),
         date = as.Date(posix),
         month = as.numeric(format(date, "%m")) - month1,
         week = as.numeric(format(date, "%W")) - week1,
         day = as.numeric(format(date, "%j")) - day1)

   } else {

     studs <- studs %>%
       dplyr::mutate(
         date = as.Date(posix),
         month = as.numeric(format(date, "%m")) - month1,
         week = as.numeric(format(date, "%W")) - week1,
         day = as.numeric(format(date, "%j")) - day1)
   }

  return(studs)
}


time <- as.POSIXct(1000012000, origin="1970-01-01")
strftime(time, format="%H:%M:%S")



