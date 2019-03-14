#' read_from_SL
#'
#' Import a chosen StudentLife data set as
#' a tibble. Leave \code{menu1} and \code{menu2}
#' unspecified to choose interactively via a
#' menu.
#'
#' @param menu1 The Menu 1 choice. Leave
#' blank to choose interactively.
#' @param menu2 The Menu 1 choice. Leave
#' blank to choose interactively.
#' @param prefix The path to the top
#' directory of the StudentLife dataset
#' @param vars Character vector of variable
#' names to import for all students. Leave
#' blank and this will be chosen interactively
#' if necesssary.
#'
#' @examples
#' pr <- "C:/Users/danie/Data/StudentLife/dataset/dataset"
#' students <- import_from_SL(prefix = prefix)
#'
#' @export


read_from_SL <- function(menu1, menu2, prefix = ".", vars) {


  path <- get_path(menu1, menu2)

  if ( path %in% EMA_json ) {

    studs <- get_EMA_studs(path, prefix, vars)

  } else if ( path %in% long_csv ) {

    studs <- get_long_csv_studs(path, prefix, vars)

  } else if ( path %in% wide_csv ) {

    studs <- get_wide_csv_studs(path, prefix, vars)

  } else if ( path %in% txt ) {

    studs <- get_txt_studs(path, prefix, vars)

  }

  studs$uid <- as.integer(studs$uid)

  return(studs)
}



# Helper functions and shared variables ---------------------------------

EMAs <- c("Activity",
          "Administration's response",
          "Behavior",
          "Boston Bombing",
          "Cancelled Classes",
          "Class",
          "Class 2",
          "Comment",
          "Dartmouth now",
          "Dimensions",
          "Dimensions protestors",
          "Dining Halls",
          "Do Campbell's jokes suck_",
          "Events",
          "Exercise",
          "Green Key 1",
          "Green Key 2",
          "Lab",
          "Mood",
          "Mood 1",
          "Mood 2",
          "PAM",
          "QR_Code",
          "Sleep",
          "Social",
          "Stress",
          "Study Spaces")

sensors <- c("activity",
             "audio",
             "bluetooth",
             "conversation",
             "dark",
             "gps",
             "phonecharge",
             "phonelock",
             "wifi",
             "wifi_location")

others <- c("app_usage",
            "calendar",
            "call_log",
            "dining",
            "sms")

educations <- c("class",
                "deadlines",
                "grades",
                "piazza")

surveys <- c("BigFive",
             "FlourishingScale",
             "LonelinessScale",
             "panas",
             "PerceivedStressScale",
             "PHQ-9",
             "psqi",
             "vr_12")


menu1_choices <- c("sensing", "EMA", "education", "survey", "other")
menu2_list <- list("sensing" = sensors,
                   "EMA/response" = EMAs,
                   "other" = others,
                   "education" = educations,
                   "survey" = surveys)


## In dataset/

# Open to wide csv
wide_csv <- c(paste0("survey/",surveys),
              paste0("education/", educations))

# Open to txt
txt <- c("dinning")

# Open to long format student csv
long_csv <- c("sms", "call_log", "calendar", "app_usage",
              paste0("sensing/", menu2_list[["sensing"]]))

# Open to student json
EMA_json <- paste0("EMA/response/", menu2_list[["EMA/response"]])


get_txt_studs <- function(path, prefix, vars) {

  `%>%` <- dplyr::`%>%`

  pr <- paste0(prefix, "/", path, "/", "u")
  paths <- c(paste0(pr, "0", seq(0,9), ".txt"),
             paste0(pr, seq(10,59), ".txt"))
  readr::read_csv(paths[2],
                  col_names = c("date-time","location","type"))
  studs <- list()
  missing_studs <- 0
  for (i in 1:60) {
    tryCatch({
      this_stud <- suppressMessages(
        readr::read_csv(paths[i], progress = FALSE,
                        col_names = c("date-time","location","type")))
      this_stud$uid <- i - 1
      studs[[length(studs)+1]] <- this_stud
    }, error = function(e){missing_studs <<- missing_studs + 1},
    warning = function(w) {})
  }

  # Bind students
  studs <- studs %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  if ( !missing(vars) ) studs <- dplyr::select(studs, vars)

  return(studs)
}


get_wide_csv_studs <- function(path, prefix, vars) {

  full_path <- paste0(prefix, "/", path, ".csv")
  studs <- utils::read.csv(full_path)

  studs$uid <- as.integer(substr(studs$uid, 2, 3))
  studs$uid <- NULL

  if(!missing(vars)) studs <- dplyr::select(studs, vars)
  studs <- tibble::as.tibble(studs)

  return(studs)
}



get_long_csv_studs <- function(path, prefix, vars) {

  `%>%` <- dplyr::`%>%`

  # Import the data
  splat <- unlist(strsplit(path, split='/', fixed=TRUE))
  name <- splat[length(splat)]
  if( name == "app_usage" ) name <- "running_app"
  if( name == "bluetooth" ) name <- "bt"
  pr <- paste0(prefix, "/", path, "/", name, "_u")
  paths <- c(paste0(pr, "0", seq(0,9), ".csv"),
             paste0(pr, seq(10,59), ".csv"))
  studs <- list()
  missing_studs <- 0
  if ( name == "wifi_location" || name == "gps" ) {
    for (i in 1:60) {
      tryCatch({
        col_names <- as.character(
          utils::read.csv(paths[i], nrows = 1,
                   stringsAsFactors = FALSE,
                   header = FALSE))
        this_stud <- suppressMessages(
          utils::read.csv(paths[i], skip = 1,
                   col.names = c(col_names, "to_drop"),
                   header = FALSE,
                   stringsAsFactors = FALSE))
        this_stud$to_drop <- NULL
        this_stud$uid <- i - 1
        studs[[length(studs)+1]] <- this_stud
      }, error = function(e){
        missing_studs <<- missing_studs + 1},
      warning = function(w) {})
    }
  } else {
    for (i in 1:60) {
      tryCatch({
        this_stud <- suppressMessages(
          readr::read_csv(paths[i], progress = FALSE))
        this_stud$uid <- i - 1
        studs[[length(studs)+1]] <- this_stud
      }, error = function(e){missing_studs <<- missing_studs + 1},
      warning = function(w) {})
    }
  }

  if ( path == "sms" & missing(vars) ) {
    studs <- lapply(studs, function(x){
      x <- dplyr::select(x, id, device, timestamp)
    })
  }

  # Bind students
  studs <- studs %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  if ( !missing(vars) ) studs <- dplyr::select(studs, vars)

  return(studs)
}



get_EMA_studs <- function(path, prefix, vars) {

  studs <- EMA_to_list(prefix, path)

  if ( missing(vars) ) {
    # Create a menu to choose vars

    vars_list <- attributes(studs)$vars_present

    vars_opt <- unlist(lapply(vars_list, function(x) {
      paste0(unlist(x), collapse = ", ")
    }))

    if ( length(vars_opt) > 1 ) {

      choice <- menu(choices = vars_opt,
                     title = "Choose vars:",
                     graphics = TRUE)
      vars <- vars_list[[choice]]
    } else {

      vars <- vars_list[[1]]
    }
  }

  studs <- EMA_list_to_tibble(studs, vars)

  ds <- attributes(studs)$dropped_students

  if (ds > 0) {
    warning(paste0(ds, " students were dropped ",
                   " with the choice of vars."))
  }

  return(studs)
}



get_path <- function(menu1, menu2) {

  if ( missing(menu1) & !missing(menu2) ) {
    stop(paste0("if menu2 is specified then menu1 ",
                "must be specified also"))
  }

  if ( missing(menu1) ) {

    menu1 <- menu1_choices[[utils::menu(
      choices = menu1_choices,
      title = "Choose Menu 1 option:")]]

  } else {

    menu1 <- menu1_choices[[menu1]]

  }

  if (menu1 == "EMA") menu1 <- "EMA/response"

  menu2_choices <- menu2_list[[menu1]]
  if ( missing(menu2) ) {

      menu2 <- utils::menu(
        choices = menu2_choices,
        title = "Choose Menu 2 option:")
  }

  if ( !is.null(menu2_choices) ) {

    menu2 <- menu2_choices[[menu2]]

  } else {

    menu2 <- NULL

  }

  result <- paste0(menu1, "/", menu2)
  splat <- unlist(strsplit(result, split='other/', fixed=TRUE))
  result <- splat[length(splat)]

  if ( result == "dining" ) result <- "dinning"

  return(result)
}

EMA_to_list <- function(prefix, path) {

  `%>%` <- dplyr::`%>%`

  splat <- unlist(strsplit(path, split='/', fixed=TRUE))
  EMA_name <- splat[length(splat)]

  pr <- paste0(prefix, "/", path, "/", EMA_name, "_u")
  paths <- c(paste0(pr, "0", seq(0,9), ".json"),
             paste0(pr, seq(10,59), ".json"))
  studs <- list()
  missing_studs <- 0
  for (i in 1:60) {
    tryCatch({
      this_stud <- jsonlite::fromJSON(paths[i])
      this_stud$uid <- i - 1
      studs[[length(studs)+1]] <- this_stud
    }, error = function(e){missing_studs <<- missing_studs + 1},
    warning = function(w){})
  }

  vars_present <- unique(lapply(studs, 'names'))

  if ( length(vars_present) == 0 ) {
    stop(paste0("There was an error finding data. ",
                "Perhaps Check the path prefix to ensure ",
                "it points to the top level of the StudentLife ",
                "dataset directory"))
  }

  for (i in 1:length(vars_present)) {
    if ( is.null(vars_present[[i]]) ) {
      vars_present[[i]] <- NULL; break
    }
  }

  attr(studs, "vars_present") <- vars_present

  return(studs)
}

EMA_list_to_tibble <- function(studs, vars = "resp_time") {

  # Drop lists not sharing variable names specified by parameter 'vars'
  null_ind <- c()
  for (i in 1:length(studs)) {
    if ( !all(vars %in% names(studs[[i]]) )) {
      null_ind <- c(null_ind, i)
    }
  }
  dropped_students <- length(null_ind)
  studs[null_ind] <- NULL

  # Drop all columns other than those specified by parameter 'vars'
  studs <- lapply(studs, function(x){
    dplyr::select(x, vars)
  })

  `%>%` <- dplyr::`%>%`

  # Bind and make tibble
  studs <- studs %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  attr(studs, "dropped_students") <- dropped_students

  return(studs)
}



## DISUSED OLD IMPORT FUNCTIONS

## #' EMA2tibble
## #'
## #' This function is mostly used for reading StudentLife EMA data into R.
## #' Note that \code{\link{na.exclude}} is used. Only the columns specified
## #' by \code{var} will be selected.
## #'
## #' @param prefix The path prefix.
## #' @param EMA_name The name of the folder in the StudentLife dataset,
## #' as a subfolder of the folder specified by \code{prefix}.
## #' Usually as a subfolder of the EMA/response directory.
## #' @param vars Vector of names of variables to read for each student.
## #' @param timestamp Character string indicating which variable
## #' contains unix time stamps to convert to columns date,
## #' day, week and month. Leave as default for no conversion.
## #' @param month1,week1,day1 Integers specifying the
## #' month, week and day of the year that the study begins.
## #' Best left as defaults.
## #' @param days_in_weeks Logical, only used if \code{timestamp}
## #' is not default. If \code{TRUE} then days are per week and
## #' factors rather than per year and integer.
## #'
## #'
## #' @examples
## #' # Import Stress EMAs
## #' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset/EMA/response/"
## #' n <-  "Stress"
## #' v <- c("level", "resp_time")
## #' stress <- JSON2tibble(p, n, v)
## #'
## #' # Import PAM EMAs
## #' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset/EMA/response/"
## #' n <-  "PAM"
## #' v <- c("picture_idx", "resp_time")
## #' pam1 <- JSON2tibble(p, n, v)
## #' pam2 <- JSON2tibble(p, n, v, timestamp = "resp_time")
## #' pam3 <- studentlife::JSON2tibble(p, n, v, "resp_time",
## #'                                 days_in_weeks = TRUE)
## #'
## #' @export
##
## JSON2tibble <- function(prefix, EMA_name, vars, timestamp = "",
##                         month1 = 3, week1 = 11, day1 = 83,
##                         days_in_weeks = FALSE) {
##
##   `%>%` <- dplyr::`%>%`
##
##   pr <- paste0(prefix, EMA_name, "/", EMA_name, "_u")
##   paths <- c(paste0(pr, "0", seq(0,9), ".json"),
##              paste0(pr, seq(10,59), ".json"))
##   studs <- list()
##   missing_studs <- 0
##   for (i in 1:60) {
##     tryCatch(studs[[length(studs)+1]] <- jsonlite::fromJSON(paths[i]),
##              error = function(e){missing_studs <<- missing_studs + 1},
##              warning = function(w){})
##   }
##
##   # Drop lists not sharing variable names specified by parameter 'vars'
##   null_ind <- c()
##   for (i in 1:length(studs)) {
##     if ( !all(vars %in% names(studs[[i]]) )) {
##       null_ind <- c(null_ind, i)
##     }
##   }
##   studs[null_ind] <- NULL
##
##   # Drop all columns other than those specified by parameter 'vars'
##   studs <- lapply(studs, function(x){
##     dplyr::select(x, vars)
##   })
##
##   studs <- do_transformations(studs, month1, week1, day1,
##                               days_in_weeks, timestamp)
##
##   return(studs)
## }
##
##
## #' csv2tibble
## #'
## #' This function is usually used for importing sensor data from csv
## #' files in the StudentLife dataset. Note that \code{\link{na.exclude}}
## #' is used.
## #'
## #' @param name The name of the folder in the StudentLife dataset,
## #' a subfolder of the folder specified by \code{prefix}
## #' @inheritParams JSON2tibble
## #'
## #' @examples
## #' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset/sensing/"
## #' n <- "conversation"
## #' con1 <- csv2tibble(p,n)
## #' con2 <- csv2tibble(p,n, timestamp = "start_timestamp")
## #' con3 <- csv2tibble(p,n, timestamp = "start_timestamp",
## #'                    days_in_weeks = TRUE)
## #' @export
##
## csv2tibble <- function(prefix, name, timestamp = "",
##                        month1 = 3, week1 = 11, day1 = 83,
##                        days_in_weeks = FALSE) {
##
##   `%>%` <- dplyr::`%>%`
##
##   # Import the data
##   pr <- paste0(prefix, name, "/", name, "_u")
##   paths <- c(paste0(pr, "0", seq(0,9), ".csv"),
##              paste0(pr, seq(10,59), ".csv"))
##   studs <- list()
##   missing_studs <- 0
##   for (i in 1:60) {
##     tryCatch(studs[[length(studs)+1]] <- utils::read.csv(paths[i]),
##              error = function(e){missing_studs <<- missing_studs + 1},
##              warning = function(w) {})
##   }
##
##   studs <- do_transformations(studs, month1, week1, day1,
##                               days_in_weeks, timestamp)
##
##   return(studs)
## }
##
##
##
## # Helper functions --------------------------------------------------------
## weekdays <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
##
##
## do_transformations <- function(studs, month1, week1, day1,
##                                days_in_weeks, timestamp) {
##
##   `%>%` <- dplyr::`%>%`
##
##   # Bind and exclude NAs
##   studs <- studs %>%
##     dplyr::bind_rows(.id = "student") %>%
##     na.exclude() %>%
##     tibble::as_tibble()
##
##   if ( !(timestamp == "") ) {
##     studs <- make_daily(timestamp, studs, month1, week1, day1)
##
##     if ( days_in_weeks ) {
##       studs <- make_days_in_weeks(studs)
##     }
##   }
##
##   return(studs)
## }
##
##
## make_daily <- function(timestamp, studs, month1, week1, day1) {
##
##   `%>%` <- dplyr::`%>%`
##
##   studs <- studs %>%
##     dplyr::mutate(
##       date = as.Date(as.POSIXct(get(timestamp),
##                                 origin="1970-01-01")),
##       month = as.numeric(format(date, "%m")) - month1,
##       week = as.numeric(format(date, "%W")) - week1,
##       day = as.numeric(format(date, "%j")) - day1)
##
## #  # Get days within weeks
## #  temp <- studs %>% dplyr::mutate(day = (day - 1) %% 7 )
## #
## #  start_week <- min(studs$week)
## #  start_day <- min(temp[which(temp$week == start_week),]$day)
## #
## #  attr(studs, "start_week") <- start_week
## #  attr(studs, "start_day") <- weekdays[start_day+1]
##
##   return(studs)
## }
##
##
## make_days_in_weeks <- function(studs) {
##
##   `%>%` <- dplyr::`%>%`
##
##   return( studs %>% dplyr::mutate(
##     day = factor(weekdays[((day - 1) %% 7 + 1)],
##                  levels = weekdays)) )
## }
##



