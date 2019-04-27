#' download_studentlife
#'
#' Download the entire StudentLife dataset
#'
#'@param url The url for the StudentLife dataset.
#'@param dest The destination path.
#'@param unzip Logical. If \code{TRUE} then the
#'dataset will be unzipped. Leave as default
#'unless you plan to do it manually.
#'@param untar Logical. If \code{TRUE} then the
#'dataset will be untarred. Leave as default
#'unless you plan to do it manually.
#'
#'@examples
#'d <- "D:/Datasets/studentlife"
#'download_studentlife(dest = d)
#'load_SL_tibble(location = d)
#'
#'@export

download_studentlife <- function(
  url = "https://studentlife.cs.dartmouth.edu/dataset/dataset.tar.bz2",
  dest = ".",
  unzip = TRUE,
  untar = TRUE) {

  message("Downloading the StudentLife dataset...")
  d <- "dataset.tar.bz2"
  p <- paste0(dest, "/", d)
  download.file(url = url, destfile = p)
  message("Download complete")

  if (unzip) {
    message("Unzipping the StudentLife dataset...")
    R.utils::bunzip2(p, remove = FALSE, skip = TRUE)
    message("Unzip complete")
    message(paste0("You may now wish to delete ",
                   "dataset.tar.bz2 ",
                   "to save disk space")) }

  if (untar) {
    d <- "dataset.tar"
    p <- paste0(dest, "/", d)
    message("Untarring the StudentLife dataset...")
    utils::untar(p, exdir = dest)
    message("Untar complete")
    message(paste0("You may now wish to delete ",
                   "dataset.tar ",
                   "to save disk space")) }
}



#' load_SL_tibble
#'
#' Import a chosen StudentLife table as
#' a tibble. Leave \code{schema} and \code{table}
#' unspecified to choose interactively via a
#' menu.
#'
#' @param schema An integer. The menu 1 choice. Leave
#' blank to choose interactively.
#' @param table An integer. The menu 2 choice. Leave
#' blank to choose interactively.
#' @param location The path to the top
#' directory of the StudentLife dataset.
#' @param time_options A character vector specifying which
#' table types to include in the menu, organised by level
#' of time information present. The default includes everything. Note
#' this parameter only has an effect when used with the interactive menu.
#' @param vars Character vector of variable
#' names to import for all students. Leave
#' blank and this will be chosen interactively
#' if necesssary.
#' @param csv_nrows An integer specifying the number of rows to read
#' per student if the target is a csv. The largest files in StudentLife are csv
#' files, so this allows code testing with less overhead.
#' @param datafolder Specifies the subfolder of \code{location}
#' that contains the relevant data. This should normally
#' be left as the default.
#'
#' @examples
#' p <- "D:/Datasets/studentlife"
#' students <- load_SL_tibble(location = p)
#'
#' @export
load_SL_tibble <- function(
  schema, table, location = ".",
  time_options = c("period", "timestamp", "dateonly", "dateless"),
  vars, csv_nrows, datafolder = "/dataset") {

  location <- paste0(location, datafolder)

  path <- get_path(location, schema, table, time_options)

  if ( path %in% EMA_json ) {

    studs <- get_EMA_studs(path, location, vars)

  } else if ( path %in% long_csv ) {

    studs <- get_long_csv_studs(path, location, vars, csv_nrows)

  } else if ( path %in% wide_csv ) {

    studs <- get_wide_csv_studs(path, location, vars, csv_nrows)

  } else if ( path %in% txt ) {

    studs <- get_txt_studs(path, location, vars)

  }

  studs$uid <- factor(studs$uid, levels = 0:59)

  return(studs)
}



# Helper functions ---------------------------------


get_txt_studs <- function(path, location, vars) {

  `%>%` <- dplyr::`%>%`

  pr <- paste0(location, "/", path, "/", "u")
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


get_wide_csv_studs <- function(path, location, vars, csv_nrows) {

  full_path <- paste0(location, "/", path, ".csv")
  name <- get_name_from_path(path)

  args <- list(file = full_path)
  cn <- c("uid", "class1", "class2", "class3", "class4")
  if (name == "class") {args$col.names = cn; args$header = FALSE}
  if (!missing(csv_nrows)) args$nrows = csv_nrows
  studs <- do.call(utils::read.csv, args)

  studs$uid <- as.integer(substr(studs$uid, 2, 3))
  studs <- tibble::as_tibble(studs)
  if(!missing(vars)) studs <- dplyr::select(studs, vars)


  return(studs)
}



get_long_csv_studs <- function(path, location, vars, csv_nrows) {

  `%>%` <- dplyr::`%>%`

  name <- get_name_from_path(path)
  if( name == "app_usage" ) name <- "running_app"
  if( name == "bluetooth" ) name <- "bt"
  paths <- generate_paths(location, path, name)

  args <- list()
  if ( !missing(csv_nrows) ) args$nrows <- csv_nrows

  studs <- list()
  missing_studs <- 0
  if ( name == "wifi_location" || name == "gps" ) {
    args2 <- c(args, list(skip = 1, header = FALSE,
      stringsAsFactors = FALSE))
    for (i in 1:60) {

      args2$file <- paths[i]
      tryCatch({
        col_names <- as.character(
          utils::read.csv(file = paths[i], nrows = 1,
                   stringsAsFactors = FALSE,
                   header = FALSE))
        args2$col.names <- c(col_names, "to_drop")
        this_stud <- suppressMessages(
          do.call(utils::read.csv, args2))
        this_stud$to_drop <- NULL
        this_stud$uid <- i - 1
        studs[[length(studs)+1]] <- this_stud
      }, error = function(e){
        missing_studs <<- missing_studs + 1},
      warning = function(w) {})
    }
  } else {
    if ( missing(csv_nrows) ) csv_nrows <- Inf
    for (i in 1:60) {
      tryCatch({
        this_stud <- suppressMessages(
          readr::read_csv(file = paths[i], progress = FALSE, n_max = csv_nrows))
        this_stud$uid <- i - 1
        studs[[length(studs)+1]] <- this_stud
      }, error = function(e){missing_studs <<- missing_studs + 1},
      warning = function(w) {})
    }
  }

  if ( path == "sms" & missing(vars) ) {
    studs <- lapply(studs, function(x){
      x <- dplyr::select(as.data.frame(x), id, device, timestamp)
    })
  }

  # Bind students
  studs <- studs %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  if ( !missing(vars) ) studs <- dplyr::select(studs, vars)

  return(studs)
}



get_EMA_studs <- function(path, location, vars) {

  studs <- EMA_to_list(location, path)

  if ( missing(vars) ) {
    # Create a graphical menu to choose vars

    vars_list <- attributes(studs)$vars_present

    vars_opt <- unlist(lapply(vars_list, function(x) {
      sort(paste0(unlist(x), collapse = ", "))
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
  ds <- paste0(ds, collapse = ", ")

  if (ds > 0) {
    warning(paste0("The students dropped ",
                   " with the choice of vars were numbers ", ds, "."))
  }

  return(studs)
}



get_path <- function(location, menu1, menu2, time_options) {

  if ( missing(menu1) & !missing(menu2) ) {
    stop(paste0("if menu2 is specified then menu1 ",
                "must be specified also"))
  }

  # Present interactive menu 1
  menu1_restrict <- unlist(time_opt_list1[time_options], use.names = FALSE)
  menu1_choices <- menu1_choices[which(menu1_choices %in% menu1_restrict)]
  if ( missing(menu1) ) {

    menu1 <- menu1_choices[[utils::menu(
      choices = menu1_choices,
      title = "Choose Menu 1 option:")]]

  } else {

    menu1 <- menu1_choices[[menu1]]

  }

  if (menu1 == "EMA") menu1 <- "EMA/response"

  # Present interactive menu 2
  menu2_choices <- menu2_list[[menu1]]
  menu2_restrict <- unlist(time_opt_list2[time_options], use.names = FALSE)
  menu2_choices <- menu2_choices[which(menu2_choices %in% menu2_restrict)]
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
  result <- get_name_from_path(result, split = 'other/')

  if ( result == "dining" ) result <- "dinning"

  return(result)
}

EMA_to_list <- function(location, path) {

  `%>%` <- dplyr::`%>%`

  name <- get_name_from_path(path)
  if (name == "QR_Code") name <- "QR"
  paths <- generate_paths(location, path, name, ext = ".json")

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

  vars_present <- unique(lapply(studs, function(x){sort(names(x))} ))

  if ( length(vars_present) == 0 ) {
    stop(paste0("There was an error finding data. ",
                "Perhaps Check the location to ensure ",
                "it points to the top level of the StudentLife ",
                "dataset directory"))
  }

  for (i in 1:length(vars_present)) {
    if ( is.null(vars_present[[i]]) ) {
      vars_present[[i]] <- NULL; break
    }
  }

  ## Get EMA definition information
  if (name == "PAM") {

    EMA_questions <- paste0("Refer to: Pollak, J. P., Adams, P., & Gay, G.",
                            " (2011, May). PAM: a photographic affect meter",
                            " for frequent, in situ measurement of affect.",
                            " In Proceedings of the SIGCHI conference on Human",
                            " factors in computing systems (pp. 725-734). ACM.")

  } else if (name == "QR") {

    name <- "QR_Code"
    EMA_questions <- paste0(
                     "Classroom seating positions. See the layout at ",
                     "https://studentlife.cs.dartmouth.edu/dataset.html")

  } else {
    EMA_definition <- jsonlite::fromJSON(
      paste0(location,"/EMA/EMA_definition.json"), flatten = TRUE)
    EMA_names <- gsub("\\?", "_", EMA_definition$name)
    EMA_questions <- EMA_definition[
      which(EMA_names == name),2][[1]]
    EMA_questions <- tibble::as_tibble(EMA_questions)
    EMA_questions <- EMA_questions[-which(EMA_questions$question_id == "location"),]
  }

  attr(studs, "missing_students") <- missing_studs
  attr(studs, "vars_present") <- vars_present
  attr(studs, "EMA_name") <- name
  attr(studs, "EMA_questions") <- EMA_questions

  return(studs)
}

EMA_list_to_tibble <- function(studs, vars = "resp_time") {

  studs_list <- studs

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
    dplyr::select(as.data.frame(x), vars)
  })

  `%>%` <- dplyr::`%>%`

  # Bind and make tibble
  studs <- studs %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  attr(studs_list, "dropped_students") <- null_ind - 1

  transfer_EMA_attrs(studs) <- studs_list

  class(studs) <- c("EMA_tbl", class(studs))

  return(studs)
}



# Shared variables  -------------------------------------------------------



EMA <- c("Activity",
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

sensing <- c("activity",
             "audio",
             "bluetooth",
             "conversation",
             "dark",
             "gps",
             "phonecharge",
             "phonelock",
             "wifi",
             "wifi_location")

other <- c("app_usage",
           "calendar",
           "call_log",
           "dining",
           "sms")

education <- c("class",
               "deadlines",
               "grades",
               "piazza")

survey <- c("BigFive",
            "FlourishingScale",
            "LonelinessScale",
            "panas",
            "PerceivedStressScale",
            "PHQ-9",
            "psqi",
            "vr_12")

# Tables that have a start_time and end_time (or 'start' and 'end') timestamp
period <- c("conversation", "dark", "phonecharge", "phonelock")

# Tables that have a timestamp
timestamp <- c(other,
               EMA,
               sensing[-which(sensing %in% period)])

# Tables that only have a date
dateonly <- c("deadlines")

# Tables that have no indication of time
dateless <- c(education[-which(education %in% dateonly)],
              survey)


#timestamp_names <- c("timestamp", "date-time", "resp_time")

time_opt_list1 <- list("period"    = c("sensing"),
                       "timestamp" = c("other", "EMA", "sensing"),
                       "dateonly"  = c("education"),
                       "dateless"  = c("education", "survey"))

time_opt_list2 <- list("period" = period,
                       "timestamp" = timestamp,
                       "dateonly" = dateonly,
                       "dateless" = dateless)

# These are the schemas
menu1_choices <- c("sensing", "EMA", "education", "survey", "other")

# List of tables by schema
menu2_list <- list("sensing" = sensing,
                   "EMA/response" = EMA,
                   "other" = other,
                   "education" = education,
                   "survey" = survey)


## In dataset/

# Open to wide csv
wide_csv <- c(paste0("survey/",survey),
              paste0("education/", education))

# Open to txt
txt <- c("dinning")

# Open to long format student csv
long_csv <- c("sms", "call_log", "calendar", "app_usage",
              paste0("sensing/", menu2_list[["sensing"]]))

# Open to student json
EMA_json <- paste0("EMA/response/", menu2_list[["EMA/response"]])



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
## #' # Import Stress EMA
## #' p <- "C:/Users/danie/Data/StudentLife/dataset/dataset/EMA/response/"
## #' n <-  "Stress"
## #' v <- c("level", "resp_time")
## #' stress <- JSON2tibble(p, n, v)
## #'
## #' # Import PAM EMA
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



