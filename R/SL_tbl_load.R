#' download_studentlife
#'
#' Download the entire StudentLife dataset
#'
#'@param url The url for the StudentLife dataset.
#'@param dest The destination path. If the path does
#'not exist it is created with \code{\link{dir.create}}
#'@param unzip Logical. If \code{TRUE} then the
#'dataset will be unzipped with \code{\link[R.utils]{bunzip2}}.
#'Leave as default unless you plan to do it manually.
#'@param untar Logical. If \code{TRUE} then the
#'dataset will be untarred with \code{\link[utils]{untar}}.
#'Leave as default unless you plan to do it manually.
#'
#'@examples
#'\donttest{
#'d <- "D:/Datasets/studentlife"
#'download_studentlife(dest = d)
#'
#'## With menu
#'load_SL_tibble(location = d)
#'
#'## Without menu
#'SL_tables
#'load_SL_tibble(schema = "EMA", table = "PAM", location = d)
#'}
#'
#'@export
download_studentlife <- function(
  url = paste0("https://studentlife.cs.dartmouth.edu",
               "/dataset/dataset.tar.bz2"),
  dest = ".",
  unzip = TRUE,
  untar = TRUE) {

  message("Downloading the StudentLife dataset...")
  d <- "dataset.tar.bz2"
  p <- paste0(dest, "/", d)
  if (!dir.exists(dest)) dir.create(dest)
  utils::download.file(url = url, destfile = p)
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
#' @param schema A character string. The menu 1 choice. Leave
#' blank to choose interactively.
#' @param table A character string. The menu 2 choice. Leave
#' blank to choose interactively.
#' @param location The path to a copy of the StudentLife dataset.
#' @param time_options A character vector specifying which
#' table types (out of "interval", "timestamp", "dateonly" and "dateless")
#' to include in the menu. This allows you to restrict menu options
#' according to the amount of date-time information present in the data.
#' The default includes all data. Note
#' this parameter only has an effect when used with the interactive menu.
#' @param vars Character vector of variable
#' names to import for all students. Leave
#' blank and this will be chosen interactively
#' if necesssary. If \code{vars} contains
#' "timestamp" then effort will be made
#' to convert "timestamp" to appropriate
#' variable name(s) for the target table.
#' @param csv_nrows An integer specifying the number of rows to read
#' per student if the target is a csv. The largest files in StudentLife are csv
#' files, so this allows code testing with less overhead.
#' @param datafolder Specifies the subfolder of \code{location}
#' that contains the relevant data. This should normally
#' be left as the default.
#' @param uid_range An integer vector. The range of uids in
#' the StudentLife study.
#'
#' @return
#' An object of class \code{SL_tibble} is returned. These inherit
#' properties from class \code{\link[tibble]{tibble}} and
#' class \code{\link{data.frame}}.
#' Depending on the date-time information available, the object
#' may also be a \code{timestamp_SL_tibble},
#' \code{interval_SL_tibble} or
#' \code{dateonly_SL_tibble} (which are all
#' subclasses of \code{SL_tibble}).
#'
#' @examples
#' \donttest{
#'d <- "D:/Datasets/studentlife"
#'download_studentlife(dest = d)
#'
#'## With menu
#'load_SL_tibble(location = d)
#'
#'## Without menu
#'SL_tables
#'PAM <- load_SL_tibble(schema = "EMA", table = "PAM", location = d)
#'
#'## Load less data for testing with less overhead
#'act <- load_SL_tibble(schema = "sensing", table = "activity",
#'                      location = d, csv_nrows = 10)
#'
#'## Browse all tables with timestamps (non-interval)
#'load_SL_tibble(location = d, time_options = "timestamp", csv_nrows = 10)
#'
#'## Browse all tables with intervals
#'load_SL_tibble(location = d, time_options = "interval", csv_nrows = 10)
#'
#'## Browse all dateless tables
#'load_SL_tibble(location = d, time_options = "dateless", csv_nrows = 10)
#'}
#'
#' @export
load_SL_tibble <- function(
  schema, table, location = ".",
  time_options = c("interval", "timestamp", "dateonly", "dateless"),
  vars, csv_nrows, datafolder = "dataset",
  uid_range = getOption("SL_uids")) {

  time_options <- tolower(time_options)
  datafolder <- paste0("/", datafolder)

  opt <- c("interval", "timestamp", "dateonly", "dateless")
  options_check(par = time_options, opt = opt)

  if ( !dir.exists(location) )
    stop("the directory specified by location parameter does not exist")

  if ( !dir.exists(paste0(location, datafolder)) )
    stop(paste0("the location does not have a subfolder named ", datafolder))

  if (!missing(vars)) {
    if( !("uid" %in% vars ) ) vars <- c("uid", vars)
  }

  if ( missing(schema) & !missing(table) ) {
    stop(paste0("if table is specified then schema ",
                "must be specified also"))
  }

  if (!missing(schema))
    schema <- pmatch(tolower(schema), tolower(menu_data$menu1_choices))

  if (!missing(table))
    table <- pmatch(tolower(table), tolower(menu_data$menu2_list[[schema]]))

  location <- paste0(location, datafolder)

  path <- get_path(location, schema, table, time_options)

  if ( path %in% menu_data$EMA_json ) {

    tab <- get_EMA_tab(path, location, vars)

  } else if ( path %in% menu_data$long_csv ) {

    tab <- get_long_csv_tab(path, location, vars, csv_nrows)

  } else if ( path %in% menu_data$wide_csv ) {

    tab <- get_wide_csv_tab(path, location, vars, csv_nrows)

  } else if ( path %in% menu_data$txt ) {

    tab <- get_txt_tab(path, location, vars)

  }

  tab$uid <- factor(tab$uid, levels = uid_range)

  tab <- structure(
    tab, schema = attr(path, "schema"),
    table = attr(path, "table"))

  names(tab) <- clean_strings(names(tab))

  return(tab)
}


############################################################################
############################################################################
# Helper functions ---------------------------------

get_path <- function(location, menu1, menu2, time_options) {


  # Present interactive menu 1
  menu1_restrict <- unlist(
    menu_data$time_opt_list1[time_options], use.names = FALSE)
  menu1_choices <- menu_data$menu1_choices[
    which(menu_data$menu1_choices %in% menu1_restrict)]
  if ( missing(menu1) ) {

    menu1 <- menu1_choices[[utils::menu(
      choices = menu1_choices,
      title = "Choose Menu 1 option:")]]
  } else {

    menu1 <- menu1_choices[[menu1]]
  }

  schema <- menu1

  if (menu1 == "EMA") menu1 <- "EMA/response"

  # Present interactive menu 2
  menu2_choices <- menu_data$menu2_list[[menu1]]
  menu2_restrict <- unlist(
    menu_data$time_opt_list2[time_options], use.names = FALSE)
  menu2_choices <- menu2_choices[
    which(menu2_choices %in% menu2_restrict)]
  if ( missing(menu2) ) {

    menu2 <- utils::menu(
      choices = menu2_choices,
      title = "Choose Menu 2 option:")
  }

  if (is.null(menu2_choices))
    stop("No tables found in specified schema")

  menu2 <- menu2_choices[[menu2]]
  table <- menu2

  result <- paste0(menu1, "/", menu2)
  result <- get_name_from_path(result, split = 'other/')

  if ( result == "dining" ) result <- "dinning"

  result <- structure(result, schema = schema, table = table)

  return(result)
}

get_txt_tab <- function(path, location, vars) {

  `%>%` <- dplyr::`%>%`

  if ( !missing(vars) )
    if ( "timestamp" %in% vars )
      vars[pmatch("timestamp", vars)] <- "date-time"


  pr <- paste0(location, "/", path, "/", "u")
  paths <- c(paste0(pr, "0", seq(0,9), ".txt"),
             paste0(pr, seq(10,59), ".txt"))

  readr::read_csv(paths[2],
                  col_names = c("date-time","location","type"))
  tab <- list()
  missing_tab <- 0
  for (i in 1:60) {
    if(file.exists(paths[i])) {
      this_stud <- suppressMessages(
        readr::read_csv(paths[i], progress = FALSE,
                        col_names = c("date-time","location","type")))
      this_stud$uid <- i - 1
      tab[[length(tab)+1]] <- this_stud
    } else {
      missing_tab <<- missing_tab + 1
    }
  }

  # Bind students
  tab <- tab %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  if ( !missing(vars) ) tab <- dplyr::select(tab, vars)

  class(tab) <- c("SL_tbl", class(tab))

  if ( "date-time" %in% names(tab) ) {
    names(tab)[pmatch("date-time", names(tab))] <- "timestamp"
    tab$timestamp <- as.numeric(
      as.POSIXct(tab$timestamp, origin="1970-01-01"))
    class(tab) <- c("timestamp_SL_tbl", class(tab))
  }

  return(tab)
}


get_wide_csv_tab <- function(path, location, vars, csv_nrows) {

  full_path <- paste0(location, "/", path, ".csv")
  name <- get_name_from_path(path)

  args <- list(file = full_path)
  cn <- c("uid", "class1", "class2", "class3", "class4")
  if (name == "class") {args$col.names = cn; args$header = FALSE}
  # if (!missing(csv_nrows)) args$nrows = csv_nrows
  tab <- do.call(utils::read.csv, args)

  tab$uid <- as.integer(substr(tab$uid, 2, 3))
  tab <- tibble::as_tibble(tab)
  if(!missing(vars)) tab <- dplyr::select(tab, vars)


  if ( name %in% menu_data$dateonly ) {

    tab <- tidyr::gather(tab, "date", "deadlines", -c("uid"))
    tab$date <- as.Date(
      as.POSIXct(substr(tab$date,2,11), format = "%Y.%m.%d"))

    class(tab) <- c("SL_tbl", class(tab))

    class(tab) <- c("dateonly_SL_tbl", class(tab))

  } else {

    class(tab) <- c("SL_tbl", class(tab))

  }

  return(tab)
}



get_long_csv_tab <- function(path, location, vars, csv_nrows) {

  `%>%` <- dplyr::`%>%`

  name <- get_name_from_path(path)
  if( name == "app_usage" ) name <- "running_app"
  if( name == "bluetooth" ) name <- "bt"
  paths <- generate_paths(location, path, name)

  if( !missing(vars) )
    if( "timestamp" %in% vars ) {
      if( name %in% menu_data$interval ) {
        if ( name == "conversation" ) {
          vars[pmatch("timestamp", vars)] <- "start_timestamp"
          vars <- c(vars, "end_timestamp")
        } else {
          vars[pmatch("timestamp", vars)] <- "start"
          vars <- c(vars, "end")
        }
      } else if ( name %in% c("bt","gps","wifi","wifi_location") ) {
        vars[pmatch("timestamp", vars)] <- "time"
      }
    }

  args <- list()
  if ( !missing(csv_nrows) ) args$nrows <- csv_nrows

  tab <- list()
  missing_tab <- 0
  if ( name == "wifi_location" || name == "gps" ) {
    args2 <- c(args, list(skip = 1, header = FALSE,
      stringsAsFactors = FALSE))
    for (i in 1:60) {
      if(file.exists(paths[i])) {
        args2$file <- paths[i]
        col_names <- as.character(
          utils::read.csv(file = paths[i], nrows = 1,
                          stringsAsFactors = FALSE,
                          header = FALSE))
        args2$col.names <- c(col_names, "to_drop")
        this_stud <- suppressMessages(
          do.call(utils::read.csv, args2))
        this_stud$to_drop <- NULL
        this_stud$uid <- i - 1
        tab[[length(tab)+1]] <- this_stud
      } else {
        missing_tab <<- missing_tab + 1
      }
    }
  } else {
    if ( missing(csv_nrows) ) csv_nrows <- Inf
    for (i in 1:60) {
      if(file.exists(paths[i])) {
        this_stud <- suppressMessages(
          readr::read_csv(
            file = paths[i], progress = FALSE, n_max = csv_nrows))
        this_stud$uid <- i - 1
        tab[[length(tab)+1]] <- this_stud
      } else {
        missing_tab <<- missing_tab + 1
      }
    }
  }

  if ( path == "sms" && missing(vars) ) {
    tab <- lapply(tab, function(x){
      x <- dplyr::select(
        as.data.frame(x), "id", "device", "timestamp", "uid")
    })
  }

  # Bind students
  tab <- tab %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  if ( !missing(vars) ) tab <- dplyr::select(tab, vars)

  if( name %in% menu_data$interval ) {
    if ( !(name == "conversation") ) {
      if ( "start" %in% names(tab) )
        names(tab)[pmatch("start", names(tab))] <- "start_timestamp"
      if ( "end" %in% names(tab) )
        names(tab)[pmatch("end", names(tab))] <- "end_timestamp"
    }
  } else if ( name %in% c("bt","gps","wifi","wifi_location") ) {
    if ( "time" %in% names(tab) )
      names(tab)[pmatch("time", names(tab))] <- "timestamp"
  }

  class(tab) <- c("SL_tbl", class(tab))

  if ("timestamp" %in% names(tab))
    class(tab) <- c("timestamp_SL_tbl", class(tab))

  if ("start_timestamp" %in% names(tab) && "end_timestamp" %in% names(tab))
    class(tab) <- c("interval_SL_tbl", class(tab))

  return(tab)
}



get_EMA_tab <- function(path, location, vars) {

  if (!missing(vars) ) {
    if ("timestamp" %in% vars) {
      vars[pmatch("timestamp", vars)] <- "resp_time"
    }
  }

  tab <- EMA_to_list(location, path)

  if ( missing(vars) ) {
    # Create a graphical menu to choose vars

    vars_list <- attributes(tab)$vars_present

    vars_opt <- unlist(lapply(vars_list, function(x) {
      sort(paste0(unlist(x), collapse = ", "))
    }))

    if ( length(vars_opt) > 1 ) {

      choice <- utils::menu(choices = vars_opt,
                     title = "Choose vars:",
                     graphics = TRUE)
      vars <- vars_list[[choice]]
    } else {

      vars <- vars_list[[1]]
    }
  }

  tab <- EMA_list_to_tibble(tab, vars)

  ds <- attributes(tab)$dropped_students
  ds <- paste0(ds, collapse = ", ")

  if (ds > 0) {
    message(paste0("The students dropped ",
                   " with the choice of vars were numbers ", ds, "."))
  }

  class(tab) <- c("SL_tbl", class(tab))

  if ("timestamp" %in% names(tab)) {
    class(tab) <- c("EMA_SL_tbl", "timestamp_SL_tbl", class(tab))
  }

  return(tab)
}

EMA_to_list <- function(location, path) {

  `%>%` <- dplyr::`%>%`

  name <- get_name_from_path(path)
  if (name == "QR_Code") name <- "QR"
  paths <- generate_paths(location, path, name, ext = ".json")

  tab <- list()
  missing_tab <- 0
  for (i in 1:60) {
    if( file.exists(paths[i])
        && readLines(paths[i], 1, warn = FALSE) != "[]") {
      this_stud <- jsonlite::fromJSON(paths[i])
      this_stud$uid <- i - 1
      tab[[length(tab)+1]] <- this_stud
    } else {
      missing_tab <<- missing_tab + 1
    }
  }

  vars_present <- unique(lapply(tab, function(x){sort(names(x))} ))

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

    EMA_questions <-
      paste0("Refer to: Pollak, J. P., Adams, P., & Gay, G.",
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
      pmatch(name, EMA_names),2][[1]]
    EMA_questions <- tibble::as_tibble(EMA_questions)
    EMA_questions <- EMA_questions[
      -pmatch("location", EMA_questions$question_id),]
  }

  tab <- structure(
    tab, missing_students = missing_tab,
    vars_present = vars_present,
    EMA_name = name,
    EMA_questions = EMA_questions)

  return(tab)
}

EMA_list_to_tibble <- function(tab, vars = "resp_time") {

  tab_list <- tab

  # Drop lists not sharing variable names specified by parameter 'vars'
  null_ind <- c()
  for (i in 1:length(tab)) {
    if ( !all(vars %in% names(tab[[i]]) )) {
      null_ind <- c(null_ind, i)
    }
  }
  tab[null_ind] <- NULL

  # Drop all columns other than those specified by parameter 'vars'
  tab <- lapply(tab, function(x){
    dplyr::select(as.data.frame(x), vars)
  })

  `%>%` <- dplyr::`%>%`

  # Bind and make tibble
  tab <- tab %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  if ( "resp_time" %in% names(tab) )
    names(tab)[pmatch("resp_time", names(tab))] <- "timestamp"

  attr(tab_list, "dropped_students") <- null_ind - 1
  transfer_EMA_attrs(tab) <- tab_list

  return(tab)
}
