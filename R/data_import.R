#' download_studentlife
#'
#' Download the entire StudentLife dataset
#'
#'@param url The url for the StudentLife dataset.
#'@param dest The destination path. If the path does
#'not exist it is created with \code{\link{dir.create}}
#'@param unzip Logical. If \code{TRUE} then the
#'dataset will be unzipped with \code{\link{bunzip2}}.
#'Leave as default unless you plan to do it manually.
#'@param untar Logical. If \code{TRUE} then the
#'dataset will be untarred with \code{\link{untar}}.
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
  url = "https://studentlife.cs.dartmouth.edu/dataset/dataset.tar.bz2",
  dest = ".",
  unzip = TRUE,
  untar = TRUE) {

  message("Downloading the StudentLife dataset...")
  d <- "dataset.tar.bz2"
  p <- paste0(dest, "/", d)
  if (!dir.exists(p)) dir.create(p)
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
#' table types to include in the menu, organised by level
#' of time information present. The default includes everything. Note
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
  vars, csv_nrows, datafolder = "/dataset") {

  if (!missing(vars)) {
    if( !("uid" %in% vars ) ) vars <- c("uid", vars)
  }

  if (!missing(schema))
    schema <- which(tolower(schema) == tolower(menu_data$menu1_choices))

  if (!missing(table))
    table <- which(tolower(table) == tolower(menu_data$menu2_list[[schema]]))

  location <- paste0(location, datafolder)

  path <- get_path(location, schema, table, time_options)

  if ( path %in% menu_data$EMA_json ) {

    studs <- get_EMA_studs(path, location, vars)

  } else if ( path %in% menu_data$long_csv ) {

    studs <- get_long_csv_studs(path, location, vars, csv_nrows)

  } else if ( path %in% menu_data$wide_csv ) {

    studs <- get_wide_csv_studs(path, location, vars, csv_nrows)

  } else if ( path %in% menu_data$txt ) {

    studs <- get_txt_studs(path, location, vars)

  }

  studs$uid <- factor(studs$uid, levels = 0:59)

  return(studs)
}


############################################################################
############################################################################
# Helper functions ---------------------------------


get_txt_studs <- function(path, location, vars) {

  `%>%` <- dplyr::`%>%`

  if ( !missing(vars) )
    if ( "timestamp" %in% vars )
      vars[which(vars == "timestamp")] <- "date-time"


  pr <- paste0(location, "/", path, "/", "u")
  paths <- c(paste0(pr, "0", seq(0,9), ".txt"),
             paste0(pr, seq(10,59), ".txt"))

  readr::read_csv(paths[2],
                  col_names = c("date-time","location","type"))
  studs <- list()
  missing_studs <- 0
  for (i in 1:60) {
    if(file.exists(paths[i])) {
      this_stud <- suppressMessages(
        readr::read_csv(paths[i], progress = FALSE,
                        col_names = c("date-time","location","type")))
      this_stud$uid <- i - 1
      studs[[length(studs)+1]] <- this_stud
    } else {
      missing_studs <<- missing_studs + 1
    }
  }

  # Bind students
  studs <- studs %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  if ( !missing(vars) ) studs <- dplyr::select(studs, vars)

  class(studs) <- c("SL_tbl", class(studs))

  if ( "date-time" %in% names(studs) ) {
    names(studs)[which(names(studs) == "date-time")] <- "timestamp"
    studs$timestamp <- as.numeric(
      as.POSIXct(studs$timestamp, origin="1970-01-01"))
    class(studs) <- c("timestamp_SL_tbl", class(studs))
  }

  return(studs)
}


get_wide_csv_studs <- function(path, location, vars, csv_nrows) {

  full_path <- paste0(location, "/", path, ".csv")
  name <- get_name_from_path(path)

  args <- list(file = full_path)
  cn <- c("uid", "class1", "class2", "class3", "class4")
  if (name == "class") {args$col.names = cn; args$header = FALSE}
  # if (!missing(csv_nrows)) args$nrows = csv_nrows
  studs <- do.call(utils::read.csv, args)

  studs$uid <- as.integer(substr(studs$uid, 2, 3))
  studs <- tibble::as_tibble(studs)
  if(!missing(vars)) studs <- dplyr::select(studs, vars)


  class(studs) <- c("SL_tbl", class(studs))

  if ( name == "deadlines" ) {

    studs <- tidyr::gather(studs, "date", "deadlines", -c("uid"))
    studs$date <- as.Date(
      as.POSIXct(substr(studs$date,2,11), format = "%Y.%m.%d"))

    class(studs) <- c("dateonly_SL_tbl", class(studs))
  }

  return(studs)
}



get_long_csv_studs <- function(path, location, vars, csv_nrows) {

  `%>%` <- dplyr::`%>%`

  name <- get_name_from_path(path)
  if( name == "app_usage" ) name <- "running_app"
  if( name == "bluetooth" ) name <- "bt"
  paths <- generate_paths(location, path, name)

  if( !missing(vars) )
    if( "timestamp" %in% vars ) {
      if( name %in% menu_data$interval ) {
        if ( name == "conversation" ) {
          vars[which(vars == "timestamp")] <- "start_timestamp"
          vars <- c(vars, "end_timestamp")
        } else {
          vars[which(vars == "timestamp")] <- "start"
          vars <- c(vars, "end")
        }
      } else if ( name %in% c("bt","gps","wifi","wifi_location") ) {
        vars[which(vars == "timestamp")] <- "time"
      }
    }

  args <- list()
  if ( !missing(csv_nrows) ) args$nrows <- csv_nrows

  studs <- list()
  missing_studs <- 0
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
        studs[[length(studs)+1]] <- this_stud
      } else {
        missing_studs <<- missing_studs + 1
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
        studs[[length(studs)+1]] <- this_stud
      } else {
        missing_studs <<- missing_studs + 1
      }
    }
  }

  if ( path == "sms" && missing(vars) ) {
    studs <- lapply(studs, function(x){
      x <- dplyr::select(
        as.data.frame(x), "id", "device", "timestamp", "uid")
    })
  }

  # Bind students
  studs <- studs %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  if ( !missing(vars) ) studs <- dplyr::select(studs, vars)

  if( name %in% menu_data$interval ) {
    if ( !(name == "conversation") ) {
      if ( "start" %in% names(studs) )
        names(studs)[which(names(studs) == "start")] <- "start_timestamp"
      if ( "end" %in% names(studs) )
        names(studs)[which(names(studs) == "end")] <- "end_timestamp"
    }
  } else if ( name %in% c("bt","gps","wifi","wifi_location") ) {
    if ( "time" %in% names(studs) )
      names(studs)[which(names(studs) == "time")] <- "timestamp"
  }

  class(studs) <- c("SL_tbl", class(studs))

  if ("timestamp" %in% names(studs))
    class(studs) <- c("timestamp_SL_tbl", class(studs))

  if ("start_timestamp" %in% names(studs) && "end_timestamp" %in% names(studs))
    class(studs) <- c("interval_SL_tbl", class(studs))

  return(studs)
}



get_EMA_studs <- function(path, location, vars) {

  if (!missing(vars) ) {
    if ("timestamp" %in% vars) {
      vars[which(vars == "timestamp")] <- "resp_time"
    }
  }

  studs <- EMA_to_list(location, path)

  if ( missing(vars) ) {
    # Create a graphical menu to choose vars

    vars_list <- attributes(studs)$vars_present

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

  studs <- EMA_list_to_tibble(studs, vars)

  ds <- attributes(studs)$dropped_students
  ds <- paste0(ds, collapse = ", ")

  if (ds > 0) {
    message(paste0("The students dropped ",
                   " with the choice of vars were numbers ", ds, "."))
  }

  class(studs) <- c("SL_tbl", class(studs))

  if ("timestamp" %in% names(studs)) {
    class(studs) <- c("EMA_SL_tbl", "timestamp_SL_tbl", class(studs))
  }

  return(studs)
}

EMA_to_list <- function(location, path) {

  `%>%` <- dplyr::`%>%`

  name <- get_name_from_path(path)
  if (name == "QR_Code") name <- "QR"
  paths <- generate_paths(location, path, name, ext = ".json")

  studs <- list()
  missing_studs <- 0
  for (i in 1:60) {
    if( file.exists(paths[i])
        && readLines(paths[i], 1, warn = FALSE) != "[]") {
      this_stud <- jsonlite::fromJSON(paths[i])
      this_stud$uid <- i - 1
      studs[[length(studs)+1]] <- this_stud
    } else {
      missing_studs <<- missing_studs + 1
    }
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
      which(EMA_names == name),2][[1]]
    EMA_questions <- tibble::as_tibble(EMA_questions)
    EMA_questions <- EMA_questions[
      -which(EMA_questions$question_id == "location"),]
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

  if ( "resp_time" %in% names(studs) )
    names(studs)[which(names(studs) == "resp_time")] <- "timestamp"

  attr(studs_list, "dropped_students") <- null_ind - 1
  transfer_EMA_attrs(studs) <- studs_list

  return(studs)
}


get_path <- function(location, menu1, menu2, time_options) {

  if ( missing(menu1) & !missing(menu2) ) {
    stop(paste0("if menu2 is specified then menu1 ",
                "must be specified also"))
  }

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
