options_check <- function(par, opt) {
  par_name <- deparse(substitute(par))
  nr <- which(!(par %in% opt))
  if (length(nr) > 0) {
    nr_opt <- paste0(par[nr], collapse = ", ")
    stop(paste0(par_name, "(s) not recognised: ", nr_opt))
  }
}

`transfer_SL_tbl_attrs<-` <- function(to, ..., value) {

  if(!confirm_SL_tibble(value))
    stop("value is not an SL_tbl")

  to <- structure(
    to, schema = attr(value, "schema"),
    table = attr(value, "table"))

  return(to)
}

`transfer_EMA_attrs<-` <- function(to, ..., value) {


  to <- structure(
    to, dropped_students = attr(value, "dropped_students"),
    missing_students = attr(value, "missing_students"),
    EMA_name = attr(value, "EMA_name"),
    EMA_questions = attr(value, "EMA_questions"))

  return(to)
}


get_name_from_path <- function(path, split = '/') {
  splat <- unlist(strsplit(path, split=split, fixed=TRUE))
  name <- splat[length(splat)]
  return(name)
}

generate_paths <- function(location, path, name, ext = ".csv") {
  pr <- paste0(location, "/", path, "/", name, "_u")
  paths <- c(paste0(pr, "0", seq(0,9), ext),
             paste0(pr, seq(10,59), ext))
}

confirm_SL_tibble <- function(tab) {

  if ( !("SL_tbl" %in% class(tab)) )
    {warning("object not of class SL_tbl"); return(FALSE)}

  if ( !("uid" %in% names(tab)) )
    {warning("'uid' not in SL_tbl"); return(FALSE)}

  uids <- tab$uid

  if ( !is.factor(uids) || !all(levels(uids) %in% getOption("SL_uids")) )
    {warning("'uid' is not factor with correct levels"); return(FALSE)}

  schema <- attr(tab, "schema")
  table <- attr(tab, "table")

  if ( !(schema %in% menu_data$menu1_choices)
       || length(schema) > 1 )
    {warning("invalid schema name"); return(FALSE)}

  sch_num <- which(menu_data$menu1_choices == schema)

  if ( !(table %in% menu_data$menu2_list[[sch_num]])
       || length(table) > 1 )
    {warning("invalid table name"); return(FALSE)}

  return(TRUE)

}

confirm_interval_SL_tibble <- function(tab) {

  if ( !("start_timestamp" %in% names(tab)) )
    {warning("'start_timestamp' not in interval_SL_tbl"); return(FALSE)}

  if ( !("end_timestamp" %in% names(tab)) )
    {warning("'end_timestamp' not in interval_SL_tbl"); return(FALSE)}

  return(confirm_SL_tibble(tab))

}

confirm_timestamp_SL_tibble <- function(tab) {

  if ( !("timestamp" %in% names(tab)) )
    {warning("'timestamp' not in timestamp_SL_tbl"); return(FALSE)}

  return(confirm_SL_tibble(tab))
}

confirm_dateonly_SL_tibble <- function(tab) {

  # Note that a dateonly_SL_tbl may contain epoch information
  # if it was derived from a formerly timestamped SL_tbl

  if ( !("date" %in% names(tab)) )
    {warning("'date' not in dateonly_SL_tbl"); return(FALSE)}

  return(confirm_SL_tibble(tab))
}

confirm_reg_SL_tibble <- function(tab) {

  blocks <- attributes(tab)$blocks

  if ( length(blocks) == 0 )
    {warning("blocks attribute is empty"); return(FALSE)}

  return(confirm_SL_tibble(tab))
}

clean_strings <- function(x) {
  return(gsub('([[:punct:]])|\\s+','_', x))
}


#'is_SL_tibble
#'
#'Confirm that an object is a StudentLife tibble
#'
#'@param x Any object
#'
#'@return Logical
#'
#'@examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab_PAM <- load_SL_tibble(schema = "EMA", table = "PAM", location = d)
#'
#' # Returns TRUE
#' is_SL_tibble(tab_PAM)
#'
#'@export
is_SL_tibble <- function (x)
{
  confirm_SL_tibble(x) && inherits(x, "SL_tbl")
}


#'is_dateonly_SL_tibble
#'
#'Confirm that an object is a
#'date-only StudentLife tibble
#'
#'@param x Any object
#'
#'@return Logical
#'
#'@examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab_DL <- load_SL_tibble(
#'   schema = "education", table = "deadlines", location = d)
#'
#' # Returns TRUE
#' is_dateonly_SL_tibble(tab_DL)
#'
#'@export
is_dateonly_SL_tibble <- function (x)
{
  confirm_dateonly_SL_tibble(x) && inherits(x, "dateonly_SL_tbl")
}


#'is_interval_SL_tibble
#'
#'Confirm that an object is
#'an interval StudentLife tibble
#'
#'@param x Any object
#'
#'@return Logical
#'
#'@examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab_con <- load_SL_tibble(
#'   schema = "sensing", table = "conversation", location = d, csv_nrow = 10)
#'
#' # Returns TRUE
#' is_interval_SL_tibble(tab_con)
#'
#'@export
is_interval_SL_tibble <- function (x)
{
  confirm_interval_SL_tibble(x) && inherits(x, "interval_SL_tbl")
}


#'is_timestamp_SL_tibble
#'
#'Confirm that an object is a
#'timestamped StudentLife tibble
#'
#'@param x Any object
#'
#'@return Logical
#'
#'@examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab_PAM <- load_SL_tibble(schema = "EMA", table = "PAM", location = d)
#'
#' # Returns TRUE
#' is_timestamp_SL_tibble(tab_PAM)
#'
#'@export
is_timestamp_SL_tibble <- function (x)
{
  confirm_timestamp_SL_tibble(x) && inherits(x, "timestamp_SL_tbl")
}


#'is_reg_SL_tibble
#'
#'Confirm that an object is a
#'regularised StudentLife tibble
#'
#'@param x Any object
#'
#'@return Logical
#'
#'@examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab_PAM <- load_SL_tibble(schema = "EMA", table = "PAM", location = d)
#'
#' reg_PAM <- regularise_time(
#'   tab_PAM, blocks = c("day", "epoch"), m = mean(picture_idx, na.rm = TRUE))
#'
#' # Returns TRUE
#' is_reg_SL_tibble(reg_PAM)
#'
#'@export
is_reg_SL_tibble <- function (x)
{
  confirm_reg_SL_tibble(x) && inherits(x, "reg_SL_tbl")
}


#'is_dateless_SL_tibble
#'
#'Confirm that an object is a
#'dateless StudentLife tibble
#'
#'@param x Any object
#'
#'@return Logical
#'
#'@examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab_S <- load_SL_tibble(
#'   schema = "survey", table = "BigFive", location = d)
#'
#' # Returns TRUE
#' is_dateless_SL_tibble(tab_S)
#'
#'@export
is_dateless_SL_tibble <- function(x)
{
  inherits(x, "dateless_SL_tbl")
}


#'get_schema
#'
#'Retrieve the schema name from a StudentLife tibble
#'
#'@param x An object of class StudentLife tibble
#'(\code{SL_tbl}), as produced by the function
#'\code{\link[studentlife]{load_SL_tibble}}.
#'
#'@return A character string indicating the schema name
#'
#'@examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab_PAM <- load_SL_tibble(schema = "EMA", table = "PAM", location = d)
#'
#' # Returns "EMA"
#' get_schema(tab_PAM)
#'
#'@export
get_schema <- function(x) {
  return(attr(x,"schema"))
}

#'get_table
#'
#'Retrieve the table name from a StudentLife tibble
#'
#'@param x An object of class StudentLife tibble
#'(\code{SL_tbl}), as produced by the function
#'\code{\link[studentlife]{load_SL_tibble}}.
#'
#'@return A character string indicating the table name
#'
#'@examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab_PAM <- load_SL_tibble(schema = "EMA", table = "PAM", location = d)
#'
#' # Returns "PAM"
#' get_table(tab_PAM)
#'
#'@export
get_table <- function(x) {
  return(attr(x,"table"))
}

#' get_EMA_questions
#'
#' Get the EMA questions from a StudentLife tibble
#' whose schema is "EMA".
#'
#' @param x A StudentLife tibble whose schema is
#' EMA, as output by the function
#' \code{\link[studentlife]{load_SL_tibble}}.
#'
#' @return The EMA_questions attribute of \code{x}
#'
#'@examples
#' d <- tempdir()
#' download_studentlife(location = d, url = "testdata")
#'
#' tab_PAM <- load_SL_tibble(schema = "EMA", table = "PAM", location = d)
#'
#' # Returns "PAM"
#' get_EMA_questions(tab_PAM)
#'
#'@export
get_EMA_questions <- function(x) {
  return(attr(x, "EMA_questions"))
}


strings_are_numeric <- function(x) {
  grepl("^[0-9]+$", x[!is.na(x)], perl=T)
}
