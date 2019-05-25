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

  if ( !is.factor(uids) || !(levels(uids) %in% getOption("SL_uids")) )
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

is_SL_tbl <- function (x)
{
  inherits(x, "SL_tbl")
}

is_dateonly_SL_tbl <- function (x)
{
  inherits(x, "dateonly_SL_tbl")
}

is_interval_SL_tbl <- function (x)
{
  inherits(x, "interval_SL_tbl")
}

is_timestamp_SL_tbl <- function (x)
{
  inherits(x, "timestamp_SL_tbl")
}

is_reg_SL_tbl <- function (x)
{
  inherits(x, "reg_SL_tbl")
}

strings_are_numeric <- function(x) {
  grepl("^[0-9]+$", x[!is.na(x)], perl=T)
}
