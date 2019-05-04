options_check <- function(par, opt) {
  par_name <- deparse(substitute(par))
  nr <- which(!(par %in% opt))
  if (length(nr) > 0) {
    nr_opt <- paste0(par[nr], collapse = ", ")
    stop(paste0(par_name, "(s) not recognised: ", nr_opt))
  }
}

`transfer_EMA_attrs<-` <- function(to, ..., value) {

  attr(to, "dropped_students") <- attr(value, "dropped_students")
  attr(to, "missing_students") <- attr(value, "missing_students")
  attr(to, "EMA_name") <- attr(value, "EMA_name")
  attr(to, "EMA_questions") <- attr(value, "EMA_questions")

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

confirm_SL_tibble <- function(studs) {

  if ( !("SL_tbl" %in% class(studs)) )
    {warning("object not of class SL_tbl"); return(FALSE)}

  if ( !("uid" %in% names(studs)) )
    {warning("'uid' not in SL_tbl"); return(FALSE)}

  schema <- attr(studs, "schema")
  table <- attr(studs, "table")

  if ( !(schema %in% menu_data$menu1_choices)
       || length(schema) > 1 )
    {warning("invalid schema name"); return(FALSE)}

  sch_num <- which(menu_data$menu1_choices == schema)

  if ( !(table %in% menu_data$menu2_list[[sch_num]])
       || length(table) > 1 )
    {warning("invalid table name"); return(FALSE)}

  return(TRUE)

}

confirm_interval_SL_tibble <- function(studs) {

  if ( !("start_timestamp" %in% names(studs)) )
    {warning("'start_timestamp' not in interval_SL_tbl"); return(FALSE)}

  if ( !("end_timestamp" %in% names(studs)) )
    {warning("'end_timestamp' not in interval_SL_tbl"); return(FALSE)}

  return(confirm_SL_tibble(studs))

}

confirm_timestamp_SL_tibble <- function(studs) {

  if ( !("timestamp" %in% names(studs)) )
    {warning("'timestamp' not in timestamp_SL_tbl"); return(FALSE)}

  return(confirm_SL_tibble(studs))
}

confirm_dateonly_SL_tibble <- function(studs) {

  if ( !("date" %in% names(studs)) )
    {warning("'date' not in dateonly_SL_tbl"); return(FALSE)}

  return(confirm_SL_tibble(studs))
}

confirm_reg_SL_tibble <- function(studs) {

  blocks <- attributes(studs)$blocks

  if ( length(blocks) == 0 )
    {warning("blocks attribute is null"); return(FALSE)}

  return(confirm_SL_tibble(studs))
}

clean_strings <- function(x) {
  return(gsub('([[:punct:]])|\\s+','_', x))
}

