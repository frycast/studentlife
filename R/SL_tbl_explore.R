#' observation_block_hist
#'
#' Produce a histogram with frequency of observations
#' in each hour or number of hours
#'
#' @param studs A timestamp_SL_tbl or interval_SL_tbl.
#' @param break_hours Specify the width in hours of each
#' histogram bin.
#' @param xlab Argument passed to \code{\link[graphics]{hist}}.
#' @param main Argument passed to \code{\link[graphics]{hist}}.
#' @param ... Arguments passed to \code{\link[graphics]{hist}}.
#'
#' @export
response_hour_hist <- function(studs, break_hours = 1,
                               xlab = "Hours into study",
                               main = "Distribution of response times",
                               ...) {

  if ( "timestamp_SL_tbl" %in% class(studs) ) {

    res_t <- studs[["timestamp"]]/3600

  } else if ( "interval_SL_tbl" %in% class(studs) ) {

    res_t <- studs[["start_timestamp"]]/3600

  } else {

    stop("studs not of class timestamp_SL_tbl or interval_SL_tbl")

  }

  minr <- min(res_t); maxr <- max(res_t)
  br <- seq(0, maxr - minr + 1, by = break_hours)
  hist(res_t - minr, ..., breaks = br,
       xlab = xlab,
       main = main)
}

response_hour_hist(studs_t, breaks_by = 10)








##### INCOMPLETE
### ISSUES
## Needs documentation
##
###
##' strongest_uids
##'
##' Returns the uids of the n students in the
##' data.frame that have the most responses
##'
##'@export
#strongest_uids <- function(studs, n = 10) {
#
#  `%>%` <- dplyr::`%>%`
#
#  return((studsg %>% dplyr::group_by(uid) %>%
#           summarise(nr = dplyr::n()) %>%
#           dplyr::top_n(n, nr))$uid)
#}
#
#
#
#
#### INCOMPLETE
### ISSUES
## Needs documentation and generality
##
###
##' vis_stud_NAs
##'
##' This function should be applied after adding
##' NAs to the imported data with \code{\link{add_NAs}}
##'
##'
##' @export
#vis_stud_NAs <- function(studs) {
#
#  `%>%` <- dplyr::`%>%`
#
#  wide_studsg_NA <- studs %>%
#    dplyr::select(epoch, day, uid, m) %>%
#    tidyr::spread(uid, m) %>%
#    dplyr::select(-epoch, -day)
#
#  X11()
#  wide_studsg_NA  %>%
#    vapply(function(x) mean(!is.na(x)), numeric(1)) %>%
#    sort() %>% barplot(); abline(h = 0.5, col = "red")
#
#  print(skimr::skim(studs))
#
#  X11()
#  visdat::vis_miss(wide_studsg_NA)
#}
#
#





#blocks_barplot <- function(studs, ..., breaks_multiplier = 1,
#                           blocks = c("day", "hour"),
#                           xlab = "Hours into study",
#                           main = "Distribution of response times") {
#
#  new_blocks <- blocks[which( !(blocks %in% names(studs)) )]
#
#  if ( length(new_blocks) > 0 )
#    studs <- regularise_time(studs, blocks = new_blocks, add_NAs = FALSE)
#
#
#
#  vals <- tidyr::unite(studs, blocks, blocks, sep = "_", remove = TRUE)$blocks
#
#  barplot(table(vals))
#
#  if ( "timestamp_SL_tbl" %in% class(studs) ) {
#
#    res_t <- studs[["timestamp"]]/3600
#
#  } else if ( "interval_SL_tbl" %in% class(studs) ) {
#
#    res_t <- studs[["start_timestamp"]]/3600
#
#  } else {
#
#    stop("studs not of class timestamp_SL_tbl or interval_SL_tbl")
#
#  }
#
#  minr <- min(res_t); maxr <- max(res_t)
#  br <- seq(0, maxr - minr + 1, by = 1)
#  hist(res_t - minr, ..., breaks = br*breaks_multiplier,
#       xlab = xlab,
#       main = main)
#}
