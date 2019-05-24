#' response_hour_hist
#'
#' Produce a histogram with frequency of observations
#' in each hour or number of hours
#'
#' @param tab A timestamp_SL_tbl or interval_SL_tbl.
#' @param break_hours Specify the width in hours of each
#' histogram bin.
#' @param xlab Argument passed to \code{\link[graphics]{hist}}.
#' @param main Argument passed to \code{\link[graphics]{hist}}.
#' @param ... Arguments passed to \code{\link[graphics]{hist}}.
#'
#' @export
response_hour_hist <- function(tab, break_hours = 10,
                               xlab = "Hours into study",
                               main = paste0("Distribution of ",
                                             attr(tab,"table"),
                                             " response times"),
                               ...) {

  if ( "timestamp_SL_tbl" %in% class(tab) ) {

    res_t <- tab[["timestamp"]]/3600

  } else if ( "interval_SL_tbl" %in% class(tab) ) {

    res_t <- tab[["start_timestamp"]]/3600

  } else {

    stop("tab not of class timestamp_SL_tbl or interval_SL_tbl")

  }

  minr <- min(res_t); maxr <- max(res_t)
  br <- seq(0, maxr - minr + 1, by = break_hours)
  graphics::hist(res_t - minr, ..., breaks = br,
       xlab = xlab,
       main = main)
}





# # THIS FUNCTION HIGHLIGHTS THAT
# # WE SHOULD INTRODUCE A WAY TO
# # EASILY RESTRICT THE STUDY
# # PERIOD AFTER LOADING DATA
# # AND VISUALISING THE NAs
# # (IT ALSO NEEDS MORE GENERALITY)
# #' vis_NAs_by_student
# #'
# #' This function should be applied after adding
# #' NAs to the imported data with \code{\link{add_NAs}}
# #'
# #'
# #' @export
# vis_NAs_by_student <- function(tab) {
#   `%>%` <- dplyr::`%>%`
#   wide_tabg_NA <- tab %>%
#     dplyr::select(day, epoch, uid, m) %>%
#     tidyr::spread(uid, m) %>%
#     dplyr::select(-day, -epoch)
#   X11()
#   wide_tabg_NA  %>%
#     vapply(function(x) mean(!is.na(x)), numeric(1)) %>%
#     sort() %>% barplot(); abline(h = 0.5, col = "red")
#   print(skimr::skim(tab))
#   X11()
#   visdat::vis_miss(wide_tabg_NA)
# }
















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
#strongest_uids <- function(tab, n = 10) {
#
#  `%>%` <- dplyr::`%>%`
#
#  return((tabg %>% dplyr::group_by(uid) %>%
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
#vis_stud_NAs <- function(tab) {
#
#  `%>%` <- dplyr::`%>%`
#
#  wide_tabg_NA <- tab %>%
#    dplyr::select(epoch, day, uid, m) %>%
#    tidyr::spread(uid, m) %>%
#    dplyr::select(-epoch, -day)
#
#  X11()
#  wide_tabg_NA  %>%
#    vapply(function(x) mean(!is.na(x)), numeric(1)) %>%
#    sort() %>% barplot(); abline(h = 0.5, col = "red")
#
#  print(skimr::skim(tab))
#
#  X11()
#  visdat::vis_miss(wide_tabg_NA)
#}
#
#





#blocks_barplot <- function(tab, ..., breaks_multiplier = 1,
#                           blocks = c("day", "hour"),
#                           xlab = "Hours into study",
#                           main = "Distribution of response times") {
#
#  new_blocks <- blocks[which( !(blocks %in% names(tab)) )]
#
#  if ( length(new_blocks) > 0 )
#    tab <- regularise_time(tab, blocks = new_blocks, add_NAs = FALSE)
#
#
#
#  vals <- tidyr::unite(tab, blocks, blocks, sep = "_", remove = TRUE)$blocks
#
#  barplot(table(vals))
#
#  if ( "timestamp_SL_tbl" %in% class(tab) ) {
#
#    res_t <- tab[["timestamp"]]/3600
#
#  } else if ( "interval_SL_tbl" %in% class(tab) ) {
#
#    res_t <- tab[["start_timestamp"]]/3600
#
#  } else {
#
#    stop("tab not of class timestamp_SL_tbl or interval_SL_tbl")
#
#  }
#
#  minr <- min(res_t); maxr <- max(res_t)
#  br <- seq(0, maxr - minr + 1, by = 1)
#  hist(res_t - minr, ..., breaks = br*breaks_multiplier,
#       xlab = xlab,
#       main = main)
#}
