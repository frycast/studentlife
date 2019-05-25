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
  br <- seq(0, maxr - minr + break_hours, by = break_hours)
  graphics::hist(res_t - minr, ..., breaks = br,
       xlab = xlab,
       main = main)
}




#' vis_NAs
#'
#' Produce a visualisation of the number of missing values
#' among each student in a regularised SL_tbl.
#'
#' @param tab A an object of class \code{reg_SL_tbl} as
#' produced by the function \code{\link[studentlife]{regularise_time}}.
#' @param response A character string naming one of the columns
#' in \code{tab} that is not in \code{attr(tab, "blocks")}. If
#' missing then this defaults to the first such column name.
#' @param main The plot title, passed to \code{\link[ggplot2]{ggtitle}}.
#'
#'
#' @return A ggplot object.
#'
#' @export
vis_NAs <- function(tab, response,
                    main = paste0("Missing values by student (",
                                  attr(tab,"table"),")" )) {

  if (!is_reg_SL_tbl(tab)) stop("tab must be a reg_SL_tbl")


  blocks <- attr(tab, "blocks")

  if (missing(response)) {
    nam <- names(tab)
    nam <- nam[which( !(nam %in% blocks) )]
    response <- nam[1]
  }

 `%>%` <- dplyr::`%>%`
 wide_tabg_NA <- tab %>%
   dplyr::select(c(blocks, "uid"), response) %>%
   tidyr::spread("uid", response) %>%
   dplyr::select(-blocks)
 visdat::vis_miss(wide_tabg_NA) + ggplot2::ggtitle(main)
}

#' vis_response_counts
#'
#' Produce an ordered bar plot of the
#' total number of responses
#' for each student in a regularised SL_tbl.
#'
#' @param tab A an object of class \code{reg_SL_tbl} as
#' produced by the function \code{\link[studentlife]{regularise_time}}.
#' @param response A character string naming one of the columns
#' in \code{tab} that is not in \code{attr(tab, "blocks")}. If
#' missing then this defaults to the first such column name.
#' @param main The plot title, passed to \code{\link[graphics]{barplot}}.
#' @param ... Arguments passed to \code{\link[graphics]{barplot}}.
#'
#' @export
vis_response_counts <- function(tab, response,
                                main = paste0("Total responses by student (",
                                              attr(tab,"table"),")" ),...) {
  if (!is_reg_SL_tbl(tab)) stop("tab must be a reg_SL_tbl")

  blocks <- attr(tab, "blocks")

  if (missing(response)) {
    nam <- names(tab)
    nam <- nam[which( !(nam %in% blocks) )]
    response <- nam[1]
  }

  `%>%` <- dplyr::`%>%`
  wide_tabg_NA <- tab %>%
    dplyr::select(c(blocks, "uid"), response) %>%
    tidyr::spread("uid", response) %>%
    dplyr::select(-blocks)
  wide_tabg_NA  %>%
    vapply(function(x) sum(!is.na(x)), numeric(1)) %>%
    sort() %>% graphics::barplot(main = main)
}

#vis_NAs_by_student <- function(tab) {
#
#  if (!is_reg_SL_tbl(tab)) stop("tab must be a reg_SL_tbl")
#
#  blocks <- attr(tab, "blocks")
#
#  `%>%` <- dplyr::`%>%`
#  wide_tabg_NA <- tab %>%
#    dplyr::select(day, epoch, uid, m) %>%
#    tidyr::spread(uid, m) %>%
#    dplyr::select(-day, -epoch)
#  X11()
#  wide_tabg_NA  %>%
#    vapply(function(x) mean(!is.na(x)), numeric(1)) %>%
#    sort() %>% barplot(); abline(h = 0.5, col = "red")
#  print(skimr::skim(tab))
#  X11()
#  visdat::vis_miss(wide_tabg_NA)
#}
















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
