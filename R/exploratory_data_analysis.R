#### INCOMPLETE
## ISSUES
# Needs documentation
#
##
#' strongest_uids
#'
#' Returns the uids of the n students in the
#' data.frame that have the most responses
#'
#'@export
strongest_uids <- function(studs, n = 10) {

  `%>%` <- dplyr::`%>%`

  return((studsg %>% dplyr::group_by(uid) %>%
           summarise(nr = dplyr::n()) %>%
           dplyr::top_n(n, nr))$uid)
}




### INCOMPLETE
## ISSUES
# Needs documentation and generality
#
##
#' vis_stud_NAs
#'
#' This function should be applied after adding
#' NAs to the imported data with \code{\link{add_NAs}}
#'
#'
#' @export
vis_stud_NAs <- function(studs) {

  `%>%` <- dplyr::`%>%`

  wide_studsg_NA <- studs %>%
    dplyr::select(epoch, day, uid, m) %>%
    tidyr::spread(uid, m) %>%
    dplyr::select(-epoch, -day)

  X11()
  wide_studsg_NA  %>%
    vapply(function(x) mean(!is.na(x)), numeric(1)) %>%
    sort() %>% barplot(); abline(h = 0.5, col = "red")

  print(skimr::skim(studs))

  X11()
  visdat::vis_miss(wide_studsg_NA)
}


### INCOMPLETE
## ISSUES
#
# Needs documentation
#
###
#' respose_hour_hist
#'
#'
#'
#' @export
response_hour_hist <- function(studs, timestamp = "resp_time") {

  res_t <- studs[[timestamp]]/3600
  minr <- min(res_t); maxr <- max(res_t)
  br <- seq(0, maxr - minr + 1, by = 1)
  hist(res_t - minr, breaks = br, xlab = "Hours into study",
       main = "Distribution of response times")
}
