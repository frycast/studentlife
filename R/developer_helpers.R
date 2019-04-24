`transfer_EMA_attrs<-` <- function(to, ..., value) {

  attr(to, "EMA_name") <- attr(value, "EMA_name")
  attr(to, "EMA_questions") <- attr(value, "EMA_questions")

  return(to)
}
