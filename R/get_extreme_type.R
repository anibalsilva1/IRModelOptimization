#' Get extreme type.
#'
#' @description Returns the type of an extreme depending on the existence of
#' outliers in the adjusted boxplot.
#'
#' @param y numeric \code{vector}.
#'
#' @return
#' @export
#'
#' @examples
get_extreme_type <- function(y){

  stats <- adjboxStats(y)$stats

  low_w <- stats[1]
  upper_w <- stats[5]

  low_l <- length(y[y < low_w])
  upper_l <- length(y[y > upper_w])

  if(low_l != 0 & upper_l != 0){
    type <- "both"
  }else if(upper_l != 0 & low_l == 0){
    type <- "high"
  }else if(low_l != 0 & upper_l == 0){
    type <- "low"
  }
  else{
    type <- "both"
  }

  return(type)
}
