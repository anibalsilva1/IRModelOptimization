#' Get sigma.
#'
#' @description Calculates the number of instances have a relevance until a given
#' threshold.
#'
#' @param phi A numeric vector with the relevance values of the target variable.
#' @param steps A numeric vector with steps.
#'
#' @return A numeric vector with the number of occurrences of a given observation.
#' @export
#'
#' @examples
get_sigma <- function(phi, steps){

  v <- vector(mode = "numeric", length=length(phi))

  for(i in 1:length(phi)) {
    for(j in 1:length(steps)){
      if(phi[i] >= steps[j]){
        v[i] <- v[i]+1
      }
    }
  }
  return(v)
}
