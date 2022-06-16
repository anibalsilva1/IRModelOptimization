#' Calculates the value for which SERA is minimum.
#'
#' @param trues A vector of true values.
#' @param phi.trues A vector of the relevance function w.r.t. true values.
#' @param ph Control points.
#' @param step The step used in the trapezoidal rule. Defaults to 0.001.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
sera_min <- function(trues, phi.trues = NULL, ph = NULL, step = 0.001){

  if(is.null(phi.trues)) phi.trues <- phi(trues,ph)

  th <- c(seq(0,1,step))

  num = sapply(th, FUN = function(x)  sum(trues[phi.trues >= x]))
  num_areas = sum(sapply(2:length(th), FUN = function(x) step * (num[x-1] + num[x])/2))

  den = sapply(th, FUN = function(x) length(trues[phi.trues >=x]))
  den_areas = sum(sapply(2:length(th), FUN = function(x) step * (den[x-1] + den[x])/2))

  min = num_areas/den_areas

  return(min)

}
