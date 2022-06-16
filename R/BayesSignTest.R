#' Calculates the posterior probabilities of a model A (left) being practically better then a model B (right).
#'
#' @param diffVector A numeric vector with error differences.
#' @param rope_min A numeric value with the minimum of ROPE.
#' @param rope_max A numeric value with the maximum of ROPE.
#'
#' @return A list with the posterior probabilities.
#' @export
#'
#' @examples

BayesianSignTest <- function(diffVector,rope_min,rope_max) {

  library(MCMCpack)

  samples <- 3000

  diffVector <- c(0,diffVector)

  #for the moment we implement the sign test.
  nLeft <- sum(diffVector < rope_min)
  nRight <- sum(diffVector > rope_max)
  nRope <- length(diffVector) - nLeft - nRight


  alpha <- c(nLeft,nRope,nRight)/length(diffVector)

  alpha <- alpha+0.0001

  alpha.res <- colMeans(MCMCpack::rdirichlet(30000, alpha))

  results = list ("probLeft"=alpha.res[1], "probRope"=alpha.res[2],
                  "probRight"=alpha.res[3])

  return (results)

}
