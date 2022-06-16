#' Merges comparisonResults objects.
#'
#' @param results A list of ComparisonResults objects.
#' @param nDs A character vector with datasets names.
#' @param nWFs A character vector with workflows names.
#'
#' @return A merged ComparisonResults object.
#' @export
#' @import performanceEstimation
#' @import stats
#'
#' @examples
merge_results <- function(results, nDs, nWFs){

  res <- NULL
  for(d in nDs){
    resD <- NULL

    for(l in nWFs){
      load(paste(d, l,"RData", sep = "."))
      x <- get(paste(d, l, "Res", sep = "."))
      resD <- if (is.null(resD)) x else mergeEstimationRes(resD, x, by = "workflows")
    }
    res <- if(is.null(res)) resD else mergeEstimationRes(res, resD, by = "tasks")
  }
  names(res) <- nDs
  return(res)
}
