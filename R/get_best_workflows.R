#' Get best workflows to list.
#'
#' @description Transforms a \code{data.frame} with the best workflows found for each model into a list.
#' @param df A \code{data.frame} with the best workflow per dataset.
#' @param nmodels Number of models for each dataset.
#'
#' @return The best workflows for each dataset in a \code{list}.
#' @export
#'
#' @examples
get_best_workflows <- function(df, nmodels){

  b <- list()
  ds <-  df %>% distinct(as.character(dataset)) %>% pull()
  wfs <- pull(df, workflow)
  v <- c()
  n <- 1

  for(i in 1:length(ds)){
    for(j in seq(from=n, to=n+nmodels-1)){
      v <- c(v, wfs[j])
    }
    n = n+nmodels
    b[[ds[i]]] <- v
    v <- c()
  }
  return(b)
}
