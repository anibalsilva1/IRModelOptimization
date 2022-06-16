#' Top k Performers
#'
#'
#'
#' @description Given a \code{data.frame} with all the workflows for each dataset, returns a character
#' vector with \code{k} workflows that had the lowest error across all datasets.
#'
#' @param res A data.frame returned from \code{\link{get_avg_scores_by_metric}}.
#' @param k A numeric with the number of workflows to return.
#' @param m.name A character with the name of the model.
#'
#' @return A character vector with the top-k workflows.
#' @export
#'
#' @examples
get_top_k_performers <- function(res, k = 3, m.name){

  res <- res %>% tidyr::separate(workflow,
                          into = c("model", "version"),
                          sep = ".v") %>%
    dplyr::filter(model == m.name)

    metrics <- colnames(res %>% dplyr::select(matches("avg")))


    top <- sapply(metrics, FUN = function(metric) res %>% dplyr::group_by(dataset) %>%
                                                   dplyr::slice_min(!!sym(metric), n = k) %>%
                                                   ungroup() %>%
                                                   count(version) %>%
                                                   slice_max(version, n = k) %>%
                                                   dplyr::select(-n))
    top <- unlist(top)
    top <- unique(top)

    res <- sapply(top, FUN = function(i) paste0(m.name, ".v", i))

  return(res)
}
