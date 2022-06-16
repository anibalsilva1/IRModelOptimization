#' Given a merged ComparisonResults object, returns a dataframe containing the
#' average results from each workflow.
#'
#' @param res A ComparisonResults object.
#' @param metric A character specifying the metric. Possible values: sera or mse.
#'
#' @return A tibble object with the following columns: dataset, workflow,
#' parameters and average result.
#' @export
#'
#' @examples
get_avg_scores_by_metric <- function(res, metric = "sera"){

  resdata <- tibble::tibble(
    dataset = character(),
    workflow = character(),
    metrics = list(),
    params = list()
  )

  for(d in 1:length(res)) {

    for(w in 1:length(res[[d]])){

      resdata <- resdata %>% add_row(
        dataset = names(res)[d],
        workflow = names(res[[d]])[w],
        metrics = list(res[[d]][[w]]@iterationsScores),
        params = list(res[[d]][[w]]@workflow@pars)
      )
    }
  }

  resdata <- resdata %>% mutate(
    avg = sapply(1:nrow(resdata), FUN = function(i) colMeans(resdata$metrics[i][[1]])[metric]),
  ) %>%
    dplyr::select(-metrics) %>%
    dplyr::mutate(across(where(is.character), as.factor))

  resdata <- resdata %>% dplyr::mutate(workflow = as.factor(workflow))


  return(resdata)
}
