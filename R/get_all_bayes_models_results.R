#' A wrapper function that returns the best workflow probabilities for each model
#' according to Bayes Sign Test
#'
#' @param res A ComparisonResults object.
#' @param df A data.frame returned from \code{\link{get_avg_scores_by_metrics}}.
#' @param models A character vector with the models that we want to evaluate.
#' @param metric A character with the metric name.
#'
#' @return A data.frame with all the pair-wise comparisons of workflows for each
#' model and respective probabilities.
#' @export
#'
#' @examples
get_all_best_models_bayes <- function(res, df, models, metric = "sera"){

  results <- list()

  models <- sapply(models, FUN = function(i) paste0("wf.", i))

  for(model in models){

    print(paste0("Getting results for model ", model))

    topkperf <- get_top_k_performers(df, m.name = model)
    bayesdf <- get_best_models_bayes(res, workflows = topkperf, metric = metric)

    results[[model]] <- bayesdf
  }

  return(results)
}
