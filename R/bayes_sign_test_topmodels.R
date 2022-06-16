#' Bayes Sign Test Top Models
#'
#'
#' @description Performs Bayes Sign Test for the best performers for a given dataset.
#' @param betswfs A \code{list} with the best workflows for a given dataset.
#' @param oracle The model which you want to compare the others.
#' @param metric Metric to evaluate the test. It must be one used in performanceEstimation.
#' @param rope Region of Practical Equivalence. Defines a region where the oracle and the other model is practical equivalent. Defaults to 0.01.
#' @param return.errs If true, in addition to the probabilities returned form the Bayes Sign test it also returns the mean differences between the model and the oracle.
#'
#' @return
#' @export
#'
#' @examples
bayes_sign_test_topmodels <- function(betswfs,
                                      oracle,
                                      metric,
                                      rope=0.01,
                                      return.errs=FALSE){


  res_df <- list()

  for(i in 1:length(betswfs)){

    ds_name <- names(betswfs)[i]

    res_wfs <- matrix(nrow=10, ncol=4, dimnames = list(NULL, betswfs[[i]]))

    for(j in 1:length(betswfs[[i]])){

      wf_name <- betswfs[[i]][j]

      res_wfs[, wf_name] <- res[[ds_name]][[wf_name]]@iterationsScores[, metric]
    }

    res_wfs_df <- as.data.frame(res_wfs)

    res_wfs_df <- res_wfs_df %>%
      mutate(across(matches(oracle), .names="oracle")) %>%
      mutate(across(everything(), ~ (oracle - .x)/.x)) %>%
      dplyr::select(-matches(oracle), -oracle) %>%
      summarise(across(everything(), ~ mean(.x))) %>%
      #mutate(across(everything(), ~ .x/sum(.x))) %>% # just to test
      rename_with(.cols=everything(), .fn= ~ str_remove(.x ,"\\.v\\d+"))

    res_df[[ds_name]] <- res_wfs_df
  }
  res_dfs <- do.call(rbind, res_df)

  res_bayes <- tibble(
    model=character(),
    oracle=character(),
    modelProb=numeric(),
    oracleProb=numeric(),
    rope=numeric()
  )

  models <- colnames(res_dfs)
  for(model in models){

    dv <- res_dfs[, model]


    bsr <- BayesianSignTest(dv, rope_min = -rope, rope_max = rope)
    res_bayes <- res_bayes %>% dplyr::add_row(model = model,
                                              oracle = oracle,
                                              modelProb = bsr$probRight,
                                              oracleProb = bsr$probLeft,
                                              rope = bsr$probRope)
  }
  res_bayes <- res_bayes %>%
    dplyr::mutate(
      modelProb = sprintf("%0.3f", modelProb),
      oracleProb = sprintf("%0.3f", oracleProb),
      rope = sprintf("%0.3f", rope),
      metric = metric)
  if(isTRUE(return.errs))
    return(list("results_errs" = res_dfs,
                "results_bayes" = res_bayes))
  else
    return(res_bayes)
}
