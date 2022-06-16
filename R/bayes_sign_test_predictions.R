#' Bayes Sign Test Predictions
#'
#' @description Performs Bayes Sign Test on predictions.
#' @param scores A \code{data.frame} with the predictions from each model for all datasets.
#' @param oracle A character with the model which we want to compare the others.
#' @param rope rope Region of Practical Equivalence. Defines a region where the oracle and the other model is practical equivalent. Defaults to 0.01.
#'
#' @return Probabilities returned form the Bayes Sign test
#' @export
#'
#' @examples
bayesres_preds <- function(scores, oracle, rope=0.01){

  diffs <- scores %>%
    dplyr::select(-dataset) %>%
    dplyr::mutate(across(matches(oracle), .names="oracle")) %>%
    dplyr::mutate(across(everything(), ~ (oracle - .x)/.x)) %>%
    dplyr::select(-matches(oracle), -oracle)

  res_bayes <- tibble(
    model=character(),
    oracle=character(),
    modelProb=numeric(),
    oracleProb=numeric(),
    rope=numeric()
  )
  models <- colnames(diffs)

  for(model in models){

    dv <- diffs[, model] %>% pull()

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
      rope = sprintf("%0.3f", rope))

  return(res_bayes)

}
